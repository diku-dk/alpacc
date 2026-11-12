#!/usr/bin/env bash
#SBATCH --job-name=alpacc-profile
#SBATCH --output=alpacc-profile-%j.out
#SBATCH --error=alpacc-profile-%j.err
#SBATCH --gres=gpu:a100:1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=02:00:00
# Adjust to your cluster:
##SBATCH --partition=gpu
##SBATCH --account=<account>

# SLURM wrapper for benchmarks/profile-gpu.sh.
# Submit from anywhere inside the repo, e.g.:
#   sbatch profile-gpu.slurm            (from benchmarks/)
#   sbatch benchmarks/profile-gpu.slurm (from the repo root)
#
# Requirements on the node:
#   - nvcc + ncu (module load below; nsys optional, nvprof N/A on A100)
#   - alpacc on PATH (build once on the login node:
#       cabal install exe:alpacc --installdir=$HOME/.local/bin --overwrite-policy=always)
#   - ncu needs GPU performance-counter access. On many clusters this is
#     enabled for exclusive-GPU jobs; if every ncu section fails with
#     ERR_NVGPUCTRPERM, ask the admins to allow counter access
#     (NVreg_RestrictProfilingToAdminUsers=0) or request an exclusive node.

set -u

# --- Environment (adjust module names to your cluster) ---
module load cuda 2>/dev/null || true
# module load nsight-compute 2>/dev/null || true
export PATH="$HOME/.local/bin:$PATH"   # wherever alpacc was installed

echo "=== Job $SLURM_JOB_ID on $(hostname) ==="
nvidia-smi --query-gpu=name,compute_cap,driver_version --format=csv
command -v alpacc || { echo "FATAL: alpacc not on PATH"; exit 1; }
command -v ncu    || { echo "FATAL: ncu not on PATH";    exit 1; }

# --- Run the profiling suite ---
# Resolve the repo root regardless of submission directory or direct invocation.
SUBMIT_DIR="${SLURM_SUBMIT_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)}"
cd "$SUBMIT_DIR" || exit 1
REPO_ROOT=$(git rev-parse --show-toplevel 2>/dev/null)
if [ -z "$REPO_ROOT" ]; then
    echo "FATAL: $SUBMIT_DIR is not inside a git checkout of alpacc"
    exit 1
fi
cd "$REPO_ROOT" || exit 1

# Default dataset size (~100 MiB payload). Generation is CPU-side and
# single-shot; it can take a few minutes.
OUT="$SUBMIT_DIR/profile-results-a100-${SLURM_JOB_ID:-local}"
bash benchmarks/profile-gpu.sh "$OUT"

echo "=== Done. Send back: ${OUT}.tar.gz ==="
