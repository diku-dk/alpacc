#!/usr/bin/env bash
#SBATCH --job-name=alpacc-sweep
#SBATCH --output=alpacc-sweep-%j.out
#SBATCH --error=alpacc-sweep-%j.err
#SBATCH --gres=gpu:a100:1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=03:00:00
# Adjust to your cluster:
##SBATCH --partition=gpu
##SBATCH --account=<account>

# SLURM wrapper for benchmarks/sweep-cuda-lexer.sh.
#
# Sweeps (state_t, index_t) × (BLOCK_SIZE, ITEMS_PER_THREAD) for a given
# grammar on the compute node's GPU and reports the fastest configuration
# per type combo.  Used to populate the per-arch tuning table in
# backends/cuda/lexer.cu.
#
# Submit from anywhere inside the repo, e.g.:
#   sbatch sweep-cuda-lexer.slurm             (from benchmarks/)
#   sbatch benchmarks/sweep-cuda-lexer.slurm  (from the repo root)
#
# Environment (all optional):
#   GRAMMAR      path to the .alp file to sweep (default: grammars/json.alp)
#   INPUT_SIZE   dataset size in tokens (default: 10485760, ~20 MiB payload)
#   BS_LIST      block sizes to try     (default: "128 256")
#   IPT_LIST     items-per-thread values (default: "32 64 96")
#   STATE_LIST   state_t typedefs       (default: "uint16_t uint32_t")
#   INDEX_LIST   index_t typedefs       (default: "int32_t int64_t")
#   BENCH_RUNS   --benchmark N per config (default: 5)
#   BENCH_WARMUP --warmup N              (default: 2)
#
# Requirements on the node:
#   - nvcc (loaded from cuda/12.x module, or on PATH)
#   - alpacc on PATH (build once on the login node:
#       cabal install exe:alpacc --installdir=$HOME/.local/bin --overwrite-policy=always)

set -u

# --- Environment (adjust module names to your cluster) ---
module load cuda 2>/dev/null || true
export PATH="$HOME/.local/bin:$PATH"   # wherever alpacc was installed

echo "=== Job ${SLURM_JOB_ID:-local} on $(hostname) ==="
nvidia-smi --query-gpu=name,compute_cap,driver_version --format=csv
command -v alpacc || { echo "FATAL: alpacc not on PATH"; exit 1; }
command -v nvcc   || { echo "FATAL: nvcc not on PATH";   exit 1; }

# --- Resolve repo root and grammar path ---
SUBMIT_DIR="${SLURM_SUBMIT_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)}"
cd "$SUBMIT_DIR" || exit 1
REPO_ROOT=$(git rev-parse --show-toplevel 2>/dev/null)
if [ -z "$REPO_ROOT" ]; then
    echo "FATAL: $SUBMIT_DIR is not inside a git checkout of alpacc"
    exit 1
fi
cd "$REPO_ROOT" || exit 1

GRAMMAR="${GRAMMAR:-grammars/json.alp}"
if [ ! -f "$GRAMMAR" ]; then
    # Try treating it as absolute or already resolved.
    if [ ! -f "$(realpath -m "$GRAMMAR")" ]; then
        echo "FATAL: grammar file $GRAMMAR not found"
        exit 1
    fi
fi

INPUT_SIZE="${INPUT_SIZE:-10485760}"
BS_LIST="${BS_LIST:-128 256}"
IPT_LIST="${IPT_LIST:-32 64 96}"
STATE_LIST="${STATE_LIST:-uint16_t uint32_t}"
INDEX_LIST="${INDEX_LIST:-int32_t int64_t}"
export BENCH_RUNS="${BENCH_RUNS:-5}"
export BENCH_WARMUP="${BENCH_WARMUP:-2}"
export INPUT_SIZE

# --- Ensure the dataset exists (Makefile-driven when the benchmark dir has one) ---
GRAMMAR_NAME=$(basename "$GRAMMAR" .alp)
BENCH_DIR="benchmarks/$GRAMMAR_NAME"
DATASET=""
if [ -d "$BENCH_DIR" ] && [ -f "$BENCH_DIR/Makefile" ]; then
    DATASET="$BENCH_DIR/data-${INPUT_SIZE}.inputs"
    if [ ! -f "$DATASET" ]; then
        echo "=== Generating dataset $DATASET ==="
        make -C "$BENCH_DIR" "data-${INPUT_SIZE}.inputs" INPUT_SIZE="$INPUT_SIZE"
    fi
fi

# --- Run the sweep ---
echo "=== Starting sweep for $GRAMMAR ==="
echo "    BS_LIST    = $BS_LIST"
echo "    IPT_LIST   = $IPT_LIST"
echo "    STATE_LIST = $STATE_LIST"
echo "    INDEX_LIST = $INDEX_LIST"

bash benchmarks/sweep-cuda-lexer.sh "$GRAMMAR" "$DATASET" \
     "$BS_LIST" "$IPT_LIST" "$STATE_LIST" "$INDEX_LIST"

# --- Bundle artifacts for retrieval ---
LATEST_ARTIFACTS=$(ls -1dt benchmarks/results/sweep-${GRAMMAR_NAME}-* 2>/dev/null | head -1)
if [ -n "$LATEST_ARTIFACTS" ]; then
    BUNDLE="$SUBMIT_DIR/sweep-${GRAMMAR_NAME}-${SLURM_JOB_ID:-local}.tar.gz"
    tar czf "$BUNDLE" -C "$(dirname "$LATEST_ARTIFACTS")" "$(basename "$LATEST_ARTIFACTS")"
    echo "=== Bundle: $BUNDLE ==="
fi

echo "=== Done ==="
