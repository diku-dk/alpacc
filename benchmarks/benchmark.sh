#!/bin/bash
#SBATCH -p gpu
#SBATCH --gres=gpu:a100
#SBATCH --mem=32G
#SBATCH --time=3:00:00
#SBATCH --output=benchmark-%j.log

INPUT_SIZE=${INPUT_SIZE:-52428800}

set -e

# Change to the benchmarks/ directory so `make` finds the Makefile.
# When run via sbatch, SLURM_SUBMIT_DIR is the directory sbatch was called from.
# When run directly as a shell script, fall back to the script's own directory.
if [ -n "${SLURM_SUBMIT_DIR:-}" ]; then
    cd "$SLURM_SUBMIT_DIR"
else
    cd "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
fi

# --- Environment setup -----------------------------------------------
# On a cluster with environment modules, load the right toolchain.
# On a local machine without `module`, just use whatever's on PATH
# (make sure cuda, futhark, gmp, gcc-11 are installed/available there).
if command -v module >/dev/null 2>&1; then
    module unload cuda    2>/dev/null || true
    module load   cuda/12.8
    module load   gmp
    module unload gcc      2>/dev/null || true
    module load   gcc/11.2.0
else
    echo "No 'module' command found — assuming cuda/futhark/gmp/gcc are already on PATH."
fi

# --- Sanity checks ---------------------------------------------------
for tool in nvcc futhark gcc; do
    if ! command -v "$tool" >/dev/null 2>&1; then
        echo "Warning: '$tool' not found on PATH." >&2
    fi
done

# --- Benchmark -------------------------------------------------------

make --no-print-directory build         FUTHARK_BACKEND=cuda INPUT_SIZE=$INPUT_SIZE
make --no-print-directory bench-cuda    INPUT_SIZE=$INPUT_SIZE
make --no-print-directory bench-futhark FUTHARK_BACKEND=cuda INPUT_SIZE=$INPUT_SIZE

echo "Benchmarking finished."
