#!/bin/bash

# Test script for the PSE primitive (backends/cuda/pse.cu).  Emulates the CUDA code
# generator's concatenation (common.cu + scan.cu + pse.cu), appends the test
# main, compiles with nvcc, and runs a GPU-vs-CPU differential test of both
# PSE semantics (strict < and <=).  Requires an NVIDIA GPU with cooperative
# launch support (SM 6.0+); intended for local runs, not hosted CI.

# Function to show usage
show_usage() {
    echo "Usage: $0 [arch]"
    echo "  arch: nvcc -arch value (default: native)"
    echo "Example: $0 sm_75"
}

# Check for help flag
if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
    show_usage
    exit 0
fi

arch="${1:-native}"

echo "Testing PSE primitive with -arch=$arch..."

# Resolve the repo root from the script's own location
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"

# Create a temporary directory for this run
temp_dir=$(mktemp -d)
trap "rm -rf \"$temp_dir\"" EXIT

# Concatenate in generator order, then the test main
cat "$REPO_ROOT/backends/cuda/common.cu" \
    "$REPO_ROOT/backends/cuda/scan.cu" \
    "$REPO_ROOT/backends/cuda/pse.cu" \
    "$REPO_ROOT/tests/pse_test_main.cu" > "$temp_dir/pse_test.cu"

if ! nvcc -std=c++17 -arch="$arch" -Xcompiler -O2 \
        -o "$temp_dir/pse_test" "$temp_dir/pse_test.cu"; then
    echo "ERROR: nvcc compilation failed"
    exit 1
fi

if ! "$temp_dir/pse_test"; then
    echo "ERROR: PSE test FAILED"
    exit 1
fi

echo "All PSE tests passed."
