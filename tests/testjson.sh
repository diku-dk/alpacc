#!/bin/bash

# Test script for the JSON grammar with a configurable Futhark backend.
# Runs lexer, parser, and combined (lexer+parser) tests using a single
# parseable input of length 10000.

# Function to show usage
show_usage() {
    echo "Usage: $0 [backend]"
    echo "  backend: Futhark backend to use (default: c)"
    echo "           Options: c, multicore, opencl, cuda, ispc"
    echo "Example: $0 multicore"
}

# Check for help flag
if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
    show_usage
    exit 0
fi

backend="${1:-c}"
length=10

echo "Testing JSON grammar with backend '$backend' and length $length..."

# Resolve the repo root from the script's own location
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
GRAMMAR="$REPO_ROOT/grammars/json.alp"

# Create a temporary directory for this run
temp_dir=$(mktemp -d)
trap "rm -rf \"$temp_dir\"" EXIT

# Set up Futhark packages once
echo "Setting up Futhark packages..."
cd "$temp_dir"
futhark pkg add github.com/diku-dk/containers
futhark pkg add github.com/diku-dk/sorts
futhark pkg sync
if [ $? -ne 0 ]; then
    echo "Failed to set up Futhark packages"
    exit 1
fi
echo "Futhark packages ready"

# Run tests for a specific mode.
# Arguments:
#   $1 – mode flag passed to alpacc (e.g. "--lexer", "--parser", or "" for both)
run_json_test() {
    local mode_flag="$1"

    local mode_name
    if [ -z "$mode_flag" ]; then
        mode_name="combined"
    else
        mode_name="${mode_flag#--}"
    fi

    echo "========================================="
    echo "Testing JSON $mode_name mode (backend=$backend)..."
    echo "========================================="

    local work_dir="$temp_dir/$mode_name"
    mkdir -p "$work_dir"
    cd "$work_dir"

    # Copy Futhark package files into the work directory
    cp -r "$temp_dir/lib" .
    cp "$temp_dir/futhark.pkg" .

    # Generate Futhark source for the JSON grammar
    # shellcheck disable=SC2086
    if ! alpacc futhark "$GRAMMAR" $mode_flag; then
        echo "ERROR: alpacc futhark failed for $mode_name mode"
        return 1
    fi

    # Generate the test input/output files
    # shellcheck disable=SC2086
    if ! alpacc test generate "$GRAMMAR" --single-long --parseable --length $length $mode_flag; then
        echo "ERROR: alpacc test generate failed for $mode_name mode"
        return 1
    fi

    # Run the Futhark script with the selected backend.
    # `futhark script -b` produces binary output with a 16-byte header that
    # encodes the result type; strip it before passing to `alpacc test compare`.
    futhark script --backend="$backend" -b json.fut 'test ($loadbytes "json.inputs")' \
        | tail -c +16 > json.results

    # Compare expected vs actual results
    # shellcheck disable=SC2086
    if ! alpacc test compare "$GRAMMAR" json.inputs json.outputs json.results $mode_flag; then
        echo "ERROR: Test FAILED for JSON $mode_name mode"
        return 1
    fi

    echo "JSON $mode_name test PASSED"
    return 0
}

run_json_test "--lexer"  || exit 1
run_json_test "--parser" || exit 1
run_json_test ""         || exit 1

echo "All JSON grammar tests passed."
