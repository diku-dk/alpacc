#!/bin/bash
# test-long-input.sh — Run lexer, parser, and combined modes on a single
# long input for an arbitrary grammar and backend.
#
# Usage:
#   test-long-input.sh <grammar.alp> [backend] [length]
#
# backend (default: c):
#   c                    — alpacc c backend, compiled with cc
#   cuda                 — alpacc cuda backend, compiled with nvcc (requires GPU)
#   futhark-<target>     — alpacc futhark backend, run via futhark eval
#                          <target> is the Futhark execution target, e.g.:
#                          futhark-multicore, futhark-opencl, futhark-ispc, futhark-cuda
#
# length (default: 100000)
#
# Exit codes: 0 = all tests passed, 1 = at least one failed.
#
# Adding a new backend:
#   1. Write a function  setup_<backend>   (optional; called once before tests)
#   2. Write a function  run_backend_<backend> <mode_flag> <stem>
#      - generates <stem>.results from <stem>.inputs
#      - returns 0 on success, non-zero on failure
#   3. Add a case for the backend name in the dispatch section below.

set -euo pipefail

show_usage() {
    echo "Usage: $0 <grammar.alp> [backend] [length] [--index32]"
    echo "  grammar.alp  path to the .alp grammar file"
    echo "  backend      c (default), cuda, or any futhark backend"
    echo "  length       input length (default: 100000)"
    echo "  --index32    generate + verify with 32-bit index_t (default: 64-bit)"
}

if [ "${1:-}" = "-h" ] || [ "${1:-}" = "--help" ]; then
    show_usage; exit 0
fi
if [ $# -lt 1 ]; then
    echo "error: grammar file required" >&2; show_usage; exit 1
fi


GRAMMAR="$(realpath "$1")"
backend="${2:-c}"
length="${3:-100000}"
index_flag="${4:-}"
if [ -n "$index_flag" ] && [ "$index_flag" != "--index32" ]; then
    echo "error: unrecognised argument '$index_flag' (expected --index32 or omitted)" >&2
    show_usage; exit 1
fi
grammar_name="$(basename "$GRAMMAR" .alp)"

echo "Testing '$grammar_name' grammar with backend '$backend' (length=$length${index_flag:+, $index_flag})..."

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
temp_dir=$(mktemp -d)
trap 'rm -rf "$temp_dir"' EXIT

# ---------------------------------------------------------------------------
# Backend: c
# ---------------------------------------------------------------------------

setup_c() { :; }

run_backend_c() {
    local mode_flag="$1" stem="$2"
    # shellcheck disable=SC2086
    if ! alpacc c "$GRAMMAR" $mode_flag $index_flag -o "${stem}.c"; then
        echo "ERROR: alpacc c failed"; return 1
    fi
    if ! cc -std=c99 -O2 ${CFLAGS:-} -o "${stem}" "${stem}.c" &>/dev/null; then
        echo "ERROR: cc compilation failed"; return 1
    fi
    "./${stem}" < "${stem}.inputs" > "${stem}.results"
}

# ---------------------------------------------------------------------------
# Backend: cuda
# ---------------------------------------------------------------------------

setup_cuda() { :; }

run_backend_cuda() {
    local mode_flag="$1" stem="$2"
    local arch="${CUDA_ARCH:-native}"
    # shellcheck disable=SC2086
    if ! alpacc cuda "$GRAMMAR" $mode_flag $index_flag -o "${stem}.cu"; then
        echo "ERROR: alpacc cuda failed"; return 1
    fi
    if ! nvcc -O3 -std=c++17 -arch="$arch" -o "${stem}" "${stem}.cu" &>/dev/null; then
        echo "ERROR: nvcc compilation failed"; return 1
    fi
    "./${stem}" < "${stem}.inputs" > "${stem}.results"
}

# ---------------------------------------------------------------------------
# Backend: futhark-<target>  (e.g. futhark-multicore, futhark-opencl, ...)
# ---------------------------------------------------------------------------

# futhark_pkg_dir: where the shared futhark.pkg + lib/ live (set by setup_futhark)
futhark_pkg_dir=""
futhark_target=""   # Futhark execution target, stripped from the backend name

setup_futhark() {
    futhark_pkg_dir="$temp_dir/futhark-pkg"
    mkdir -p "$futhark_pkg_dir"
    echo "Setting up Futhark packages..."
    ( cd "$futhark_pkg_dir"
      futhark pkg add github.com/diku-dk/containers
      futhark pkg add github.com/diku-dk/sorts
      futhark pkg sync
    )
    echo "Futhark packages ready"
}

run_backend_futhark() {
    local mode_flag="$1" stem="$2"
    cp -r "$futhark_pkg_dir/lib" .
    cp "$futhark_pkg_dir/futhark.pkg" .
    # shellcheck disable=SC2086
    if ! alpacc futhark "$GRAMMAR" $mode_flag $index_flag; then
        echo "ERROR: alpacc futhark failed"; return 1
    fi
    # Lexer and combined modes: a tuning file with chunk_size=10000 forces
    # multi-chunk lexing on the long input, exercising the chunked code path
    # as a regression test.  (A parser-only program has no chunk_size and
    # rejects the file.)
    local tuning_opt=()
    if [ "$mode_flag" != "--parser" ]; then
        echo "chunk_size=10000" > "${stem}.fut.tuning"
        tuning_opt=(--pass-option="--tuning=${stem}.fut.tuning")
    fi
    # The entry point runs compiled (--backend); only the two io calls are
    # interpreted.  io.savevalue writes the binary data format: strip its
    # 15-byte header (b, version, rank, 4-byte type, i64 length).
    futhark eval -w -f "${stem}.fut" --backend="$futhark_target" "${tuning_opt[@]}" \
        "io.savevalue \"${stem}.results.bin\" (test (io.loadbytes \"${stem}.inputs\"))" \
        > /dev/null
    tail -c +16 "${stem}.results.bin" > "${stem}.results"
}

# ---------------------------------------------------------------------------
# Dispatch: map backend name -> setup_* and run_backend_* functions
# ---------------------------------------------------------------------------

case "$backend" in
    c)
        setup_fn=setup_c
        run_fn=run_backend_c
        ;;
    cuda)
        setup_fn=setup_cuda
        run_fn=run_backend_cuda
        ;;
    futhark-*)
        futhark_target="${backend#futhark-}"
        setup_fn=setup_futhark
        run_fn=run_backend_futhark
        ;;
    *)
        echo "error: unknown backend '$backend'" >&2
        echo "       use c, cuda, or futhark-<target> (e.g. futhark-multicore)" >&2
        exit 1
        ;;
esac

# ---------------------------------------------------------------------------
# Per-mode test runner
# ---------------------------------------------------------------------------

run_test() {
    local mode_flag="$1"
    local mode_name
    if [ -z "$mode_flag" ]; then mode_name="combined"; else mode_name="${mode_flag#--}"; fi

    echo "========================================="
    echo "Testing $grammar_name $mode_name mode (backend=$backend)..."
    echo "========================================="

    local work_dir="$temp_dir/$mode_name"
    mkdir -p "$work_dir"
    cd "$work_dir"

    # shellcheck disable=SC2086
    if ! alpacc test generate "$GRAMMAR" --single-long --length "$length" $mode_flag $index_flag; then
        echo "ERROR: alpacc test generate failed for $mode_name mode"; return 1
    fi

    if ! "$run_fn" "$mode_flag" "$grammar_name"; then
        return 1
    fi

    # shellcheck disable=SC2086
    if ! alpacc test compare "$GRAMMAR" \
            "${grammar_name}.inputs" "${grammar_name}.outputs" "${grammar_name}.results" \
            $mode_flag $index_flag; then
        echo "ERROR: test FAILED for $grammar_name $mode_name mode"; return 1
    fi

    echo "$grammar_name $mode_name test PASSED"
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

"$setup_fn"

run_test "--lexer"  || exit 1
run_test "--parser" || exit 1
run_test ""         || exit 1

echo "All $grammar_name tests passed (backend=$backend)."
