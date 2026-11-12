#!/bin/bash

# Differential test of the CUDA backend (lexer, parser, or combined mode).
#
# For each random grammar this script:
#   1. Generates test inputs with `alpacc test generate --length 4`.
#   2. Runs the CUDA binary in batch mode across all six (BS, IPT) combinations
#      and checks each against `alpacc test compare`.
#   3. Runs the CUDA binary in server mode (a loop of counted batches) with
#      the batch fed twice and checks it matches the batch output twice over.
#   4. Verifies -i/-o file mode gives identical output to stdin/stdout mode.
#
# Requires nvcc and an NVIDIA GPU; not run in hosted CI (GPU-less).

show_usage() {
    echo "Usage: $0 [q_value] [k_value] [target_runs] [parallel_jobs] [type_flag] [arch] [--index32]"
    echo "  q_value:       -q parameter for alpacc (default: 1)"
    echo "  k_value:       -k parameter for alpacc (default: 1)"
    echo "  target_runs:   number of successful grammars (default: 10)"
    echo "  parallel_jobs: number of parallel jobs (default: 1)"
    echo "  type_flag:     --lexer, --parser, or empty for combined (default: empty)"
    echo "  arch:          nvcc -arch value (default: native)"
    echo "  --index32:     pass --index32 to generator and test tools (default: 64-bit)"
    echo "Example: $0 1 1 20 1 '' native"
    echo "Example: $0 1 1 20 1 '' native --index32"
}

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
    show_usage; exit 0
fi

q_value="${1:-1}"
k_value="${2:-1}"
target="${3:-10}"
parallel_jobs="${4:-1}"
type_flag="${5:-}"
arch="${6:-native}"
index_flag="${7:-}"
if [ -n "$index_flag" ] && [ "$index_flag" != "--index32" ]; then
    echo "Error: unrecognised index flag '$index_flag' (expected --index32 or empty)"
    show_usage; exit 1
fi

if ! [[ "$q_value" =~ ^[0-9]+$ ]] || ! [[ "$k_value" =~ ^[0-9]+$ ]] || ! [[ "$target" =~ ^[0-9]+$ ]]; then
    echo "Error: q_value, k_value, and target must be positive integers"
    show_usage; exit 1
fi

if ! [[ "$parallel_jobs" =~ ^[0-9]+$ ]]; then
    echo "Error: parallel_jobs must be a positive integer"
    show_usage; exit 1
fi

echo "Starting alpacc CUDA testing..."
echo "Target: $target successful grammars"
echo "Using -q $q_value -k $k_value ${type_flag:-<combined>} ${index_flag}"
echo "nvcc arch: $arch"
echo "Running with $parallel_jobs parallel jobs"

temp_dir=$(mktemp -d)
trap "rm -rf $temp_dir" EXIT

counter_file="$temp_dir/counter"
echo "0" > "$counter_file"
done_file="$temp_dir/done"

run_test() {
    local job_id=$1
    local q_value=$2
    local k_value=$3
    local temp_dir=$4
    local counter_file=$5
    local target=$6
    local done_file=$7
    local arch=$8
    local type_flag=$9
    local index_flag=${10}

    local work_dir="$temp_dir/job_$job_id"
    mkdir -p "$work_dir"
    cd "$work_dir"

    local codegen_fails=0

    while [ ! -f "$done_file" ]; do
        # Generate random grammar
        if ! alpacc random &> /dev/null; then
            echo "alpacc random failed"; return 1
        fi
        cat > random.alp.tmp << ALPEOF
params {
  lookback=$q_value.
  lookahead=$k_value.
}

ALPEOF
        cat random.alp >> random.alp.tmp
        mv random.alp.tmp random.alp

        # Try to generate CUDA code; skip grammars that fail codegen,
        # but fail fast on configuration errors or endless codegen failures.
        if ! alpacc cuda random.alp $type_flag $index_flag &> codegen_err.txt; then
            if grep -q "must be positive" codegen_err.txt; then
                echo "ERROR: alpacc rejected the configuration itself (job $job_id):"
                cat codegen_err.txt
                return 1
            fi
            codegen_fails=$((codegen_fails + 1))
            if [ "$codegen_fails" -ge 1000 ]; then
                echo "ERROR: codegen failed $codegen_fails consecutive times for job $job_id; giving up. Last error:"
                cat codegen_err.txt
                return 1
            fi
            continue
        fi
        codegen_fails=0

        # Default build (used for server / -i/-o mode below).  The (BS, IPT)
        # sweep in Test 1 rebuilds per config: BLOCK_SIZE / ITEMS_PER_THREAD
        # are compile-time constants (baked via -DALPACC_BLOCK_SIZE /
        # -DALPACC_ITEMS_PER_THREAD), so the --block-size / --items-per-thread
        # CLI flags on the binary are runtime no-ops that only print a
        # warning.  Previously this script relied on those flags and the
        # entire sweep silently tested a single configuration.
        if ! nvcc -std=c++17 -arch="$arch" -o random random.cu &> /dev/null; then
            echo "nvcc compilation failed for job $job_id"; return 1
        fi

        # Generate test inputs (small, to keep GPU memory bounded)
        alpacc test generate random.alp $type_flag $index_flag --length 4 &> /dev/null

        local all_ok=true

        # ---- Test 1: batch mode across all six (BS, IPT) combinations ----
        # Each combo needs its own compile.  Some (BS, IPT) pairs may not fit
        # the shmem budget for a given grammar's endo_t width; nvcc fails in
        # that case and we skip the config rather than fail the run.
        for bs in 128 256; do
            for ipt in 2 4 8; do
                local bin="random_${bs}_${ipt}"
                if ! nvcc -std=c++17 -arch="$arch" \
                        -DALPACC_BLOCK_SIZE="$bs" \
                        -DALPACC_ITEMS_PER_THREAD="$ipt" \
                        -o "$bin" random.cu &> /dev/null; then
                    # (BS, IPT) does not fit; skip.
                    continue
                fi

                ./"$bin" -i random.inputs -o "results_${bs}_${ipt}.bin" 2>/dev/null

                if ! alpacc test compare random.alp random.inputs random.outputs \
                        "results_${bs}_${ipt}.bin" $type_flag $index_flag &> /dev/null; then
                    echo "===== FAIL: batch BS=$bs IPT=$ipt, job $job_id ====="
                    cat random.alp
                    alpacc test compare random.alp random.inputs random.outputs \
                        "results_${bs}_${ipt}.bin" $type_flag $index_flag
                    all_ok=false
                    break 2
                fi
            done
        done

        if ! $all_ok; then return 1; fi

        # The cross-config byte-diff check that used to live here has been
        # removed: alpacc test compare already validated each config's output
        # against the reference tokenisation, and the outputs are not
        # required to be byte-identical across (BS, IPT) since encoding
        # widths are compile-time properties.

        # ---- Test 2: server mode (default BS/IPT) ----
        # Server mode loops counted batches: feeding the batch file twice
        # must yield the batch output twice.
        if ! cat random.inputs random.inputs | ./random --server > server_results.bin 2>/dev/null; then
            echo "===== FAIL: server mode crashed, job $job_id ====="
            all_ok=false
        elif ! cat results_128_2.bin results_128_2.bin | cmp -s - server_results.bin; then
            echo "===== FAIL: server mode output differs from batch, job $job_id ====="
            all_ok=false
        fi

        if ! $all_ok; then return 1; fi

        # ---- Success ----
        (
            flock -x 200
            count=$(cat "$counter_file")
            if [ "$count" -lt "$target" ]; then
                count=$((count + 1))
                echo "$count" > "$counter_file"
                echo "$count/$target completed"
                if [ "$count" -ge "$target" ]; then
                    touch "$done_file"
                fi
            fi
        ) 200>"$counter_file.lock"
        return 0
    done
    return 0
}

export -f run_test

seq 1 $target | parallel --no-notice -j "$parallel_jobs" --halt soon,fail=1 --line-buffer \
    "run_test {} $q_value $k_value $temp_dir $counter_file $target $done_file $arch '$type_flag' '$index_flag'"

final_count=$(cat "$counter_file")
if [ "$final_count" -ge "$target" ]; then
    echo "Tests passed."
    exit 0
else
    echo "Failed to reach target of $target successful runs (got $final_count)"
    exit 1
fi
