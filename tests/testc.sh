#!/bin/bash

# Differential test of the C backend (sequential LLP reference implementation).
# Generates random grammars, compiles the generated C program with cc, runs it
# on `alpacc test generate` inputs and checks results with `alpacc test compare`.
# Also runs the binary in --server mode (a loop of counted batches) with
# the batch fed twice and checks it matches the batch output twice over.
# Supports --lexer, --parser, and combined (no flag) modes.
# Runs without a GPU (usable in hosted CI).

show_usage() {
    echo "Usage: $0 [q_value] [k_value] [target_runs] [parallel_jobs] [type_flag] [--index32]"
    echo "  q_value:       -q parameter for alpacc (default: 1)"
    echo "  k_value:       -k parameter for alpacc (default: 1)"
    echo "  target_runs:   number of successful runs needed (default: 10)"
    echo "  parallel_jobs: number of parallel jobs (default: number of CPU cores)"
    echo "  type_flag:     --lexer, --parser, or empty for combined (default: empty)"
    echo "  --index32:     pass --index32 to generator and test tools (default: 64-bit)"
    echo "Example: $0 2 3 50 4 --parser"
    echo "Example: $0 2 3 50 4 --parser --index32"
}

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
    show_usage; exit 0
fi

q_value="${1:-1}"
k_value="${2:-1}"
target="${3:-10}"
parallel_jobs="${4:-$(nproc)}"
type_flag="${5:-}"
index_flag="${6:-}"
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

echo "Starting alpacc C backend testing script..."
echo "Target: $target successful runs"
echo "Using -q $q_value -k $k_value ${type_flag:-<combined>} ${index_flag}"
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
    local type_flag=$8
    local index_flag=$9

    local work_dir="$temp_dir/job_$job_id"
    mkdir -p "$work_dir"
    cd "$work_dir"

    while [ ! -f "$done_file" ]; do
        if ! alpacc random &> /dev/null; then
            echo "alpacc random failed"; return 1
        fi

        cat > random.alp.tmp << EOF
params {
  lookback=$q_value.
  lookahead=$k_value.
}

EOF
        cat random.alp >> random.alp.tmp
        mv random.alp.tmp random.alp

        # Try to generate C code; skip grammars that fail codegen.
        if ! alpacc c random.alp $type_flag $index_flag &> /dev/null; then
            continue
        fi

        if ! cc -std=c99 -O2 -o random random.c &> /dev/null; then
            echo "cc compilation failed for job $job_id"; return 1
        fi

        alpacc test generate random.alp $type_flag $index_flag &> /dev/null
        ./random < random.inputs > random.results

        # Server mode loops counted batches: feeding the batch file twice
        # must yield the batch output twice.
        if ! cat random.inputs random.inputs | ./random --server > server_results.bin; then
            echo "===== FAIL: server mode crashed, job $job_id ====="
            cat random.alp
            return 1
        elif ! cat random.results random.results | cmp -s - server_results.bin; then
            echo "===== FAIL: server mode output differs from batch, job $job_id ====="
            cat random.alp
            return 1
        fi

        if alpacc test compare random.alp random.inputs random.outputs random.results $type_flag $index_flag &> /dev/null; then
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
        else
            echo "========================================="
            echo "Tests failed for job $job_id"
            echo "========================================="
            echo "Content of random.alp:"
            echo "-----------------------------------------"
            cat random.alp
            echo "-----------------------------------------"
            echo "Test comparison output:"
            alpacc test compare random.alp random.inputs random.outputs random.results $type_flag $index_flag
            echo "========================================="
            return 1
        fi
    done

    return 0
}

export -f run_test

seq 1 $target | parallel --no-notice -j "$parallel_jobs" --halt soon,fail=1 --line-buffer \
    "run_test {} $q_value $k_value $temp_dir $counter_file $target $done_file '$type_flag' '$index_flag'"

final_count=$(cat "$counter_file")
if [ "$final_count" -ge "$target" ]; then
    echo "Tests passes."
    exit 0
else
    echo "Failed to reach target of $target successful runs (got $final_count)"
    exit 1
fi
