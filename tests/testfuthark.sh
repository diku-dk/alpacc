#!/bin/bash

# Function to show usage
show_usage() {
    echo "Usage: $0 [q_value] [k_value] [target_runs] [parallel_jobs] [type_flag] [backend] [--index32]"
    echo "  q_value: -q parameter for alpacc (default: 1)"
    echo "  k_value: -k parameter for alpacc (default: 1)"
    echo "  target_runs: number of successful runs needed (default: 10)"
    echo "  parallel_jobs: number of parallel jobs (default: number of CPU cores)"
    echo "  type_flag: either empty, --lexer, or --parser (default: empty)"
    echo "  backend: futhark backend (default: multicore)"
    echo "  --index32: pass --index32 to generator and test tools (default: 64-bit)"
    echo "Example: $0 2 3 50 4 --lexer multicore"
    echo "Example: $0 2 3 50 4 --lexer multicore --index32"
}

# Check for help flag
if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
    show_usage
    exit 0
fi

# Get arguments with validation
q_value="${1:-1}"
k_value="${2:-1}"
target="${3:-10}"
parallel_jobs="${4:-$(nproc)}"

# arg5 is type_flag (--lexer or --parser) or omitted; arg6 is backend.
# When called from the Makefile in combined mode the empty type_flag is dropped
# by shell word-splitting, so backend lands in arg5. Detect this by checking
# whether arg5 looks like a flag.
if [[ "${5:-}" == --lexer || "${5:-}" == --parser ]]; then
    type_flag="${5}"
    backend="${6:-multicore}"
    index_flag="${7:-}"
else
    type_flag=""
    backend="${5:-multicore}"
    index_flag="${6:-}"
fi

# Only --index32 (or empty) is accepted for the index flag.
if [[ -n "$index_flag" && "$index_flag" != "--index32" ]]; then
    echo "Error: unrecognised index flag '$index_flag' (expected --index32 or empty)"
    show_usage
    exit 1
fi

# Validate that arguments are numbers
if ! [[ "$q_value" =~ ^[0-9]+$ ]] || ! [[ "$k_value" =~ ^[0-9]+$ ]] || ! [[ "$target" =~ ^[0-9]+$ ]]; then
    echo "Error: q_value, k_value, and target must be positive integers"
    show_usage
    exit 1
fi

if ! [[ "$parallel_jobs" =~ ^[0-9]+$ ]]; then
    echo "Error: parallel_jobs must be a positive integer"
    show_usage
    exit 1
fi

echo "Starting alpacc testing script..."
echo "Target: $target successful runs"
echo "Using -q $q_value -k $k_value $type_flag $index_flag (backend=$backend)"
echo "Running with $parallel_jobs parallel jobs"

# Create a temporary directory for this run
temp_dir=$(mktemp -d)
trap "rm -rf $temp_dir" EXIT

# Set up Futhark packages once in the temp directory
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

# Counter file for successful runs
counter_file="$temp_dir/counter"
echo "0" > "$counter_file"

# Flag file to signal early termination
done_file="$temp_dir/done"

# Function to run a single test iteration - keeps trying until one success
run_test() {
    local job_id=$1
    local q_value=$2
    local k_value=$3
    local temp_dir=$4
    local counter_file=$5
    local target=$6
    local done_file=$7
    local backend=$8
    local type_flag=$9
    local index_flag=${10}
    
    # Create unique work directory for this job
    local work_dir="$temp_dir/job_$job_id"
    mkdir -p "$work_dir"
    cd "$work_dir"
    
    # Copy the lib directory from the parent temp_dir
    if [ -d "$temp_dir/lib" ]; then
        cp -r "$temp_dir/lib" .
    fi
    
    # Copy futhark.pkg if it exists
    if [ -f "$temp_dir/futhark.pkg" ]; then
        cp "$temp_dir/futhark.pkg" .
    fi
    
    # Keep trying until we get one successful test or we're done
    while [ ! -f "$done_file" ]; do
        # Generate random grammar
        if ! alpacc random &> /dev/null; then
            echo "alpacc random failed"
            return 1
        fi
        
        # Prepend params block to random.alp
        cat > random.alp.tmp << EOF
params {
  lookback=$q_value.
  lookahead=$k_value.
}

EOF
        cat random.alp >> random.alp.tmp
        mv random.alp.tmp random.alp
        
        # Try to convert to Futhark - keep retrying if it fails
        if ! alpacc futhark random.alp $type_flag $index_flag &> /dev/null; then
            continue  # Try a new random grammar
        fi

        # Now we have a valid Futhark conversion, run the test
        alpacc test generate random.alp $type_flag $index_flag &> /dev/null
        futhark eval -w -f random.fut --backend="$backend" \
            'io.savevalue "random.results.bin" (test (io.loadbytes "random.inputs"))' > /dev/null
        tail -c +16 random.results.bin > random.results

        if alpacc test compare random.alp random.inputs random.outputs random.results $type_flag $index_flag &> /dev/null; then
            # Success! Increment counter atomically
            (
                flock -x 200
                count=$(cat "$counter_file")
                
                # Only increment if we're still under the target
                if [ "$count" -lt "$target" ]; then
                    count=$((count + 1))
                    echo "$count" > "$counter_file"
                    echo "$count/$target completed"
                    
                    # Check if we've now reached the target
                    if [ "$count" -ge "$target" ]; then
                        touch "$done_file"
                    fi
                fi
            ) 200>"$counter_file.lock"
            
            # We completed one successful test, exit this job
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

# Run enough parallel jobs to reach the target
# Each job will complete one successful test
seq 1 $target | parallel --no-notice -j "$parallel_jobs" --halt soon,fail=1 --line-buffer \
    "run_test {} $q_value $k_value $temp_dir $counter_file $target $done_file '$backend' '$type_flag' '$index_flag'"

# Check final count
final_count=$(cat "$counter_file")
if [ "$final_count" -ge "$target" ]; then
    echo "Tests passes."
    exit 0
else
    echo "Failed to reach target of $target successful runs (got $final_count)"
    exit 1
fi
