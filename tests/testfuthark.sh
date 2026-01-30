#!/bin/bash

# Function to show usage
show_usage() {
    echo "Usage: $0 [q_value] [k_value] [target_runs] [parallel_jobs] [type_flag]"
    echo "  q_value: -q parameter for alpacc (default: 1)"
    echo "  k_value: -k parameter for alpacc (default: 1)"
    echo "  target_runs: number of successful runs needed (default: 10)"
    echo "  parallel_jobs: number of parallel jobs (default: number of CPU cores)"
    echo "  type_flag: either empty, --lexer, or --parser (default: empty)"
    echo "Example: $0 2 3 50 4 --lexer"
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
type_flag="${5:-}"

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
echo "Using -q $q_value -k $k_value $type_flag"
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

# Function to run a single test iteration - mirrors the original while loop body
run_test() {
    local job_id=$1
    local q_value=$2
    local k_value=$3
    local type_flag=$4
    local temp_dir=$5
    local counter_file=$6
    local target=$7
    local done_file=$8
    
    # Check if we're already done
    if [ -f "$done_file" ]; then
        return 0
    fi
    
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
    
    # Exact logic from original script
    if alpacc random &> /dev/null; then
        if alpacc futhark random.alp $type_flag -q $q_value -k $k_value &> /dev/null; then
            alpacc test generate random.alp $type_flag -q $q_value -k $k_value &> /dev/null
            futhark script -b random.fut 'test ($loadbytes "random.inputs")' | tail -c +16 > random.results
            if alpacc test compare random.alp random.inputs random.outputs random.results $type_flag &> /dev/null; then
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
            else
                echo "Tests failed."
                alpacc test compare random.alp random.inputs random.outputs random.results $type_flag
                return 1
            fi
        else
            : # alpacc futhark failed, skip silently
        fi
    else
        echo "alpacc random failed"
        return 1
    fi
    
    return 0
}

export -f run_test

# Run tests in parallel until we reach the target
seq 1 $((target * 10)) | parallel --no-notice -j "$parallel_jobs" --halt soon,fail=1 --line-buffer \
    "run_test {} $q_value $k_value '$type_flag' $temp_dir $counter_file $target $done_file"

# Check final count
final_count=$(cat "$counter_file")
if [ "$final_count" -ge "$target" ]; then
    echo "Tests passes."
    exit 0
else
    echo "Failed to reach target of $target successful runs (got $final_count)"
    exit 1
fi
