#!/bin/bash

# Function to show usage
show_usage() {
    echo "Usage: $0 [q_value] [k_value] [target_runs] [type_flag]"
    echo "  q_value: -q parameter for alpacc (default: 1)"
    echo "  k_value: -k parameter for alpacc (default: 1)"
    echo "  target_runs: number of successful runs needed (default: 10)"
    echo "  type_flag: either empty, --lexer, or --parser (default: empty)"
    echo "Example: $0 2 3 50 --lexer"
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
type_flag="${4:-}"

# Validate that arguments are numbers
if ! [[ "$q_value" =~ ^[0-9]+$ ]] || ! [[ "$k_value" =~ ^[0-9]+$ ]] || ! [[ "$target" =~ ^[0-9]+$ ]]; then
    echo "Error: All arguments must be positive integers"
    show_usage
    exit 1
fi

success_count=0

echo "Starting alpacc testing script..."
echo "Target: $target successful runs"
echo "Using -q $q_value -k $k_value $type_flag"

while [ $success_count -lt $target ]; do    
    if alpacc random &> /dev/null; then
        if alpacc futhark random.alp $type_flag -q $q_value -k $k_value &> /dev/null; then
            alpacc test generate random.alp $type_flag -q $q_value -k $k_value &> /dev/null
            futhark script -b random.fut 'test ($loadbytes "random.inputs")' | tail -c +16 > random.results
            if alpacc test compare random.alp random.inputs random.outputs random.results $type_flag &> /dev/null; then
                :
            else
                echo "Tests failed."
                alpacc test compare random.alp random.inputs random.outputs random.results $type_flag
                exit 1
            fi
            success_count=$((success_count + 1))
            echo "$success_count/$target completed"
        else
            :
        fi
    else
        echo "alpacc random failed"
        exit 1
    fi    
done

echo "Tests passes."
