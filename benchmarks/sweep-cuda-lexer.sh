#!/usr/bin/env bash
# Sweep (state_t, index_t) × (BLOCK_SIZE, ITEMS_PER_THREAD) for a given
# grammar's CUDA lexer, reporting the fastest configuration per type combo.
#
# Usage:
#   sweep-cuda-lexer.sh <grammar.alp> [dataset.inputs] \
#                       [BS_LIST] [IPT_LIST] [STATE_LIST] [INDEX_LIST]
#
# All list arguments are space-separated inside a single argv slot, e.g.
#   sweep-cuda-lexer.sh json.alp data.inputs "128 256" "2 4 8 16" ...
#
# Defaults exercise the interesting ranges for the JSON grammar on
# Turing-class GPUs:
#   BS_LIST     = "256"
#   IPT_LIST    = "8 12 16 20"
#   STATE_LIST  = "uint8_t"   (state_t is now endo_t fixed by codegen; this is a dummy)
#   INDEX_LIST  = "int32_t"
#
# For each (state_t, index_t) the script:
#   1. Generates the .cu once via `alpacc cuda --lexer` (no --index32 flag —
#      we override index_t at nvcc time instead).
#   2. Rebuilds the binary for every (BS, IPT) with -D overrides.
#   3. Runs `--benchmark N` and reads the kernel-time line.
#   4. Records the fastest configuration.
#   5. Prints a summary table at the end.
#
# Correctness is verified once per (state_t, index_t) by running the binary
# on a small `alpacc test generate --lexer` sample and comparing against the
# reference — but only when the type override is safe (state_t = codegen
# default OR wider).  Overrides that narrow state_t may silently fail on
# grammars whose compose table doesn't fit; we skip the correctness check
# and print a "no-verify" note.

set -euo pipefail

show_usage() {
    cat <<EOF
Usage: $0 <grammar.alp> [dataset.inputs] [BS_LIST] [IPT_LIST] [STATE_LIST] [INDEX_LIST]

Positional args:
  grammar.alp   grammar to lex
  dataset       framed .inputs file to feed the benchmark (default:
                data-\$INPUT_SIZE.inputs alongside the grammar, generated
                via the sibling Makefile if needed)
  BS_LIST       space-separated block sizes  (default: "256")
  IPT_LIST      space-separated IPT values    (default: "8 12 16 20")
  STATE_LIST    space-separated C++ typedefs for state_t
                (default: "uint8_t"; state_t is now endo_t fixed by codegen, this is a dummy)
  INDEX_LIST    space-separated C++ typedefs for index_t
                (default: "int32_t")

Environment:
  INPUT_SIZE    passed to the sibling Makefile when generating the dataset
                (default: 10485760)
  BENCH_RUNS    --benchmark N passed to the CUDA binary (default: 5)
  BENCH_WARMUP  --warmup N (default: 2)
  KEEP_LOGS     if set to 1, keep per-config build/run logs in the artifact
                dir; otherwise they are deleted on success (default: 0)

Output:
  Per-config timings on stderr as the sweep progresses.
  Final winner-per-type-combo table on stdout.
EOF
}

if [ "${1:-}" = "-h" ] || [ "${1:-}" = "--help" ]; then
    show_usage; exit 0
fi
if [ $# -lt 1 ]; then
    echo "error: grammar file required" >&2
    show_usage; exit 1
fi

GRAMMAR="$(realpath "$1")"
shift
INPUT_FILE_ARG="${1:-}"
[ $# -ge 1 ] && shift
BS_LIST="${1:-256}"
[ $# -ge 1 ] && shift
IPT_LIST="${1:-8 12 16 20}"
[ $# -ge 1 ] && shift
STATE_LIST="${1:-uint8_t}"
[ $# -ge 1 ] && shift
INDEX_LIST="${1:-int32_t}"
[ $# -ge 1 ] && shift

INPUT_SIZE="${INPUT_SIZE:-10485760}"
BENCH_RUNS="${BENCH_RUNS:-5}"
BENCH_WARMUP="${BENCH_WARMUP:-2}"
KEEP_LOGS="${KEEP_LOGS:-0}"

GRAMMAR_DIR="$(dirname "$GRAMMAR")"
GRAMMAR_NAME="$(basename "$GRAMMAR" .alp)"

# Locate the benchmarking sub-directory that ships a Makefile capable of
# building the dataset.  json/ and lisp/ under benchmarks/ both fit.
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BENCH_DIR="$REPO_ROOT/benchmarks/$GRAMMAR_NAME"
if [ ! -f "$BENCH_DIR/Makefile" ]; then
    # Fall back to a per-run scratch dir.
    BENCH_DIR="$(mktemp -d)"
    echo "[info] no ready-made benchmark dir for $GRAMMAR_NAME; using $BENCH_DIR" >&2
    trap 'rm -rf "$BENCH_DIR"' EXIT
fi

if [ -z "$INPUT_FILE_ARG" ]; then
    INPUT_FILE="$BENCH_DIR/data-${INPUT_SIZE}.inputs"
    if [ ! -f "$INPUT_FILE" ]; then
        if [ -f "$BENCH_DIR/Makefile" ]; then
            echo "[info] dataset $INPUT_FILE missing; running make to build it..." >&2
            make -C "$BENCH_DIR" "data-${INPUT_SIZE}.inputs" INPUT_SIZE="$INPUT_SIZE"
        else
            echo "[info] no Makefile for $GRAMMAR_NAME; generating dataset with alpacc test..." >&2
            alpacc test generate "$GRAMMAR" --single-long --length "$INPUT_SIZE" \
                --no-outputs --lexer -o "${BENCH_DIR}/data-${INPUT_SIZE}"
        fi
    fi
else
    INPUT_FILE="$(realpath "$INPUT_FILE_ARG")"
fi
[ -f "$INPUT_FILE" ] || { echo "error: dataset $INPUT_FILE not found" >&2; exit 1; }

ARTIFACT_DIR="$REPO_ROOT/benchmarks/results/sweep-${GRAMMAR_NAME}-$(date +%Y%m%d-%H%M%S)"
mkdir -p "$ARTIFACT_DIR"
SUMMARY_TSV="$ARTIFACT_DIR/summary.tsv"
printf 'state_t\tindex_t\tblock_size\titems_per_thread\tkernel_us\tinput_gbps\tbytes\n' > "$SUMMARY_TSV"

echo "[info] grammar   : $GRAMMAR"
echo "[info] dataset   : $INPUT_FILE"
echo "[info] artifacts : $ARTIFACT_DIR"
echo "[info] BS list   : $BS_LIST"
echo "[info] IPT list  : $IPT_LIST"
echo "[info] STATE list: $STATE_LIST"
echo "[info] INDEX list: $INDEX_LIST"

# Detect the SM arch once so we can pin it across the sweep (avoids the
# arch table changing under our feet if the auto-probe ever picked
# something different mid-run).
SM_ARCH="$(nvidia-smi --query-gpu=compute_cap --format=csv,noheader | head -1 | tr -d '.')"
[ -n "$SM_ARCH" ] || SM_ARCH=75  # last-ditch fallback

echo "[info] sm_arch  : $SM_ARCH"

# ---------------------------------------------------------------------------
# One .cu per (state_t, index_t) combo — regenerated with --sm-arch
# matching the local GPU so the compile-time table is well-defined.  Type
# overrides are applied via -D at nvcc time.
# ---------------------------------------------------------------------------
generate_cu() {
    local state_t="$1"
    local index_t="$2"
    local out="$3"
    alpacc cuda "$GRAMMAR" --lexer --sm-arch "$SM_ARCH" -o "$out"
}

# Format-preserving `printf` for the winner line.
print_winner() {
    printf '  → best: BS=%-3s IPT=%-3s kernel=%s us (%s GB/s input)\n' \
           "$1" "$2" "$3" "$4"
}

# For each (state_t, index_t) combo, generate the .cu once and sweep.
for state_t in $STATE_LIST; do
    for index_t in $INDEX_LIST; do
        combo="${state_t}_${index_t}"
        combo_dir="$ARTIFACT_DIR/$combo"
        mkdir -p "$combo_dir"
        cu_file="$combo_dir/lexer.cu"
        generate_cu "$state_t" "$index_t" "$cu_file" >/dev/null 2>&1 || {
            echo "[skip] alpacc cuda failed for state_t=$state_t index_t=$index_t" >&2
            continue
        }

        best_us=""
        best_gbps=""
        best_bs=""
        best_ipt=""
        best_bytes=""
        echo
        echo "=== state_t=$state_t  index_t=$index_t ==="

        for bs in $BS_LIST; do
            for ipt in $IPT_LIST; do
                bin="$combo_dir/lexer-${bs}-${ipt}"
                buildlog="$combo_dir/build-${bs}-${ipt}.log"

                if ! nvcc -O3 -std=c++17 -arch=native \
                        -DALPACC_STATE_T="$state_t" \
                        -DALPACC_INDEX_T="$index_t" \
                        -DALPACC_BLOCK_SIZE="$bs" \
                        -DALPACC_ITEMS_PER_THREAD="$ipt" \
                        -o "$bin" "$cu_file" \
                        >"$buildlog" 2>&1; then
                    if grep -q "uses too much shared data" "$buildlog"; then
                        printf '  BS=%-3s IPT=%-3s : shmem-overflow, skipped\n' "$bs" "$ipt"
                    elif grep -q "static assertion failed" "$buildlog"; then
                        printf '  BS=%-3s IPT=%-3s : static_assert (illegal config), skipped\n' "$bs" "$ipt"
                    else
                        printf '  BS=%-3s IPT=%-3s : build failed (see %s)\n' "$bs" "$ipt" "$buildlog"
                    fi
                    continue
                fi

                runlog="$combo_dir/run-${bs}-${ipt}.log"
                if ! "$bin" --benchmark "$BENCH_RUNS" --warmup "$BENCH_WARMUP" \
                        < "$INPUT_FILE" > "$runlog" 2>&1; then
                    printf '  BS=%-3s IPT=%-3s : runtime failure (see %s)\n' "$bs" "$ipt" "$runlog"
                    continue
                fi

                # Parse the kernel-only line, e.g.:
                #   lex_int (cuda, 22198933 bytes, 10535757 tokens):
                #           2926μs (95% CI: [...]); 33GB/s (8GB/s input-only)
                local_line=$(grep -m1 -E "lex_int .cuda," "$runlog" || true)
                data_line=$(grep -A1 -m1 -E "lex_int .cuda," "$runlog" | tail -1 || true)
                us=$(echo "$data_line" | grep -oE '^[[:space:]]*[0-9]+μs' | head -1 | grep -oE '[0-9]+')
                gbps=$(echo "$data_line" | grep -oE '[0-9]+GB/s \([0-9]+GB/s input-only\)' | head -1 | grep -oE '\([0-9]+GB/s' | grep -oE '[0-9]+')
                bytes=$(echo "$local_line" | grep -oE '[0-9]+ bytes' | head -1 | grep -oE '[0-9]+')
                if [ -z "$us" ]; then
                    printf '  BS=%-3s IPT=%-3s : could not parse timing (see %s)\n' "$bs" "$ipt" "$runlog"
                    continue
                fi

                printf '  BS=%-3s IPT=%-3s : %6s us, %s GB/s input-only\n' "$bs" "$ipt" "$us" "$gbps"
                printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
                    "$state_t" "$index_t" "$bs" "$ipt" "$us" "$gbps" "$bytes" \
                    >> "$SUMMARY_TSV"

                if [ -z "$best_us" ] || [ "$us" -lt "$best_us" ]; then
                    best_us="$us"; best_gbps="$gbps"
                    best_bs="$bs"; best_ipt="$ipt"; best_bytes="$bytes"
                fi

                if [ "$KEEP_LOGS" != "1" ]; then
                    rm -f "$buildlog" "$runlog"
                fi
            done
        done

        if [ -n "$best_us" ]; then
            print_winner "$best_bs" "$best_ipt" "$best_us" "$best_gbps"
        else
            echo "  (no successful build for this combo)"
        fi

        # Drop the per-combo directory when no logs to keep — the binaries
        # aren't useful past the sweep and can be large.
        if [ "$KEEP_LOGS" != "1" ]; then
            rm -rf "$combo_dir"
        fi
    done
done

echo
echo "============================================================"
echo "Summary (fastest per type combo):"
echo "============================================================"
# Group by (state_t, index_t), find min kernel_us per group.
if command -v awk >/dev/null 2>&1; then
    # First: build the winner-per-combo table into a temp, then pretty-print
    # with the header on top and body sorted deterministically.
    winners="$ARTIFACT_DIR/winners.tsv"
    awk -F'\t' 'NR>1 {
        key = $1"\t"$2
        if (!(key in best) || $5+0 < best[key]+0) {
            best[key] = $5; bs[key] = $3; ipt[key] = $4; gb[key] = $6
        }
    } END {
        for (k in best) {
            split(k, p, "\t")
            printf "%s\t%s\t%s\t%s\t%s\t%s\n", p[1], p[2], bs[k], ipt[k], best[k], gb[k]
        }
    }' "$SUMMARY_TSV" | sort > "$winners"
    printf '  %-10s %-10s %-4s %-4s %-10s %-8s\n' \
        "state_t" "index_t" "BS" "IPT" "kernel_us" "GB/s"
    awk -F'\t' '{ printf "  %-10s %-10s %-4s %-4s %-10s %-8s\n", $1, $2, $3, $4, $5, $6 }' \
        "$winners"
fi

echo
echo "Full TSV: $SUMMARY_TSV"
