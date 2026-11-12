#!/usr/bin/env bash
# Sweep (state_t, index_t) × (PARSER_BLOCK_SIZE, PARSER_ITEMS_PER_THREAD)
# for a given grammar's CUDA parser (`parser-cuda`), reporting the fastest
# configuration per type combo.  Mirrors `sweep-cuda-lexer.sh` but targets
# the fused parser kernel.
#
# Usage:
#   sweep-cuda-parser.sh <grammar.alp> [dataset.inputs] \
#                        [BS_LIST] [IPT_LIST] [STATE_LIST] [INDEX_LIST]
#
# All list arguments are space-separated inside a single argv slot, e.g.
#   sweep-cuda-parser.sh json.alp data.inputs "128 256" "4" ...
#
# Defaults exercise the practical parser range on Turing-class GPUs:
#   BS_LIST     = "128 256"
#   IPT_LIST    = "4"          (PSE currently static_asserts IPT == 4)
#   STATE_LIST  = "uint8_t uint16_t uint32_t"
#   INDEX_LIST  = "int32_t int64_t"
#
# For each (state_t, index_t) the script:
#   1. Generates the .cu once via `alpacc cuda` (no --parser/--index32
#      flags — we override at nvcc time).
#   2. Rebuilds the binary for every (BS, IPT) with -D overrides for
#      ALPACC_PARSER_BLOCK_SIZE / ALPACC_PARSER_ITEMS_PER_THREAD.
#   3. Runs `--benchmark N` and reads the parse_int kernel-time line.
#   4. Records the fastest configuration.
#   5. Prints a summary table at the end.
#
# NOTE: unlike the lexer sweep, the parser's tuning surface is currently
# limited by the PSE primitive's `static_assert(IPT == 4)`.  Higher IPT
# values will fail at nvcc time and are silently skipped in the sweep.

set -euo pipefail

show_usage() {
    cat <<EOF
Usage: $0 <grammar.alp> [dataset.inputs] [BS_LIST] [IPT_LIST] [STATE_LIST] [INDEX_LIST]

Positional args:
  grammar.alp   grammar to parse
  dataset       framed .inputs file to feed the parser benchmark (default:
                data-\$INPUT_SIZE.inputs alongside the grammar, generated
                via the sibling Makefile if needed)
  BS_LIST       space-separated block sizes  (default: "128 256")
  IPT_LIST      space-separated IPT values    (default: "4")
  STATE_LIST    space-separated C++ typedefs for state_t
                (default: "uint8_t uint16_t uint32_t")
  INDEX_LIST    space-separated C++ typedefs for index_t
                (default: "int32_t int64_t")

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
BS_LIST="${1:-128 256}"
[ $# -ge 1 ] && shift
IPT_LIST="${1:-4}"
[ $# -ge 1 ] && shift
STATE_LIST="${1:-uint8_t uint16_t uint32_t}"
[ $# -ge 1 ] && shift
INDEX_LIST="${1:-int32_t int64_t}"
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
                --no-outputs -o "${BENCH_DIR}/data-${INPUT_SIZE}"
        fi
    fi
else
    INPUT_FILE="$(realpath "$INPUT_FILE_ARG")"
fi
[ -f "$INPUT_FILE" ] || { echo "error: dataset $INPUT_FILE not found" >&2; exit 1; }

ARTIFACT_DIR="$REPO_ROOT/benchmarks/results/parser-sweep-${GRAMMAR_NAME}-$(date +%Y%m%d-%H%M%S)"
mkdir -p "$ARTIFACT_DIR"
SUMMARY_TSV="$ARTIFACT_DIR/summary.tsv"
printf 'state_t\tindex_t\tblock_size\titems_per_thread\tkernel_us\tbytes\n' > "$SUMMARY_TSV"

echo "[info] grammar   : $GRAMMAR"
echo "[info] dataset   : $INPUT_FILE"
echo "[info] artifacts : $ARTIFACT_DIR"
echo "[info] BS list   : $BS_LIST"
echo "[info] IPT list  : $IPT_LIST"
echo "[info] STATE list: $STATE_LIST"
echo "[info] INDEX list: $INDEX_LIST"

SM_ARCH="$(nvidia-smi --query-gpu=compute_cap --format=csv,noheader | head -1 | tr -d '.')"
[ -n "$SM_ARCH" ] || SM_ARCH=75

echo "[info] sm_arch  : $SM_ARCH"

# ---------------------------------------------------------------------------
# One .cu per (state_t, index_t) combo — the parser binary is combined
# (lexer+parser) since parser-only mode expects framed token IDs which
# the sweep dataset doesn't produce.  --sm-arch pinned to local GPU.
# ---------------------------------------------------------------------------
generate_cu() {
    local state_t="$1"
    local index_t="$2"
    local out="$3"
    alpacc cuda "$GRAMMAR" --sm-arch "$SM_ARCH" -o "$out"
}

print_winner() {
    printf '  → best: BS=%-3s IPT=%-3s kernel=%s us\n' "$1" "$2" "$3"
}

for state_t in $STATE_LIST; do
    for index_t in $INDEX_LIST; do
        combo="${state_t}_${index_t}"
        combo_dir="$ARTIFACT_DIR/$combo"
        mkdir -p "$combo_dir"
        cu_file="$combo_dir/parser.cu"
        generate_cu "$state_t" "$index_t" "$cu_file" >/dev/null 2>&1 || {
            echo "[skip] alpacc cuda failed for state_t=$state_t index_t=$index_t" >&2
            continue
        }

        best_us=""
        best_bs=""
        best_ipt=""
        best_bytes=""
        echo
        echo "=== state_t=$state_t  index_t=$index_t ==="

        for bs in $BS_LIST; do
            for ipt in $IPT_LIST; do
                bin="$combo_dir/parser-${bs}-${ipt}"
                buildlog="$combo_dir/build-${bs}-${ipt}.log"

                if ! nvcc -O3 -std=c++17 -arch=native \
                        -DALPACC_STATE_T="$state_t" \
                        -DALPACC_INDEX_T="$index_t" \
                        -DALPACC_PARSER_BLOCK_SIZE="$bs" \
                        -DALPACC_PARSER_ITEMS_PER_THREAD="$ipt" \
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

                # Parse the parse_int kernel-only line, e.g.:
                #   parse_int (cuda, 22198933 bytes):
                #           17248μs (95% CI: [...]); 1GB/s
                local_line=$(grep -m1 -E "parse_int .cuda," "$runlog" || true)
                data_line=$(grep -A1 -m1 -E "parse_int .cuda," "$runlog" | tail -1 || true)
                us=$(echo "$data_line" | grep -oE '^[[:space:]]*[0-9]+μs' | head -1 | grep -oE '[0-9]+')
                bytes=$(echo "$local_line" | grep -oE '[0-9]+ bytes' | head -1 | grep -oE '[0-9]+')
                if [ -z "$us" ]; then
                    printf '  BS=%-3s IPT=%-3s : could not parse timing (see %s)\n' "$bs" "$ipt" "$runlog"
                    continue
                fi

                printf '  BS=%-3s IPT=%-3s : %6s us\n' "$bs" "$ipt" "$us"
                printf '%s\t%s\t%s\t%s\t%s\t%s\n' \
                    "$state_t" "$index_t" "$bs" "$ipt" "$us" "$bytes" \
                    >> "$SUMMARY_TSV"

                if [ -z "$best_us" ] || [ "$us" -lt "$best_us" ]; then
                    best_us="$us"
                    best_bs="$bs"; best_ipt="$ipt"; best_bytes="$bytes"
                fi

                if [ "$KEEP_LOGS" != "1" ]; then
                    rm -f "$buildlog" "$runlog"
                fi
            done
        done

        if [ -n "$best_us" ]; then
            print_winner "$best_bs" "$best_ipt" "$best_us"
        else
            echo "  (no successful build for this combo)"
        fi

        if [ "$KEEP_LOGS" != "1" ]; then
            rm -rf "$combo_dir"
        fi
    done
done

echo
echo "============================================================"
echo "Summary (fastest per type combo):"
echo "============================================================"
if command -v awk >/dev/null 2>&1; then
    winners="$ARTIFACT_DIR/winners.tsv"
    awk -F'\t' 'NR>1 {
        key = $1"\t"$2
        if (!(key in best) || $5+0 < best[key]+0) {
            best[key] = $5; bs[key] = $3; ipt[key] = $4
        }
    } END {
        for (k in best) {
            split(k, p, "\t")
            printf "%s\t%s\t%s\t%s\t%s\n", p[1], p[2], bs[k], ipt[k], best[k]
        }
    }' "$SUMMARY_TSV" | sort > "$winners"
    printf '  %-10s %-10s %-4s %-4s %-10s\n' \
        "state_t" "index_t" "BS" "IPT" "kernel_us"
    awk -F'\t' '{ printf "  %-10s %-10s %-4s %-4s %-10s\n", $1, $2, $3, $4, $5 }' \
        "$winners"
fi

echo
echo "Full TSV: $SUMMARY_TSV"
