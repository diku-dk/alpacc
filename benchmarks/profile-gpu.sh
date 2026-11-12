#!/usr/bin/env bash
# Comprehensive GPU performance measurement for the alpacc CUDA lexer + parser.
#
# Collects, into one results directory (and tarball):
#   1. environment info (GPU, driver, CUDA, ncu versions, git revision)
#   2. wall-clock benchmarks (binary --benchmark, CI-based)
#   3. ncu clock-locked kernel timings (min/mean per kernel, CSV kept)
#   4. ncu LaunchStats + Occupancy (regs/thread, shared/block, blocks/SM)
#   5. full ncu profiles (.ncu-rep, one launch each, -lineinfo build
#      so per-source-line attribution works when imported elsewhere)
#   6. nvprof GPU trace + summary (only on CC < 8.0 — nvprof does not
#      support Ampere+; on A100 an nsys timeline is captured instead)
#
# Usage (from the repo root, alpacc + nvcc + ncu on PATH):
#   bash benchmarks/profile-gpu.sh [results-dir]
# Tunables (env):
#   INPUT_SIZE  (default 52428800)  dataset size in tokens (~2 bytes/token
#                                   payload), shared by lexer and parser
#   REPS        (default 10)        wall-clock benchmark repetitions
#   NCU_REPS    (default 12)        timed launches for ncu kernel timing
#
# Everything runs sequentially. Each section logs to its own file; the
# script never aborts on a failed section (it records the failure and
# moves on), so a partial environment still yields a usable bundle.

set -u
cd "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/.."   # repo root
REPO_ROOT=$(pwd)

INPUT_SIZE=${INPUT_SIZE:-52428800}
REPS=${REPS:-10}
NCU_REPS=${NCU_REPS:-12}
WARMUP=2

GPU_NAME=$(nvidia-smi --query-gpu=name --format=csv,noheader 2>/dev/null \
             | head -1 | tr ' /' '--' || echo unknown-gpu)
OUT=${1:-"profile-results-${GPU_NAME}-$(date +%Y%m%d-%H%M%S)"}
mkdir -p "$OUT"
OUT=$(cd "$OUT" && pwd)

BJ="benchmarks/json"
DATA="$BJ/data-${INPUT_SIZE}.inputs"

log() { printf '\n=== %s ===\n' "$*" | tee -a "$OUT/driver.log"; }
run() { # run <logfile> <cmd...>; records exit code, never aborts
    local f="$OUT/$1"; shift
    echo "+ $*" >> "$OUT/driver.log"
    "$@" > "$f" 2>&1
    local rc=$?
    echo "exit=$rc" >> "$f"
    [ $rc -ne 0 ] && echo "WARNING: '$*' exited $rc (see $f)" | tee -a "$OUT/driver.log"
    return 0
}

# ---------------------------------------------------------------- 1. env
log "Environment"
{
    echo "date: $(date -Iseconds)"
    echo "host: $(hostname)"
    echo "git:  $(git rev-parse HEAD 2>/dev/null) $(git status --porcelain | wc -l) dirty files"
    command -v alpacc || echo "WARNING: alpacc not on PATH — builds will fail"
    nvcc --version 2>&1 | tail -1
    ncu --version 2>&1 | tail -1
    command -v nvprof >/dev/null && nvprof --version 2>&1 | sed -n 2p
    command -v nsys   >/dev/null && nsys --version 2>&1
} > "$OUT/env.txt" 2>&1
nvidia-smi -q > "$OUT/nvidia-smi.txt" 2>&1
nvidia-smi --query-gpu=name,compute_cap,memory.total,clocks.max.sm,driver_version \
           --format=csv > "$OUT/gpu.csv" 2>&1
CC=$(nvidia-smi --query-gpu=compute_cap --format=csv,noheader 2>/dev/null | head -1)
CC_MAJOR=${CC%%.*}
cat "$OUT/env.txt" "$OUT/gpu.csv"

# -------------------------------------------------------------- 2. build
log "Building binaries and datasets (sequential; dataset gen can take minutes)"
run build.log make -C "$BJ" parser-cuda lexer-cuda "data-${INPUT_SIZE}.inputs" INPUT_SIZE="$INPUT_SIZE"
# Separate -lineinfo builds for the full ncu profiles (same code, source
# attribution enabled; not used for timing so -lineinfo cannot skew numbers).
run build-prof.log bash -c "
    nvcc -O3 -std=c++17 -arch=native -lineinfo -o $BJ/parser-cuda-prof $BJ/parser.cu &&
    nvcc -O3 -std=c++17 -arch=native -lineinfo -o $BJ/lexer-cuda-prof  $BJ/lexer.cu"
for f in "$BJ/parser-cuda" "$BJ/lexer-cuda" "$DATA"; do
    [ -e "$f" ] || { echo "FATAL: $f missing — build failed, aborting." | tee -a "$OUT/driver.log"; exit 1; }
done

# ---------------------------------------------------- 3. wall-clock bench
log "Wall-clock benchmarks (binary --benchmark $REPS --warmup 3)"
run bench-parser.log bash -c "$BJ/parser-cuda --benchmark $REPS --warmup 3 < $DATA"
run bench-lexer.log  bash -c "$BJ/lexer-cuda  --benchmark $REPS --warmup 3 < $DATA"
cat "$OUT/bench-parser.log" "$OUT/bench-lexer.log"

# ------------------------------------------------- 4. ncu kernel timings
# Clock-locked (base clocks) per-launch durations; compare MINIMA across
# machines — means drift with clocks and profiler overhead.
# ncu CSV prints locale-dependent thousands separators; durations are
# integer ns, so strip every non-digit before doing arithmetic.
summarize_ns() { # stdin: ncu csv; $1: label
    grep gpu__time_duration "$2" \
      | awk -F'","' -v lbl="$1" '{ v=$NF; gsub(/[^0-9]/,"",v); ms=v/1e6;
            n++; s+=ms; if (min==0 || ms<min) min=ms; if (ms>max) max=ms }
        END { if (n) printf "%s: n=%d  min=%.3f ms  mean=%.3f ms  max=%.3f ms\n",
                            lbl, n, min, s/n, max
              else  printf "%s: NO LAUNCHES CAPTURED\n", lbl }'
}
log "ncu clock-locked kernel timing: parserFusedKernel"
run ncu-time-parser.csv \
    ncu --clock-control base --csv --metrics gpu__time_duration.sum \
        -k parserFusedKernel -c 40 \
        "$BJ/parser-cuda" --benchmark "$NCU_REPS" --warmup "$WARMUP" < "$DATA"
summarize_ns "parserFusedKernel" "$OUT/ncu-time-parser.csv" | tee -a "$OUT/summary.txt"

log "ncu clock-locked kernel timing: lexer kernel"
# The lexer kernel is templated ('lexer<...>') and launched once per chunk;
# cap captured launches so a 500 MB input stays tractable under ncu.
run ncu-time-lexer.csv \
    ncu --clock-control base --csv --metrics gpu__time_duration.sum \
        -k "regex:lexer" -c 60 \
        "$BJ/lexer-cuda" --benchmark "$NCU_REPS" --warmup "$WARMUP" < "$DATA"
summarize_ns "lexer" "$OUT/ncu-time-lexer.csv" | tee -a "$OUT/summary.txt"

# --------------------------------------- 5. LaunchStats + Occupancy
log "LaunchStats + Occupancy"
run launchstats-parser.txt \
    ncu --clock-control base --section LaunchStats --section Occupancy \
        -k parserFusedKernel -s 1 -c 1 \
        "$BJ/parser-cuda" --benchmark 2 --warmup 1 < "$DATA"
run launchstats-lexer.txt \
    ncu --clock-control base --section LaunchStats --section Occupancy \
        -k "regex:lexer" -s 1 -c 1 \
        "$BJ/lexer-cuda" --benchmark 2 --warmup 1 < "$DATA"
grep -E "Registers Per Thread|Shared Memory Per Block|Block Limit|Achieved Occupancy|Grid Size|Block Size" \
    "$OUT/launchstats-parser.txt" "$OUT/launchstats-lexer.txt" | tee -a "$OUT/summary.txt"

# --------------------------------------------- 6. full ncu profiles
# One launch each, --set full, -lineinfo binary + --import-source so the
# .ncu-rep is self-contained (openable in ncu-ui / ncu --import anywhere).
log "Full ncu profiles (.ncu-rep) — slowest step, one launch each"
run ncu-full-parser.log \
    ncu --clock-control base --set full --import-source yes \
        -k parserFusedKernel -s 1 -c 1 -f -o "$OUT/parser-full" \
        "$BJ/parser-cuda-prof" --benchmark 2 --warmup 1 < "$DATA"
run ncu-full-lexer.log \
    ncu --clock-control base --set full --import-source yes \
        -k "regex:lexer" -s 4 -c 1 -f -o "$OUT/lexer-full" \
        "$BJ/lexer-cuda-prof" --benchmark 2 --warmup 1 < "$DATA"
# Text summary of the full profiles so the bundle is readable without ncu-ui.
[ -f "$OUT/parser-full.ncu-rep" ] && \
    ncu --import "$OUT/parser-full.ncu-rep" > "$OUT/parser-full-details.txt" 2>&1
[ -f "$OUT/lexer-full.ncu-rep" ] && \
    ncu --import "$OUT/lexer-full.ncu-rep" > "$OUT/lexer-full-details.txt" 2>&1

# ------------------------------------------- 7. nvprof (or nsys fallback)
if command -v nvprof >/dev/null && [ -n "$CC_MAJOR" ] && [ "$CC_MAJOR" -lt 8 ]; then
    log "nvprof GPU trace + API summary (CC $CC < 8.0)"
    run nvprof-parser.log \
        nvprof --print-gpu-trace --log-file "$OUT/nvprof-parser-trace.txt" \
        "$BJ/parser-cuda" --benchmark 3 --warmup 1 < "$DATA"
    run nvprof-parser-summary.log \
        nvprof --log-file "$OUT/nvprof-parser-summary.txt" \
        "$BJ/parser-cuda" --benchmark 3 --warmup 1 < "$DATA"
    run nvprof-lexer-summary.log \
        nvprof --log-file "$OUT/nvprof-lexer-summary.txt" \
        "$BJ/lexer-cuda" --benchmark 3 --warmup 1 < "$DATA"
else
    if ! command -v nvprof >/dev/null; then
        echo "nvprof skipped: not installed." | tee -a "$OUT/summary.txt"
    else
        echo "nvprof skipped: unsupported on CC ${CC:-?} (Ampere+)." | tee -a "$OUT/summary.txt"
    fi
    if command -v nsys >/dev/null; then
        log "nsys timeline capture (nvprof replacement on this GPU)"
        run nsys-parser.log \
            nsys profile -t cuda,nvtx --force-overwrite true \
                 -o "$OUT/parser-timeline" \
                 "$BJ/parser-cuda" --benchmark 3 --warmup 1 < "$DATA"
        run nsys-lexer.log \
            nsys profile -t cuda,nvtx --force-overwrite true \
                 -o "$OUT/lexer-timeline" \
                 "$BJ/lexer-cuda" --benchmark 3 --warmup 1 < "$DATA"
        for rep in "$OUT"/parser-timeline.nsys-rep "$OUT"/lexer-timeline.nsys-rep; do
            [ -f "$rep" ] && nsys stats --report cuda_gpu_kern_sum,cuda_gpu_mem_time_sum \
                "$rep" > "${rep%.nsys-rep}-stats.txt" 2>&1
        done
    fi
fi

# ------------------------------------------------------------ 8. bundle
log "Summary"
cat "$OUT/summary.txt"
TARBALL="${OUT}.tar.gz"
tar czf "$TARBALL" -C "$(dirname "$OUT")" "$(basename "$OUT")"
log "Done — send back: $TARBALL ($(du -h "$TARBALL" | cut -f1))"
