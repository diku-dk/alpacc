// cli.cu — generated program entry point.
//
// Appended last (after common.cu, scan.cu, [lexer.cu], pse.cu, [parser.cu]
// and the grammar constants).  Provides a unified CLI for all three modes:
//   Lex-only:    framed test batches → token/span records
//   Parse-only:  framed token-ID batches → production IDs
//   Both:        framed raw-byte batches → lexer → fused parser with
//                parents/parse_int phases → CST nodes
//
// Wire format (native little-endian batch protocol, see
// docs/wire-protocols.md):
//   batch input : u64 num_tests, then per test a frame:
//                 u64 frame_len, u64 n, payload
//                 (payload: n raw bytes for lexer/both;
//                  n × sizeof(terminal_t) token ids for parser)
//   batch output: u64 num_tests, then per test a response record:
//                 u8 valid; if valid: u64 count + native-width fields
//                 using terminal_t / production_t / index_t as generated
//                 for the grammar; `--layout` prints their sizes.
//   --server    : a loop of such counted batches until EOF, flushed
//                 after each response record.
//
// Flags (all optional):
//   -i FILE              input file (default: stdin)
//   -o FILE              output file (default: stdout)
//   --block-size  N      128 or 256 (default: 256)
//   --items-per-thread N 2, 4, or 8 (default: auto from shared memory)
//   --shared-memory N    shared memory budget in bytes (default: device query)
//   --timeit             print kernel elapsed time to stderr
//   --server             counted-batch loop until EOF (flush after each
//                        response)
//   --layout             print native type sizes (key=value lines) and exit
//   --benchmark N        time N runs (GPU-only, pre-alloc, no I/O in loop)

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cstdint>
#include <cinttypes>
#include <vector>

// Hosts are little-endian; decode by memcpy.
static inline uint64_t decode_le64(const uint8_t* p) {
    uint64_t v;
    memcpy(&v, p, sizeof v);
    return v;
}

// ---------------------------------------------------------------------------
// Compile-time (BLOCK_SIZE, ITEMS_PER_THREAD) selection.
// ---------------------------------------------------------------------------
//
// Both are baked in at nvcc-invocation time.  Precedence for each:
//   1. Explicit -DALPACC_BLOCK_SIZE=<n> / -DALPACC_ITEMS_PER_THREAD=<n>
//      always wins.
//   2. The per-arch tuning table (alpacc_ipt_tuning<SM, ELEM_BYTES>) picks
//      a CUB-style value for the codegen-baked ALPACC_SM_ARCH.
//   3. Fallback for unknown arches or when the table entry is 0: BS=256
//      and the largest ITEMS_PER_THREAD that fits in ALPACC_SHARED_MEMORY.
// Table-picked IPT is capped by the shmem-fits max so no combination
// overflows the (possibly overridden) shmem budget.
//
// No runtime dispatch: exactly one specialised kernel is instantiated per
// binary.  To try a different tuning, rebuild with different -D flags.
#ifdef HAS_LEXER
#ifndef ALPACC_BLOCK_SIZE
constexpr uint32_t ALPACC_BLOCK_SIZE = []{
  constexpr uint32_t table_bs = arch_block_size<ALPACC_SM_ARCH, endo_t, index_t>();
  return table_bs;
}();
#endif

#ifndef ALPACC_ITEMS_PER_THREAD
constexpr uint32_t ALPACC_ITEMS_PER_THREAD = []{
  constexpr uint32_t shmem_max =
      max_items_per_thread<uint32_t, endo_t, index_t, length_t, terminal_t,
                            ALPACC_BLOCK_SIZE, ALPACC_SHARED_MEMORY>();
  constexpr uint32_t table = arch_ipt<ALPACC_SM_ARCH, endo_t, index_t>();
  // Table = 0 means unknown arch; fall back to the shmem-search maximum.
  // Otherwise clamp the table pick to what actually fits.
  return table == 0 ? shmem_max
                    : (table < shmem_max ? table : shmem_max);
}();
#endif
#else
// Parser-only builds don't have the lexer types in scope; default plainly.
#ifndef ALPACC_BLOCK_SIZE
#define ALPACC_BLOCK_SIZE 256
#endif
#endif

// ---------------------------------------------------------------------------
// Argument parsing helpers
// ---------------------------------------------------------------------------

// Native type sizes for the server protocol; consumed by test harnesses
// to build/parse native frames.
static void print_layout(void) {
    printf("terminal_t=%zu\n", sizeof(terminal_t));
#ifdef HAS_PARSER
    printf("production_t=%zu\n", sizeof(production_t));
#endif
    printf("index_t=%zu\n", sizeof(index_t));
#ifdef HAS_LEXER
    printf("shared_memory=%u\n", (unsigned)ALPACC_SHARED_MEMORY);
    printf("sm_arch=%u\n", (unsigned)ALPACC_SM_ARCH);
    printf("block_size=%u\n", (unsigned)ALPACC_BLOCK_SIZE);
    printf("items_per_thread=%u\n", (unsigned)ALPACC_ITEMS_PER_THREAD);
#endif
#ifdef HAS_PARSER
    printf("parser_block_size=%u\n", (unsigned)ALPACC_PARSER_BLOCK_SIZE);
    printf("parser_items_per_thread=%u\n", (unsigned)ALPACC_PARSER_ITEMS_PER_THREAD);
#endif
}

static void usage(const char* prog) {
    fprintf(stderr,
        "Usage: %s [options]\n"
        "  -i FILE              input file (default: stdin)\n"
        "  -o FILE              output file (default: stdout)\n"
        "  --timeit             print kernel time to stderr\n"
        "  --server             counted-batch loop until EOF (flush per response)\n"
        "  --layout             print native type sizes and exit\n"
        "  --benchmark N        time N runs (GPU-only, pre-alloc, no I/O in loop)\n"
        "  --warmup N           warmup runs before timing (default: 3)\n"
        "\n"
        "Note: BLOCK_SIZE and ITEMS_PER_THREAD are baked in at compile time.\n"
        "      Rebuild with -DALPACC_BLOCK_SIZE=<n> or -DALPACC_ITEMS_PER_THREAD=<n>\n"
        "      to change them.  Run --layout to see the current values.\n"
        , prog);
}

struct CliArgs {
    const char* input_file   = nullptr;   // null → stdin
    const char* output_file  = nullptr;   // null → stdout
    bool        timeit       = false;
    bool        server       = false;
    uint32_t    benchmark    = 0;    // 0 = off; >0 = number of timed runs
    uint32_t    warmup       = 3;    // warmup runs before timing (used when benchmark > 0)
};

static bool parse_uint32(const char* s, uint32_t* out) {
    char* end;
    unsigned long v = strtoul(s, &end, 10);
    if (*end != '\0' || v > UINT32_MAX) return false;
    *out = (uint32_t)v;
    return true;
}

static CliArgs parse_args(int argc, char* argv[]) {
    CliArgs a;
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
            usage(argv[0]); exit(0);
        } else if (strcmp(argv[i], "--timeit") == 0) {
            a.timeit = true;
        } else if (strcmp(argv[i], "--server") == 0) {
            a.server = true;
        } else if (strcmp(argv[i], "--layout") == 0) {
            print_layout(); exit(0);
        } else if (strcmp(argv[i], "-i") == 0 && i + 1 < argc) {
            a.input_file = argv[++i];
        } else if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
            a.output_file = argv[++i];
        } else if (strcmp(argv[i], "--block-size") == 0 ||
                   strcmp(argv[i], "--items-per-thread") == 0 ||
                   strcmp(argv[i], "--shared-memory") == 0) {
            // Consume the value (if any) so a stale script doesn't misparse
            // the next flag as the value, and warn: these knobs are now
            // compile-time only.
            fprintf(stderr,
                "warning: %s is compile-time only in this build; rebuild with "
                "-DALPACC_%s=<n> to change it (see --layout for current values).\n",
                argv[i],
                strcmp(argv[i], "--block-size") == 0 ? "BLOCK_SIZE" :
                strcmp(argv[i], "--items-per-thread") == 0 ? "ITEMS_PER_THREAD" :
                                                             "SHARED_MEMORY");
            if (i + 1 < argc) i++;
        } else if (strcmp(argv[i], "--benchmark") == 0 && i + 1 < argc) {
            if (!parse_uint32(argv[++i], &a.benchmark) || a.benchmark == 0) {
                fprintf(stderr, "error: --benchmark must be a positive integer\n");
                exit(1);
            }
        } else if (strcmp(argv[i], "--warmup") == 0 && i + 1 < argc) {
            if (!parse_uint32(argv[++i], &a.warmup)) {
                fprintf(stderr, "error: --warmup must be a non-negative integer\n");
                exit(1);
            }
        } else {
            fprintf(stderr, "error: unknown argument '%s'\n", argv[i]);
            usage(argv[0]); exit(1);
        }
    }
    return a;
}

// ---------------------------------------------------------------------------
// I/O helpers (reused across modes)
// ---------------------------------------------------------------------------

// Host-byte-order (little-endian) helpers for the native protocol.
static __attribute__((unused)) uint64_t read_u64_le(FILE* f) {
    uint64_t v;
    if (fread(&v, sizeof v, 1, f) != 1) return (uint64_t)-1;
    return v;
}

static __attribute__((unused)) void write_u64_le(FILE* f, uint64_t v) {
    fwrite(&v, sizeof v, 1, f);
}

template <typename T>
static inline void write_val(FILE* f, T v) { fwrite(&v, sizeof(T), 1, f); }

// Read an entire file into a heap buffer; caller frees.
static uint8_t* slurp(FILE* f, size_t* out_len) {
    size_t cap = 1 << 20, len = 0;
    uint8_t* buf = (uint8_t*)malloc(cap);
    size_t r;
    while ((r = fread(buf + len, 1, cap - len, f)) > 0) {
        len += r;
        if (len == cap) { cap *= 2; buf = (uint8_t*)realloc(buf, cap); }
    }
    *out_len = len;
    return buf;
}

// ---------------------------------------------------------------------------
// Lexer-only mode (framed test batches)
// ---------------------------------------------------------------------------

#ifdef HAS_LEXER

// Run the lexer on one framed test; on success fills toks/starts/ends and
// returns true, otherwise returns false (rejected input).
template<uint32_t BLOCK_SIZE, uint32_t ITEMS_PER_THREAD>
static bool lex_one(const uint8_t* bytes, uint64_t n,
                    std::vector<terminal_t>& toks,
                    std::vector<index_t>& starts,
                    std::vector<length_t>& lengths) {
    toks.clear(); starts.clear(); lengths.clear();
    if (n == 0) return true;

    uint8_t*    d_str = nullptr;
    terminal_t* d_tok = nullptr;
    index_t*    d_s   = nullptr;
    length_t*   d_l   = nullptr;
    gpuAssert(cudaMalloc(&d_str, n));
    gpuAssert(cudaMalloc(&d_tok, n * sizeof(terminal_t)));
    gpuAssert(cudaMalloc(&d_s,   n * sizeof(index_t)));
    gpuAssert(cudaMalloc(&d_l,   n * sizeof(length_t)));
    gpuAssert(cudaMemcpy(d_str, bytes, n, cudaMemcpyHostToDevice));

    LexerCtx<uint32_t, index_t> ctx((uint32_t)n, BLOCK_SIZE, ITEMS_PER_THREAD);
    const uint32_t nblocks = numBlocks((uint32_t)n, BLOCK_SIZE, ITEMS_PER_THREAD);
    lexer<uint32_t, index_t, BLOCK_SIZE, ITEMS_PER_THREAD>
        <<<nblocks, BLOCK_SIZE>>>(ctx, d_str, d_tok, d_s, d_l, (uint32_t)n, true);
    gpuAssert(cudaDeviceSynchronize());
    gpuAssert(cudaPeekAtLastError());
    const bool     valid   = !ctx.isOverflow() && ctx.isAccept();
    const uint32_t num_lex = ctx.terminalsSize();
    ctx.cleanUp();

    if (valid && num_lex > 0) {
        toks.resize(num_lex);
        starts.resize(num_lex);
        lengths.resize(num_lex);
        gpuAssert(cudaMemcpy(toks.data(),    d_tok, num_lex * sizeof(terminal_t), cudaMemcpyDeviceToHost));
        gpuAssert(cudaMemcpy(starts.data(),  d_s,   num_lex * sizeof(index_t),    cudaMemcpyDeviceToHost));
        gpuAssert(cudaMemcpy(lengths.data(), d_l,   num_lex * sizeof(length_t),   cudaMemcpyDeviceToHost));
    }
    cudaFree(d_str); cudaFree(d_tok); cudaFree(d_s); cudaFree(d_l);
    return valid;
}

// Response record (host byte order):
//   u8 valid; if valid: u64 num_lexemes; per lexeme:
//   terminal_t terminal, index_t span start, index_t span end
template<uint32_t BLOCK_SIZE, uint32_t ITEMS_PER_THREAD>
static void run_one_lexer_test(const uint8_t* bytes, uint64_t n, FILE* out) {
    std::vector<terminal_t> toks;
    std::vector<index_t>    starts;
    std::vector<length_t>   lengths;
    if (!lex_one<BLOCK_SIZE, ITEMS_PER_THREAD>(bytes, n, toks, starts, lengths)) {
        fputc(0, out);
        return;
    }
    fputc(1, out);
    write_u64_le(out, (uint64_t)toks.size());
    for (size_t i = 0; i < toks.size(); i++) {
        write_val(out, toks[i]);
        write_val(out, starts[i]);
        write_val(out, lengths[i]);
    }
}

// Batch mode: u64 num_tests, then per test a frame (u64 frame_len,
// u64 n, n raw bytes).  Writes u64 num_tests followed by per-test
// response records (see run_one_lexer_test).
template<uint32_t BLOCK_SIZE, uint32_t ITEMS_PER_THREAD>
static int run_lexer_batch_impl(FILE* in, FILE* out, bool timeit) {
    size_t buf_len = 0;
    uint8_t* buf = slurp(in, &buf_len);
    if (!buf || buf_len < 8) { free(buf); fprintf(stderr, "error: truncated input\n"); return 1; }

    cudaEvent_t t0, t1;
    if (timeit) {
        cudaEventCreate(&t0);
        cudaEventCreate(&t1);
    }

    const uint8_t* p   = buf;
    const uint8_t* end = buf + buf_len;
    uint64_t num_tests = decode_le64(p); p += 8;
    write_u64_le(out, num_tests);
    int ret = 0;

    for (uint64_t t = 0; t < num_tests; t++) {
        if (p + 16 > end) { fprintf(stderr, "error: truncated input\n"); ret = 1; break; }
        uint64_t frame_len = decode_le64(p); p += 8;
        uint64_t n = decode_le64(p); p += 8;
        if (frame_len != 8 + n) { fprintf(stderr, "error: frame length mismatch\n"); ret = 1; break; }
        if (n > (uint64_t)(end - p)) { fprintf(stderr, "error: truncated input\n"); ret = 1; break; }
        if (timeit) cudaEventRecord(t0);
        run_one_lexer_test<BLOCK_SIZE, ITEMS_PER_THREAD>(p, n, out);
        if (timeit) {
            cudaEventRecord(t1);
            cudaEventSynchronize(t1);
            float ms = 0;
            cudaEventElapsedTime(&ms, t0, t1);
            fprintf(stderr, "input %zu (%zu bytes): %.2fms\n", (size_t)t, (size_t)n, ms);
        }
        p += n;
    }
    free(buf);
    fflush(out);

    if (timeit) {
        cudaEventDestroy(t0);
        cudaEventDestroy(t1);
    }
    return ret;
}

// Server mode: counted batches (u64 num_tests, then that many frames) in
// a loop until EOF, flush after each response.
// Frame: u64 frame_len; content: u64 n + n raw bytes.
template<uint32_t BLOCK_SIZE, uint32_t ITEMS_PER_THREAD>
static int lexer_server_impl(FILE* in, FILE* out) {
    std::vector<uint8_t> frame;
    for (;;) {
        uint64_t num_tests = read_u64_le(in);
        if (feof(in)) break;
        if (num_tests == (uint64_t)-1) {
            fprintf(stderr, "error: truncated input\n"); return 1;
        }
        write_u64_le(out, num_tests);
        fflush(out);
        for (uint64_t t = 0; t < num_tests; t++) {
            uint64_t frame_len = read_u64_le(in);
            if (feof(in) || frame_len == (uint64_t)-1 || frame_len < 8) {
                fprintf(stderr, "error: bad frame length\n"); return 1;
            }
            frame.resize(frame_len);
            if (fread(frame.data(), 1, frame_len, in) != frame_len) {
                fprintf(stderr, "error: truncated frame\n"); return 1;
            }
            uint64_t n;
            memcpy(&n, frame.data(), sizeof n);
            if (frame_len != 8 + n) {
                fprintf(stderr, "error: frame length mismatch\n"); return 1;
            }
            run_one_lexer_test<BLOCK_SIZE, ITEMS_PER_THREAD>(frame.data() + 8, n, out);
            fflush(out);
        }
    }
    return 0;
}

template<uint32_t BLOCK_SIZE, uint32_t ITEMS_PER_THREAD>
static int lexer_benchmark(FILE* in, uint32_t warmup_runs, uint32_t n_runs) {
    size_t buf_len = 0;
    uint8_t* buf = slurp(in, &buf_len);
    if (!buf || buf_len < 16) {
        free(buf); fprintf(stderr, "error: benchmark input too short\n"); return 1;
    }
    const uint8_t* p   = buf;
    const uint8_t* end = buf + buf_len;
    uint64_t num_tests = decode_le64(p); p += 8;

    cudaEvent_t ev0, ev1;
    gpuAssert(cudaEventCreate(&ev0));
    gpuAssert(cudaEventCreate(&ev1));

    for (uint64_t t = 0; t < num_tests; t++) {
        if (p + 16 > end) { free(buf); fprintf(stderr, "error: truncated input\n"); return 1; }
        uint64_t frame_len = decode_le64(p); p += 8;
        uint64_t n         = decode_le64(p); p += 8;
        if (frame_len != 8 + n || (uint64_t)(end - p) < n) {
            free(buf); fprintf(stderr, "error: input truncated\n"); return 1;
        }
        const uint8_t* data = p;
        p += n;

        uint8_t*    d_str = nullptr;
        terminal_t* d_tok = nullptr;
        index_t*    d_s   = nullptr;
        length_t*   d_l   = nullptr;
        gpuAssert(cudaMalloc(&d_str, n ? n : 1));
        gpuAssert(cudaMalloc(&d_tok, n * sizeof(terminal_t)));
        gpuAssert(cudaMalloc(&d_s,   n * sizeof(index_t)));
        gpuAssert(cudaMalloc(&d_l,   n * sizeof(length_t)));
        gpuAssert(cudaMemcpy(d_str, data, n, cudaMemcpyHostToDevice));

        std::vector<terminal_t> h_tok(n);

        const uint32_t nblocks = numBlocks((uint32_t)n, BLOCK_SIZE, ITEMS_PER_THREAD);
        LexerCtx<uint32_t, index_t> ctx((uint32_t)n, BLOCK_SIZE, ITEMS_PER_THREAD);

        uint32_t num_warmup = warmup_runs > 0 ? warmup_runs : 1;
        for (uint32_t i = 0; i < num_warmup; i++) {
            ctx.reset();
            lexer<uint32_t, index_t, BLOCK_SIZE, ITEMS_PER_THREAD>
                <<<nblocks, BLOCK_SIZE>>>(ctx, d_str, d_tok, d_s, d_l, (uint32_t)n, true);
            gpuAssert(cudaDeviceSynchronize());
        }
        // Token count is stable for a fixed input; read after warmup.
        uint32_t num_tok = ctx.terminalsSize();

        // --- kernel-only timing ---
        std::vector<float> times_ms(n_runs);
        for (uint32_t i = 0; i < n_runs; i++) {
            ctx.reset();
            gpuAssert(cudaEventRecord(ev0));
            lexer<uint32_t, index_t, BLOCK_SIZE, ITEMS_PER_THREAD>
                <<<nblocks, BLOCK_SIZE>>>(ctx, d_str, d_tok, d_s, d_l, (uint32_t)n, true);
            gpuAssert(cudaEventRecord(ev1));
            gpuAssert(cudaEventSynchronize(ev1));
            gpuAssert(cudaEventElapsedTime(&times_ms[i], ev0, ev1));
        }

        // --- IO-bound timing (H->D + kernel + D->H) ---
        std::vector<float> times_io_ms(n_runs);
        for (uint32_t i = 0; i < n_runs; i++) {
            ctx.reset();
            gpuAssert(cudaEventRecord(ev0));
            gpuAssert(cudaMemcpyAsync(d_str, data, n, cudaMemcpyHostToDevice));
            lexer<uint32_t, index_t, BLOCK_SIZE, ITEMS_PER_THREAD>
                <<<nblocks, BLOCK_SIZE>>>(ctx, d_str, d_tok, d_s, d_l, (uint32_t)n, true);
            gpuAssert(cudaMemcpyAsync(h_tok.data(), d_tok, n * sizeof(terminal_t), cudaMemcpyDeviceToHost));
            gpuAssert(cudaEventRecord(ev1));
            gpuAssert(cudaEventSynchronize(ev1));
            gpuAssert(cudaEventElapsedTime(&times_io_ms[i], ev0, ev1));
        }

        ctx.cleanUp();
        cudaFree(d_str); cudaFree(d_tok); cudaFree(d_s); cudaFree(d_l);

        // Total estimated DRAM traffic per kernel run:
        //   reads:  n bytes (d_string)
        //   writes: num_tok × (terminal_t + index_t + length_t) bytes
        //   scan lookback arrays: negligible vs payload
        uint32_t num_tiles = numBlocks((uint32_t)n, BLOCK_SIZE, ITEMS_PER_THREAD);
        size_t scan_bytes = 2 * (size_t)num_tiles *
            (sizeof(endo_t) + sizeof(endo_t) +
             sizeof(uint64_t) + sizeof(uint64_t) +
             2);
        size_t dram_bytes = n
            + (size_t)num_tok * (sizeof(terminal_t) + sizeof(index_t) + sizeof(length_t))
            + scan_bytes;

        auto print_stats = [&](const char* label, std::vector<float>& tms, size_t traffic) {
            double mean = 0, variance = 0, gbps = 0;
            double factor = (double)traffic / (1000.0 * n_runs);
            for (uint32_t i = 0; i < n_runs; i++) {
                double diff = fmax(1e3 * (double)tms[i], 0.5);
                mean     += diff / n_runs;
                variance += (diff * diff) / n_runs;
                gbps     += factor / diff;
            }
            double bound = (0.95 * sqrt(variance)) / sqrt((double)n_runs);
            fprintf(stderr, "%s:\n", label);
            fprintf(stderr, "        %.0fμs (95%% CI: [%.1fμs, %.1fμs]); %.0fGB/s (%.0fGB/s input-only)\n",
                    mean, mean - bound, mean + bound,
                    gbps,
                    gbps * (double)n / (double)traffic);
        };

        char label[64];
        snprintf(label, sizeof(label), "lex_int (cuda, %zu bytes, %u tokens)", (size_t)n, num_tok);
        print_stats(label, times_ms, dram_bytes);
        snprintf(label, sizeof(label), "lex_int (cuda+io, %zu bytes, %u tokens)", (size_t)n, num_tok);
        print_stats(label, times_io_ms, dram_bytes);
    }

    gpuAssert(cudaEventDestroy(ev0));
    gpuAssert(cudaEventDestroy(ev1));
    free(buf);
    return 0;
}

#endif // HAS_LEXER

// ---------------------------------------------------------------------------
// Parser-only mode (framed token-ID batches)
//
// Frame payload: n × terminal_t token ids (host byte order).
// Response record: u8 valid; if valid: u64 np + np×production_t prod ids.
// ---------------------------------------------------------------------------

#if defined(HAS_PARSER) && !defined(HAS_LEXER)

static void run_one_parser_test(const terminal_t* tokens, uint64_t n,
                                FILE* out) {
    std::vector<production_t> prods;
    bool ok = runParserPipeline(tokens, n, prods);
    if (ok) {
        fputc(1, out);
        write_u64_le(out, (uint64_t)prods.size());
        if (!prods.empty())
            fwrite(prods.data(), sizeof(production_t), prods.size(), out);
    } else {
        fputc(0, out);
    }
}

// Bulk-read the entire input into memory, then decode, to avoid per-token fread overhead.
static int parser_batch(FILE* in, FILE* out) {
    size_t buf_len = 0;
    uint8_t* buf = slurp(in, &buf_len);
    if (!buf || buf_len < 8) {
        free(buf);
        fprintf(stderr, "error: truncated input\n"); return 1;
    }
    const uint8_t* p   = buf;
    const uint8_t* end = buf + buf_len;

    uint64_t num_tests = decode_le64(p); p += 8;
    write_u64_le(out, num_tests);

    std::vector<terminal_t> tokens;
    for (uint64_t t = 0; t < num_tests; t++) {
        if (p + 16 > end) { free(buf); fprintf(stderr, "truncated\n"); return 1; }
        uint64_t frame_len = decode_le64(p); p += 8;
        uint64_t n = decode_le64(p); p += 8;
        if (frame_len != 8 + n * sizeof(terminal_t)) {
            free(buf); fprintf(stderr, "error: frame length mismatch\n"); return 1;
        }
        if (p + n * sizeof(terminal_t) > end) { free(buf); fprintf(stderr, "truncated\n"); return 1; }
        tokens.resize(n);
        if (n > 0)
            memcpy(tokens.data(), p, n * sizeof(terminal_t));
        p += n * sizeof(terminal_t);
        run_one_parser_test(tokens.data(), n, out);
    }
    free(buf);
    fflush(out);
    return 0;
}

// Server mode: counted batches (u64 num_tests, then that many frames) in
// a loop until EOF, flush after each response.  Each frame: u64
// frame_byte_length, then u64 n + n×terminal_t token ids.
static int parser_server(FILE* in, FILE* out) {
    std::vector<terminal_t> tokens;
    std::vector<uint8_t>    frame;
    for (;;) {
        uint64_t num_tests = read_u64_le(in);
        if (feof(in)) break;
        if (num_tests == (uint64_t)-1) {
            fprintf(stderr, "error: truncated input\n"); return 1;
        }
        write_u64_le(out, num_tests);
        fflush(out);
        for (uint64_t t = 0; t < num_tests; t++) {
            uint64_t frame_len = read_u64_le(in);
            if (feof(in) || frame_len == (uint64_t)-1 || frame_len < 8) {
                fprintf(stderr, "error: bad frame length\n"); return 1;
            }
            frame.resize(frame_len);
            if (fread(frame.data(), 1, frame_len, in) != frame_len) {
                fprintf(stderr, "error: truncated frame\n"); return 1;
            }
            uint64_t n;
            memcpy(&n, frame.data(), sizeof n);
            if (frame_len != 8 + n * sizeof(terminal_t)) {
                fprintf(stderr, "error: frame length mismatch\n"); return 1;
            }
            tokens.resize(n);
            if (n > 0)
                memcpy(tokens.data(), frame.data() + 8, n * sizeof(terminal_t));
            run_one_parser_test(tokens.data(), n, out);
            fflush(out);
        }
    }
    return 0;
}

#endif /* HAS_PARSER && !HAS_LEXER */

#if defined(HAS_PARSER) && !defined(HAS_LEXER)
// Benchmark mode: read all tests from `in`, pre-allocate GPU buffers once,
// run warmup passes, then time `n_runs` passes with CUDA events.
static int parser_benchmark(FILE* in, uint32_t warmup_runs, uint32_t n_runs) {
    size_t buf_len = 0;
    uint8_t* buf = slurp(in, &buf_len);
    if (!buf || buf_len < 16) {
        free(buf);
        fprintf(stderr, "error: benchmark input too short\n");
        return 1;
    }
    const uint8_t* p   = buf;
    const uint8_t* end = buf + buf_len;
    uint64_t num_tests = decode_le64(p); p += 8;

    cudaEvent_t ev0, ev1;
    gpuAssert(cudaEventCreate(&ev0));
    gpuAssert(cudaEventCreate(&ev1));

    for (uint64_t t = 0; t < num_tests; t++) {
        if (p + 16 > end) { free(buf); fprintf(stderr, "error: truncated input\n"); return 1; }
        uint64_t frame_len = decode_le64(p); p += 8;
        uint64_t n         = decode_le64(p); p += 8;
        if (frame_len != 8 + n * sizeof(terminal_t) ||
            (uint64_t)(end - p) < n * sizeof(terminal_t)) {
            free(buf); fprintf(stderr, "error: input truncated\n"); return 1;
        }

        index_t ni = (index_t)n;
        index_t m  = ni + (index_t)2;

        std::vector<terminal_t> h_arr((size_t)m);
        h_arr[0] = START_TERMINAL;
        if (ni > 0)
            memcpy(h_arr.data() + 1, p, (size_t)ni * sizeof(terminal_t));
        h_arr[(size_t)m - 1] = END_TERMINAL;
        p += n * sizeof(terminal_t);

        if ((uint64_t)m * (uint64_t)MAX_BRACKETS_PER_POSITION > (uint64_t)INT_MAX) {
            fprintf(stderr, "error: bracket capacity bound exceeds INT_MAX\n");
            free(buf); return 1;
        }

        ParserFused pre = allocParserFused(m);
        gpuAssert(cudaMemcpy(pre.d_arr, h_arr.data(),
                             (size_t)m * sizeof(terminal_t), cudaMemcpyHostToDevice));

        std::vector<production_t> dummy_prods;
        for (uint32_t i = 0; i < warmup_runs; i++)
            runParserFused(pre, h_arr.data(), m, dummy_prods);

        // --- kernel-only timing ---
        std::vector<float> times_ms(n_runs);
        for (uint32_t i = 0; i < n_runs; i++) {
            int one = 1;
            gpuAssert(cudaMemcpy(pre.bufs.d_valid, &one, sizeof(int), cudaMemcpyHostToDevice));
            gpuAssert(cudaEventRecord(ev0));
            launchParserFused(pre, m);
            gpuAssert(cudaEventRecord(ev1));
            gpuAssert(cudaEventSynchronize(ev1));
            gpuAssert(cudaEventElapsedTime(&times_ms[i], ev0, ev1));
        }

        // --- IO-bound timing (H->D + kernel + D->H) ---
        index_t totals[2];
        std::vector<production_t> h_prods((size_t)pre.max_pr);
        std::vector<float> times_io_ms(n_runs);
        for (uint32_t i = 0; i < n_runs; i++) {
            int one = 1;
            gpuAssert(cudaMemcpy(pre.bufs.d_valid, &one, sizeof(int), cudaMemcpyHostToDevice));
            gpuAssert(cudaEventRecord(ev0));
            gpuAssert(cudaMemcpyAsync(pre.d_arr, h_arr.data(),
                                      (size_t)m * sizeof(terminal_t), cudaMemcpyHostToDevice));
            launchParserFused(pre, m);
            gpuAssert(cudaMemcpyAsync(totals, pre.bufs.d_totals,
                                      2 * sizeof(index_t), cudaMemcpyDeviceToHost));
            gpuAssert(cudaMemcpyAsync(h_prods.data(), pre.bufs.d_productions,
                                      (size_t)pre.max_pr * sizeof(production_t), cudaMemcpyDeviceToHost));
            gpuAssert(cudaEventRecord(ev1));
            gpuAssert(cudaEventSynchronize(ev1));
            gpuAssert(cudaEventElapsedTime(&times_io_ms[i], ev0, ev1));
        }

        freeParserFused(pre);

        size_t bytes = (size_t)ni * sizeof(terminal_t);
        auto print_stats = [&](const char* label, std::vector<float>& tms) {
            double mean = 0, variance = 0, gbps = 0;
            double factor = (double)bytes / (1000.0 * n_runs);
            for (uint32_t i = 0; i < n_runs; i++) {
                double diff = fmax(1e3 * (double)tms[i], 0.5);
                mean     += diff / n_runs;
                variance += (diff * diff) / n_runs;
                gbps     += factor / diff;
            }
            double bound = (0.95 * sqrt(variance)) / sqrt((double)n_runs);
            fprintf(stderr, "%s:\n", label);
            fprintf(stderr, "        %.0fμs (95%% CI: [%.1fμs, %.1fμs]); %.0fGB/s\n",
                    mean, mean - bound, mean + bound, gbps);
        };

        char label[64];
        snprintf(label, sizeof(label), "parse_int (cuda, %zu tokens)", (size_t)ni);
        print_stats(label, times_ms);
        snprintf(label, sizeof(label), "parse_int (cuda+io, %zu tokens)", (size_t)ni);
        print_stats(label, times_io_ms);
    }

    gpuAssert(cudaEventDestroy(ev0));
    gpuAssert(cudaEventDestroy(ev1));
    free(buf);
    return 0;
}

#endif /* HAS_PARSER && !HAS_LEXER */

// ---------------------------------------------------------------------------
// Both mode: full pipeline (raw bytes → lexer → fused parser → compact tree)
//
// Frame payload: n raw bytes.  Response record (SoA sections): u8 valid;
// if valid: u64 num_tokens; num_tokens × terminal_t token ids;
// num_tokens × index_t starts; num_tokens × length_t lengths; u64 num_nodes;
// num_nodes × production_t production ids; num_nodes × index_t parents;
// num_tokens × index_t token parents.
// Same format as the generated C backend's combined mode.
// ---------------------------------------------------------------------------

#if defined(HAS_LEXER) && defined(HAS_PARSER)

struct BothResult {
    std::vector<terminal_t> toks;
    std::vector<index_t>    starts;
    std::vector<length_t>   lengths;
    BothTree                tree;
};

// Run the full pipeline on one test; fills `res` and returns validity.
template<uint32_t BLOCK_SIZE, uint32_t ITEMS_PER_THREAD>
static bool both_one(const uint8_t* bytes, uint64_t n, BothResult& res) {
    bool valid = true;
    uint32_t num_lex = 0;
    uint8_t*    d_string    = nullptr;
    terminal_t* d_terminals = nullptr;
    index_t*    d_starts    = nullptr;
    length_t*   d_lengths   = nullptr;

    if (n > 0) {
        gpuAssert(cudaMalloc(&d_string,    n * sizeof(uint8_t)));
        gpuAssert(cudaMalloc(&d_terminals, n * sizeof(terminal_t)));
        gpuAssert(cudaMalloc(&d_starts,    n * sizeof(index_t)));
        gpuAssert(cudaMalloc(&d_lengths,   n * sizeof(length_t)));
        gpuAssert(cudaMemcpy(d_string, bytes, n, cudaMemcpyHostToDevice));

        // Single-chunk launch on a fresh context (no streaming carry-over).
        LexerCtx<uint32_t, index_t> ctx((uint32_t)n, BLOCK_SIZE, ITEMS_PER_THREAD);
        const uint32_t num_blocks = numBlocks((uint32_t)n, BLOCK_SIZE, ITEMS_PER_THREAD);
        lexer<uint32_t, index_t, BLOCK_SIZE, ITEMS_PER_THREAD><<<num_blocks, BLOCK_SIZE>>>(
            ctx, d_string, d_terminals, d_starts, d_lengths, (uint32_t)n, true);
        gpuAssert(cudaDeviceSynchronize());
        gpuAssert(cudaPeekAtLastError());
        num_lex = ctx.terminalsSize();
        valid   = !ctx.isOverflow() && ctx.isAccept();
        ctx.cleanUp();
    }

    if (valid && num_lex > 0) {
        res.toks.resize(num_lex);
        res.starts.resize(num_lex);
        res.lengths.resize(num_lex);
        gpuAssert(cudaMemcpy(res.toks.data(),    d_terminals, num_lex * sizeof(terminal_t), cudaMemcpyDeviceToHost));
        gpuAssert(cudaMemcpy(res.starts.data(),  d_starts,    num_lex * sizeof(index_t),    cudaMemcpyDeviceToHost));
        gpuAssert(cudaMemcpy(res.lengths.data(), d_lengths,   num_lex * sizeof(length_t),   cudaMemcpyDeviceToHost));
    }

    if (valid) {
        index_t m = (index_t)num_lex + (index_t)2;
        if ((uint64_t)m * (uint64_t)MAX_BRACKETS_PER_POSITION > (uint64_t)INT_MAX ||
            (uint64_t)m * (uint64_t)MAX_PRODS_PER_POSITION    > (uint64_t)INT_MAX) {
            valid = false;
        } else {
            ParserFused p = allocParserFused(m);
            valid = runBothFused(p, d_terminals, (index_t)num_lex, res.tree);
            freeParserFused(p);
        }
    }

    if (d_string)    cudaFree(d_string);
    if (d_terminals) cudaFree(d_terminals);
    if (d_starts)    cudaFree(d_starts);
    if (d_lengths)   cudaFree(d_lengths);
    return valid;
}

template<uint32_t BLOCK_SIZE, uint32_t ITEMS_PER_THREAD>
static void run_one_both_test(const uint8_t* bytes, uint64_t n, FILE* out) {
    BothResult res;
    if (!both_one<BLOCK_SIZE, ITEMS_PER_THREAD>(bytes, n, res)) {
        fputc(0, out);
        return;
    }
    fputc(1, out);
    write_u64_le(out, (uint64_t)res.toks.size());
    fwrite(res.toks.data(),     sizeof(terminal_t), res.toks.size(),     out);
    fwrite(res.starts.data(),   sizeof(index_t),    res.starts.size(),   out);
    fwrite(res.lengths.data(),  sizeof(length_t),   res.lengths.size(),  out);
    write_u64_le(out, (uint64_t)res.tree.prods.size());
    fwrite(res.tree.prods.data(),         sizeof(production_t), res.tree.prods.size(),         out);
    fwrite(res.tree.parents.data(),       sizeof(index_t),      res.tree.parents.size(),       out);
    fwrite(res.tree.token_parents.data(), sizeof(index_t),      res.tree.token_parents.size(), out);
}

template<uint32_t BLOCK_SIZE, uint32_t ITEMS_PER_THREAD>
static int both_batch_impl(FILE* in, FILE* out) {
    size_t buf_len = 0;
    uint8_t* buf = slurp(in, &buf_len);
    if (!buf || buf_len < 8) {
        free(buf);
        fprintf(stderr, "error: truncated input\n"); return 1;
    }
    const uint8_t* p   = buf;
    const uint8_t* end = buf + buf_len;

    uint64_t num_tests = decode_le64(p); p += 8;
    write_u64_le(out, num_tests);

    for (uint64_t t = 0; t < num_tests; t++) {
        if (p + 16 > end) { free(buf); fprintf(stderr, "truncated\n"); return 1; }
        uint64_t frame_len = decode_le64(p); p += 8;
        uint64_t n = decode_le64(p); p += 8;
        if (frame_len != 8 + n) {
            free(buf); fprintf(stderr, "error: frame length mismatch\n"); return 1;
        }
        if (n > (uint64_t)(end - p)) { free(buf); fprintf(stderr, "truncated\n"); return 1; }
        run_one_both_test<BLOCK_SIZE, ITEMS_PER_THREAD>(p, n, out);
        p += n;
    }
    free(buf);
    fflush(out);
    return 0;
}

// Server mode for the full pipeline: counted batches (u64 num_tests, then
// that many raw-byte frames) in a loop until EOF, flush after each response.
// Frame: u64 frame_len; content: u64 n + n raw bytes.
template<uint32_t BLOCK_SIZE, uint32_t ITEMS_PER_THREAD>
static int both_server_impl(FILE* in, FILE* out) {
    std::vector<uint8_t> frame;
    for (;;) {
        uint64_t num_tests = read_u64_le(in);
        if (feof(in)) break;
        if (num_tests == (uint64_t)-1) {
            fprintf(stderr, "error: truncated input\n"); return 1;
        }
        write_u64_le(out, num_tests);
        fflush(out);
        for (uint64_t t = 0; t < num_tests; t++) {
            uint64_t frame_len = read_u64_le(in);
            if (feof(in) || frame_len == (uint64_t)-1 || frame_len < 8) {
                fprintf(stderr, "error: bad frame length\n"); return 1;
            }
            frame.resize(frame_len);
            if (fread(frame.data(), 1, frame_len, in) != frame_len) {
                fprintf(stderr, "error: truncated frame\n"); return 1;
            }
            uint64_t n;
            memcpy(&n, frame.data(), sizeof n);
            if (frame_len != 8 + n) {
                fprintf(stderr, "error: frame length mismatch\n"); return 1;
            }
            run_one_both_test<BLOCK_SIZE, ITEMS_PER_THREAD>(frame.data() + 8, n, out);
            fflush(out);
        }
    }
    return 0;
}

// Benchmark for the full pipeline: each timed run is lexer kernel → D2D
// token staging → fused parser kernel with the compact-tree phases enabled
// (with_parents), i.e. exactly what parse_int measures in the Futhark
// backend.  All buffers are allocated once per test, outside the timed
// region; the "+io" variant additionally times the input H2D copy and the
// result D2H copies (tokens, spans, tree, token parents).
template<uint32_t BLOCK_SIZE, uint32_t ITEMS_PER_THREAD>
static int both_benchmark(FILE* in, uint32_t warmup_runs, uint32_t n_runs) {
    size_t buf_len = 0;
    uint8_t* buf = slurp(in, &buf_len);
    if (!buf || buf_len < 16) {
        free(buf); fprintf(stderr, "error: benchmark input too short\n"); return 1;
    }
    const uint8_t* p   = buf;
    const uint8_t* end = buf + buf_len;
    uint64_t num_tests = decode_le64(p); p += 8;

    cudaEvent_t ev0, ev1;
    gpuAssert(cudaEventCreate(&ev0));
    gpuAssert(cudaEventCreate(&ev1));

    int ret = 0;
    for (uint64_t t = 0; t < num_tests; t++) {
        if (p + 16 > end) { free(buf); fprintf(stderr, "error: truncated input\n"); return 1; }
        uint64_t frame_len = decode_le64(p); p += 8;
        uint64_t n         = decode_le64(p); p += 8;
        if (frame_len != 8 + n || (uint64_t)(end - p) < n) {
            free(buf); fprintf(stderr, "error: input truncated\n"); return 1;
        }
        const uint8_t* data = p;
        p += n;
        if (n == 0) { fprintf(stderr, "warning: skipping empty test\n"); continue; }

        // --- Lexer buffers + discovery run (num_lexemes, lexability) ---
        uint8_t*    d_string    = nullptr;
        terminal_t* d_terminals = nullptr;
        index_t*    d_starts    = nullptr;
        length_t*   d_lengths   = nullptr;
        gpuAssert(cudaMalloc(&d_string,    n * sizeof(uint8_t)));
        gpuAssert(cudaMalloc(&d_terminals, n * sizeof(terminal_t)));
        gpuAssert(cudaMalloc(&d_starts,    n * sizeof(index_t)));
        gpuAssert(cudaMalloc(&d_lengths,   n * sizeof(length_t)));
        gpuAssert(cudaMemcpy(d_string, data, n, cudaMemcpyHostToDevice));

        const uint32_t nblocks = numBlocks((uint32_t)n, BLOCK_SIZE, ITEMS_PER_THREAD);
        LexerCtx<uint32_t, index_t> ctx((uint32_t)n, BLOCK_SIZE, ITEMS_PER_THREAD);

        lexer<uint32_t, index_t, BLOCK_SIZE, ITEMS_PER_THREAD>
            <<<nblocks, BLOCK_SIZE>>>(ctx, d_string, d_terminals, d_starts, d_lengths, (uint32_t)n, true);
        gpuAssert(cudaDeviceSynchronize());
        gpuAssert(cudaPeekAtLastError());
        uint32_t num_lex   = ctx.terminalsSize();
        bool     overflow  = ctx.isOverflow();
        bool     dfa_ok    = ctx.isAccept();

        index_t m = (index_t)num_lex + (index_t)2;
        bool capacity_ok =
            (uint64_t)m * (uint64_t)MAX_BRACKETS_PER_POSITION <= (uint64_t)INT_MAX &&
            (uint64_t)m * (uint64_t)MAX_PRODS_PER_POSITION    <= (uint64_t)INT_MAX;
        if (overflow || !dfa_ok || !capacity_ok) {
            if (overflow)
                fprintf(stderr, "error: benchmark input is not lexable (token length overflow)\n");
            else if (!dfa_ok)
                fprintf(stderr, "error: benchmark input is not lexable (DFA not in accept state)\n");
            else
                fprintf(stderr, "error: benchmark input exceeds parser capacity\n");
            ctx.cleanUp();
            cudaFree(d_string); cudaFree(d_terminals); cudaFree(d_starts); cudaFree(d_lengths);
            ret = 1;
            continue;
        }

        // --- Parser buffers; sentinels staged once ---
        ParserFused pf = allocParserFused(m);
        pf.bufs.with_parents = true;
        pf.bufs.num_lexemes  = (index_t)num_lex;
        terminal_t sent = START_TERMINAL;
        gpuAssert(cudaMemcpy(pf.d_arr, &sent, sizeof(terminal_t), cudaMemcpyHostToDevice));
        sent = END_TERMINAL;
        gpuAssert(cudaMemcpy(pf.d_arr + (size_t)(m - 1), &sent, sizeof(terminal_t), cudaMemcpyHostToDevice));

        const int one = 1;
        auto run_pipeline = [&]() {
            ctx.reset();
            lexer<uint32_t, index_t, BLOCK_SIZE, ITEMS_PER_THREAD>
                <<<nblocks, BLOCK_SIZE>>>(ctx, d_string, d_terminals, d_starts, d_lengths, (uint32_t)n, true);
            gpuAssert(cudaMemcpyAsync(pf.d_arr + 1, d_terminals,
                                      (size_t)num_lex * sizeof(terminal_t), cudaMemcpyDeviceToDevice));
            launchParserFused(pf, m);
        };

        // At least one untimed run: warmup, and d_totals must be populated
        // before the result sizes are read below.
        for (uint32_t i = 0; i < (warmup_runs > 0 ? warmup_runs : 1); i++) {
            gpuAssert(cudaMemcpy(pf.bufs.d_valid, &one, sizeof(int), cudaMemcpyHostToDevice));
            run_pipeline();
            gpuAssert(cudaDeviceSynchronize());
        }

        // Result sizes for the +io D2H copies (fixed for a fixed input).
        index_t totals[2];
        gpuAssert(cudaMemcpy(totals, pf.bufs.d_totals, 2 * sizeof(index_t), cudaMemcpyDeviceToHost));
        const index_t nt = totals[1] - (index_t)num_lex;
        std::vector<terminal_t>   h_toks(num_lex);
        std::vector<index_t>      h_starts(num_lex), h_tparents(num_lex);
        std::vector<length_t>     h_lengths(num_lex);
        std::vector<production_t> h_prods((size_t)nt);
        std::vector<index_t>      h_parents((size_t)nt);

        // --- kernel-only timing ---
        std::vector<float> times_ms(n_runs);
        for (uint32_t i = 0; i < n_runs; i++) {
            gpuAssert(cudaMemcpy(pf.bufs.d_valid, &one, sizeof(int), cudaMemcpyHostToDevice));
            gpuAssert(cudaEventRecord(ev0));
            run_pipeline();
            gpuAssert(cudaEventRecord(ev1));
            gpuAssert(cudaEventSynchronize(ev1));
            gpuAssert(cudaEventElapsedTime(&times_ms[i], ev0, ev1));
        }

        // --- IO-bound timing (H->D + kernels + D->H) ---
        std::vector<float> times_io_ms(n_runs);
        for (uint32_t i = 0; i < n_runs; i++) {
            gpuAssert(cudaMemcpy(pf.bufs.d_valid, &one, sizeof(int), cudaMemcpyHostToDevice));
            gpuAssert(cudaEventRecord(ev0));
            gpuAssert(cudaMemcpyAsync(d_string, data, n, cudaMemcpyHostToDevice));
            run_pipeline();
            gpuAssert(cudaMemcpyAsync(h_toks.data(),    d_terminals, num_lex * sizeof(terminal_t), cudaMemcpyDeviceToHost));
            gpuAssert(cudaMemcpyAsync(h_starts.data(),  d_starts,    num_lex * sizeof(index_t),    cudaMemcpyDeviceToHost));
            gpuAssert(cudaMemcpyAsync(h_lengths.data(), d_lengths,   num_lex * sizeof(length_t),   cudaMemcpyDeviceToHost));
            if (nt > (index_t)0) {
                gpuAssert(cudaMemcpyAsync(h_prods.data(),   pf.bufs.d_tree_prods,
                                          (size_t)nt * sizeof(production_t), cudaMemcpyDeviceToHost));
                gpuAssert(cudaMemcpyAsync(h_parents.data(), pf.bufs.d_tree_parents,
                                          (size_t)nt * sizeof(index_t), cudaMemcpyDeviceToHost));
            }
            gpuAssert(cudaMemcpyAsync(h_tparents.data(), pf.bufs.d_token_parents,
                                      num_lex * sizeof(index_t), cudaMemcpyDeviceToHost));
            gpuAssert(cudaEventRecord(ev1));
            gpuAssert(cudaEventSynchronize(ev1));
            gpuAssert(cudaEventElapsedTime(&times_io_ms[i], ev0, ev1));
        }

        // Timed runs force d_valid = 1 before launch; check the last run
        // really produced a valid parse so garbage inputs cannot masquerade
        // as benchmark results.
        int h_valid = 0;
        gpuAssert(cudaMemcpy(&h_valid, pf.bufs.d_valid, sizeof(int), cudaMemcpyDeviceToHost));
        if (!h_valid) {
            fprintf(stderr, "error: benchmark input does not parse\n");
            ret = 1;
        }

        freeParserFused(pf);
        ctx.cleanUp();
        cudaFree(d_string); cudaFree(d_terminals); cudaFree(d_starts); cudaFree(d_lengths);

        auto print_stats = [&](const char* label, std::vector<float>& tms) {
            double mean = 0, variance = 0, gbps = 0;
            double factor = (double)n / (1000.0 * n_runs);
            for (uint32_t i = 0; i < n_runs; i++) {
                double diff = fmax(1e3 * (double)tms[i], 0.5);
                mean     += diff / n_runs;
                variance += (diff * diff) / n_runs;
                gbps     += factor / diff;
            }
            double bound = (0.95 * sqrt(variance)) / sqrt((double)n_runs);
            fprintf(stderr, "%s:\n", label);
            fprintf(stderr, "        %.0fμs (95%% CI: [%.1fμs, %.1fμs]); %.0fGB/s\n",
                    mean, mean - bound, mean + bound, gbps);
        };

        char label[64];
        snprintf(label, sizeof(label), "parse_int (cuda, %zu bytes)", (size_t)n);
        print_stats(label, times_ms);
        snprintf(label, sizeof(label), "parse_int (cuda+io, %zu bytes)", (size_t)n);
        print_stats(label, times_io_ms);
    }

    gpuAssert(cudaEventDestroy(ev0));
    gpuAssert(cudaEventDestroy(ev1));
    free(buf);
    return ret;
}

#endif // HAS_LEXER && HAS_PARSER

// ---------------------------------------------------------------------------
// main
// ---------------------------------------------------------------------------

int main(int argc, char* argv[]) {
    CliArgs a = parse_args(argc, argv);

    // Open files
    FILE* in  = stdin;
    FILE* out = stdout;
    if (a.input_file)  { in  = fopen(a.input_file,  "rb"); if (!in)  { perror(a.input_file);  return 1; } }
    if (a.output_file) { out = fopen(a.output_file, "wb"); if (!out) { perror(a.output_file); return 1; } }

    // Resolve shared memory budget (only used for --items-per-thread=auto in
    // Compile-time (BLOCK_SIZE, ITEMS_PER_THREAD) — exactly one specialised
    // kernel instantiation per binary.  Change either by rebuilding with
    // -DALPACC_BLOCK_SIZE=<n> / -DALPACC_ITEMS_PER_THREAD=<n>.

#if defined(HAS_LEXER) && !defined(HAS_PARSER)
    // ---- Lex-only mode ----
    int ret;
    if (a.benchmark > 0)
        ret = lexer_benchmark<ALPACC_BLOCK_SIZE, ALPACC_ITEMS_PER_THREAD>(in, a.warmup, a.benchmark);
    else if (a.server)
        ret = lexer_server_impl<ALPACC_BLOCK_SIZE, ALPACC_ITEMS_PER_THREAD>(in, out);
    else
        ret = run_lexer_batch_impl<ALPACC_BLOCK_SIZE, ALPACC_ITEMS_PER_THREAD>(in, out, a.timeit);
    if (in  != stdin)  fclose(in);
    if (out != stdout) fclose(out);
    return ret;

#elif defined(HAS_PARSER) && !defined(HAS_LEXER)
    // ---- Parse-only mode ----
    int ret;
    if (a.benchmark > 0)
        ret = parser_benchmark(in, a.warmup, a.benchmark);
    else if (a.server)
        ret = parser_server(in, out);
    else
        ret = parser_batch(in, out);
    if (in  != stdin)  fclose(in);
    if (out != stdout) fclose(out);
    return ret;

#elif defined(HAS_LEXER) && defined(HAS_PARSER)
    // ---- Both mode: raw bytes → lexer → parser → CST nodes (default) ----
    int ret;
    if (a.benchmark > 0)
        ret = both_benchmark<ALPACC_BLOCK_SIZE, ALPACC_ITEMS_PER_THREAD>(in, a.warmup, a.benchmark);
    else if (a.server)
        ret = both_server_impl<ALPACC_BLOCK_SIZE, ALPACC_ITEMS_PER_THREAD>(in, out);
    else
        ret = both_batch_impl<ALPACC_BLOCK_SIZE, ALPACC_ITEMS_PER_THREAD>(in, out);
    if (in  != stdin)  fclose(in);
    if (out != stdout) fclose(out);
    return ret;
#else
    fprintf(stderr, "error: no lexer or parser compiled in\n");
    return 1;
#endif
}
