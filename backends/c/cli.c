// cli.c — generated program entry point (C backend).
//
// Appended last by the code generator, after the grammar constants,
// lexer.c (if HAS_LEXER), and parser.c (if HAS_PARSER).
//
// The generated code defines:
//   void run_case(uint64_t n, const uint8_t *in, FILE *out);
//     — one test frame (native types, host byte order), result to `out`
//   void print_layout(FILE *out);
//     — native type sizes as key=value lines (for --layout)
// plus the INPUT_BYTES(n) payload-size macro.
//
// Wire format (native little-endian batch protocol, see
// docs/wire-protocols.md):
//   batch input : u64 num_tests, then per test a frame:
//                 u64 frame_len, u64 n, payload
//                 (payload: n raw bytes for lexer/both;
//                  n × sizeof(terminal_t) token ids for parser)
//   batch output: u64 num_tests, then per test a response record:
//                 u8 valid; if valid: u64 count + native-width fields.
//   --server    : a loop of such counted batches until EOF, flushed
//                 after each response record.
//
// Flags:
//   -i FILE    input file  (default: stdin)
//   -o FILE    output file (default: stdout)
//   --timeit   print wall-clock time to stderr
//   --server   counted-batch loop until EOF (flush per response)
//   --layout   print native type sizes and exit
//   -h/--help  show usage

// ---------------------------------------------------------------------------
// I/O helpers (host byte order; alpacc targets little-endian hosts)
// ---------------------------------------------------------------------------

static uint64_t read_u64le(FILE *f) {
    uint64_t v;
    if (fread(&v, sizeof v, 1, f) != 1) return (uint64_t)-1;
    return v;
}

static void write_u64le(FILE *f, uint64_t v) { fwrite(&v, sizeof v, 1, f); }

// ---------------------------------------------------------------------------
// Argument parsing
// ---------------------------------------------------------------------------

static void usage(const char *prog) {
    fprintf(stderr,
        "Usage: %s [options]\n"
        "  -i FILE    input file  (default: stdin)\n"
        "  -o FILE    output file (default: stdout)\n"
        "  --timeit   print wall-clock elapsed time to stderr\n"
        "  --server   counted-batch loop until EOF (flush per response)\n"
        "  --layout   print native type sizes and exit\n"
        "  -h/--help  show this message\n",
        prog);
}

typedef struct {
    const char *input_file;
    const char *output_file;
    bool timeit;
    bool server;
} CliArgs;

static CliArgs parse_args(int argc, char *argv[]) {
    CliArgs a = { NULL, NULL, false, false };
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
            usage(argv[0]); exit(0);
        } else if (strcmp(argv[i], "--timeit") == 0) {
            a.timeit = true;
        } else if (strcmp(argv[i], "--server") == 0) {
            a.server = true;
        } else if (strcmp(argv[i], "--layout") == 0) {
            print_layout(stdout); exit(0);
        } else if (strcmp(argv[i], "-i") == 0 && i + 1 < argc) {
            a.input_file = argv[++i];
        } else if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
            a.output_file = argv[++i];
        } else {
            fprintf(stderr, "error: unknown argument '%s'\n", argv[i]);
            usage(argv[0]); exit(1);
        }
    }
    return a;
}

// ---------------------------------------------------------------------------
// Frame processing (shared by batch and server modes)
//
// Frame format: u64 frame_len; content: u64 n + payload
// (n raw bytes for lexer/both; n × sizeof(terminal_t) token ids for parser).
// Returns 0 on success, -1 on error.
// ---------------------------------------------------------------------------

static int process_frame(FILE *in, FILE *out) {
    uint64_t frame_len = read_u64le(in);
    if (feof(in)) {
        fprintf(stderr, "error: truncated input\n"); return -1;
    }
    if (frame_len == (uint64_t)-1 || frame_len < 8) {
        fprintf(stderr, "error: bad frame length\n"); return -1;
    }
    uint8_t *frame = (uint8_t *) malloc(frame_len);
    if (fread(frame, 1, frame_len, in) != frame_len) {
        fprintf(stderr, "error: truncated frame\n"); free(frame); return -1;
    }
    // First 8 bytes of the frame are n (host byte order).
    uint64_t n;
    memcpy(&n, frame, sizeof n);
    if (frame_len != 8 + INPUT_BYTES(n)) {
        fprintf(stderr, "error: frame length mismatch\n"); free(frame); return -1;
    }
    run_case(n, frame + 8, out);
    free(frame);
    return 0;
}

// ---------------------------------------------------------------------------
// Counted batch: u64 num_tests header, then that many frames.  The output
// leads with the same num_tests, then one response record per frame.
// Returns 0 on success, 1 on clean EOF before the count (only when
// allow_eof), -1 on error.
// ---------------------------------------------------------------------------

static int process_batch(FILE *in, FILE *out, bool allow_eof, bool flush_each) {
    uint64_t num_tests = read_u64le(in);
    if (feof(in)) {
        if (allow_eof) return 1;
        fprintf(stderr, "error: truncated input\n"); return -1;
    }
    if (num_tests == (uint64_t)-1) {
        fprintf(stderr, "error: truncated input\n"); return -1;
    }
    write_u64le(out, num_tests);
    for (uint64_t t = 0; t < num_tests; t++) {
        if (process_frame(in, out) != 0) return -1;
        if (flush_each) fflush(out);
    }
    return 0;
}

// Batch mode: exactly one counted batch.
static int batch_mode(FILE *in, FILE *out) {
    int r = process_batch(in, out, false, false);
    fflush(out);
    return r == 0 ? 0 : 1;
}

// Server mode: counted batches in a loop until EOF, flush per response.
static int server_mode(FILE *in, FILE *out) {
    for (;;) {
        int r = process_batch(in, out, true, true);
        if (r == 1) break;
        if (r < 0) return 1;
        fflush(out);
    }
    return 0;
}

// ---------------------------------------------------------------------------
// main
// ---------------------------------------------------------------------------

int main(int argc, char *argv[]) {
    CliArgs a = parse_args(argc, argv);

    FILE *in  = stdin;
    FILE *out = stdout;
    if (a.input_file)  { in  = fopen(a.input_file,  "rb"); if (!in)  { perror(a.input_file);  return 1; } }
    if (a.output_file) { out = fopen(a.output_file, "wb"); if (!out) { perror(a.output_file); return 1; } }

    struct timespec t0, t1;
    if (a.timeit) clock_gettime(CLOCK_MONOTONIC, &t0);

    int ret = a.server ? server_mode(in, out) : batch_mode(in, out);

    if (a.timeit) {
        clock_gettime(CLOCK_MONOTONIC, &t1);
        double ms = (t1.tv_sec - t0.tv_sec) * 1e3 + (t1.tv_nsec - t0.tv_nsec) * 1e-6;
        fprintf(stderr, "Time: %.2fms\n", ms);
    }

    if (in  != stdin)  fclose(in);
    if (out != stdout) fclose(out);
    return ret;
}
