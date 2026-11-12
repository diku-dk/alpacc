// parser.cu — fused single-kernel LLP parser, appended by the code generator
// after common.cu, scan.cu, pse.cu, and the grammar constants.
//
// The whole pipeline runs in ONE cooperative kernel (parserFusedKernel),
// launched via cudaLaunchCooperativeKernel with grid.sync() between phases:
//
//   Phase A+B+C: one ticket-loop pass — per-position hash-table key lookup
//            (FNV-1a + linear probe, sliding register window over each
//            thread's positions), (slen, plen) packed into one u64 and
//            scanned with decoupled lookback, STACKS/PRODUCTIONS spans
//            scattered directly from the in-register exclusive offsets.
//   Phase D: single-pass inclusive scan of ±1 bracket deltas (computed
//            on the fly from d_brackets, decoupled lookback) → depths;
//            validity check (no negative prefix, last must be 0).
//   Phase E: PSE(<=) over depths via apsepDeviceSPT (pse.cu), which is
//            itself structured as grid.sync() phases.
//   Phase F: bracket symbol check of each right bracket against its match.
//
// Combined (lexer+parser) mode additionally computes the compact parse
// tree (mirrors `parents` / `parse_int` in parser.fut), still inside the
// same cooperative kernel:
//   Phase G: exclusive scan of (arity - 1) over the productions.
//   Phase H: PSE(<=) over that scan → parent vector (parents[0] = 0).
//   Phase I: scan of is-terminal flags → compaction: terminal slots are
//            dropped, parents remapped into the compacted numbering, and
//            each terminal slot records its parent as token_parents[lexeme].
//
// Compacted layout: buffer capacity is bounded by
// m * MAX_BRACKETS_PER_POSITION / m * MAX_PRODS_PER_POSITION (the maximal
// hash-table span sizes emitted by the generator), but all per-element work
// runs over the exact totals computed in Phase B.
//
// Binary I/O (same protocol as c/parser.c and `alpacc test compare --parser`):
//   inputs:  u64 BE num_tests; per test: u64 BE n + n×u64 BE terminal ids
//   outputs: u64 BE num_tests; per test: 1 byte validity; if valid:
//            u64 BE num_prods + num_prods×u64 BE production ids

#include <vector>
#include <limits>
#include <cstdint>
#include <cstring>
#include <climits>
#include <algorithm>

// ---------------------------------------------------------------------------
// Fused kernel configuration
// ---------------------------------------------------------------------------
//
// Block size and items-per-thread for the cooperative parser kernel are
// compile-time constants.  Precedence:
//
//   1. -DALPACC_PARSER_BLOCK_SIZE=<n> / -DALPACC_PARSER_ITEMS_PER_THREAD=<n>
//      at nvcc time wins over everything else.
//   2. The per-arch tuning table (alpacc_parser_tuning<SM, ELEM>) picks a
//      value for the codegen-baked ALPACC_SM_ARCH.  ELEM = sizeof(index_t)
//      drives the bucket, matching the lexer's convention.
//   3. Fallback: BS=256, IPT=4 (the tile size at which the fused pipeline
//      was originally tuned).
//
// The parser and lexer kernels run back-to-back (never concurrently), so
// their (BS, IPT) can be picked independently.
//
// NOTE: the PSE primitive (apsepDeviceSPT) currently carries
// `static_assert(IPT == 4)` on its blocked Phase 1.  Overriding
// ALPACC_PARSER_ITEMS_PER_THREAD to a value other than 4 will fail at
// nvcc time — that's fine as a compile-time knob for future work when the
// primitive's constraint is relaxed.

// Per-arch tuning table for the parser.  Mirrors alpacc_ipt_tuning in
// lexer.cu; separate template so the parser's numbers can't be confused
// with the lexer's.  Unlike the lexer table (which stores CUB-style
// nominal_ipt_4B units), the parser table stores the raw IPT value
// directly — the PSE primitive's `static_assert(IPT == 4)` locks the
// parser to a single IPT across index_t widths, so the CUB scaling
// convention doesn't fit here.  If PSE's constraint is later relaxed to
// allow multiple IPT values, this table's `items_per_thread` becomes a
// per-arch tunable choice.
//
// Rows tagged [measured] come from `benchmarks/sweep-cuda-parser.sh`;
// [cub] rows use CUB DeviceScan tuning as a starting point.  Rows with
// items_per_thread == 0 mean "unknown arch; fall back to defaults".
template<int SM_ARCH, size_t ELEM_BYTES>
struct alpacc_parser_tuning {
  static constexpr uint32_t items_per_thread = 0;
  static constexpr uint32_t block_size       = 256;
};

// Turing (sm_75) — T4, 1660 Ti, RTX 20xx                [measured]
// Sweep on the JSON grammar (index_t = i32, 10M-token dataset).  BS=256
// remained the fastest across the {128, 256} × {4} options accessible
// under the PSE static_assert.  Only BS was tunable at IPT=4 for the
// present PSE primitive.
template<size_t ELEM> struct alpacc_parser_tuning<75, ELEM> {
  static constexpr uint32_t items_per_thread = 4;
  static constexpr uint32_t block_size       = 256;
};

// Ampere data-centre (sm_80) — A100                     [cub]
template<size_t ELEM> struct alpacc_parser_tuning<80, ELEM> {
  static constexpr uint32_t items_per_thread = 4;
  static constexpr uint32_t block_size       = 128;
};

// Ampere consumer (sm_86) — RTX 30xx, A40               [cub]
template<size_t ELEM> struct alpacc_parser_tuning<86, ELEM> {
  static constexpr uint32_t items_per_thread = 4;
  static constexpr uint32_t block_size       = 128;
};

// Ada Lovelace (sm_89) — RTX 40xx, L4/L40               [cub]
template<size_t ELEM> struct alpacc_parser_tuning<89, ELEM> {
  static constexpr uint32_t items_per_thread = 4;
  static constexpr uint32_t block_size       = 128;
};

// Hopper (sm_90) — H100                                 [cub]
template<size_t ELEM> struct alpacc_parser_tuning<90, ELEM> {
  static constexpr uint32_t items_per_thread = 4;
  static constexpr uint32_t block_size       = 128;
};

// Blackwell (sm_100) — B100/B200                        [cub]
template<size_t ELEM> struct alpacc_parser_tuning<100, ELEM> {
  static constexpr uint32_t items_per_thread = 4;
  static constexpr uint32_t block_size       = 128;
};

template<int SM_ARCH, typename J>
constexpr uint32_t arch_parser_ipt() {
  return alpacc_parser_tuning<SM_ARCH, sizeof(J)>::items_per_thread;
}

template<int SM_ARCH, typename J>
constexpr uint32_t arch_parser_block_size() {
  return alpacc_parser_tuning<SM_ARCH, sizeof(J)>::block_size;
}

#ifndef ALPACC_PARSER_BLOCK_SIZE
constexpr uint32_t alpacc_default_parser_block_size = []{
  constexpr uint32_t table_bs = arch_parser_block_size<ALPACC_SM_ARCH, index_t>();
  return table_bs;
}();
#define ALPACC_PARSER_BLOCK_SIZE alpacc_default_parser_block_size
#endif

#ifndef ALPACC_PARSER_ITEMS_PER_THREAD
constexpr uint32_t alpacc_default_parser_ipt = []{
  constexpr uint32_t table = arch_parser_ipt<ALPACC_SM_ARCH, index_t>();
  return table == 0 ? 4u : table;
}();
#define ALPACC_PARSER_ITEMS_PER_THREAD alpacc_default_parser_ipt
#endif

// The kernel is templated on <BLOCK_SIZE, ITEMS_PER_THREAD>; the file-scope
// constants below are the resolved values that get threaded into the launch
// and buffer sizing.
constexpr uint32_t FUSED_BS  = ALPACC_PARSER_BLOCK_SIZE;
constexpr int      FUSED_IPT = ALPACC_PARSER_ITEMS_PER_THREAD;

// ---------------------------------------------------------------------------
// Bracket helpers
// ---------------------------------------------------------------------------

__device__ __forceinline__ bool bracket_is_left(bracket_t b) {
    return (b >> (8 * (int)sizeof(bracket_t) - 1)) & 1;
}

__device__ __forceinline__ bracket_t bracket_unpack(bracket_t b) {
    return b & (bracket_t)~((bracket_t)1 << (8 * (int)sizeof(bracket_t) - 1));
}

// ---------------------------------------------------------------------------
// Phase A helper: hash-table lookup for one position.
//
// The caller supplies the Q+K window key (sentinel-extended: positions
// outside [0, m) read as EMPTY_TERMINAL), typically a slice of a register
// window shared by the thread's FUSED_IPT consecutive positions.
// FNV-1a hash, linear probe up to MAX_ITERS slots.
// HASH_TABLE stores one fused HashRecord per slot (key bytes + four span_t
// spans, alignas(16)); when the record fits in 16 bytes the whole probe
// iteration is a single __ldg int4 load.  Empty slots carry all-ones spans
// (SPAN_NONE, emitted by the generator); since the table has no deletions,
// a probe chain reaching an empty slot proves the key is absent, so the
// probe terminates there.  When the key additionally fits in one machine
// word (2/4/8 bytes) the compare is a single word compare; the record is
// alignas(16), so its offset-0 key is KEY_BYTES-aligned.
// Returns true iff the key was found.
// ---------------------------------------------------------------------------

constexpr int KEY_BYTES = (Q + K) * (int)sizeof(terminal_t);
constexpr bool PACKED_PROBE = KEY_BYTES == 2 || KEY_BYTES == 4 || KEY_BYTES == 8;
constexpr bool VECTOR_RECORD = sizeof(HashRecord) == 16;
constexpr span_t SPAN_NONE = (span_t)~(span_t)0;

template<int BYTES> struct KeyWord { using type = uint32_t; };  // fallback, only used when !PACKED_PROBE
template<> struct KeyWord<2> { using type = uint16_t; };
template<> struct KeyWord<8> { using type = uint64_t; };
using key_word_t = KeyWord<KEY_BYTES>::type;

template<bool PACKED>
__device__ __forceinline__ bool
lookupSpansImpl(const terminal_t* key,
                int32_t& ss, int32_t& se, int32_t& ps, int32_t& pe)
{
    uint64_t h = 14695981039346656037ULL;
#pragma unroll
    for (int64_t j = 0; j < Q + K; j++)
        h = (h ^ (uint64_t)key[j]) * 1099511628211ULL;
    h %= (uint64_t)HASH_TABLE_SIZE;

    key_word_t kw = 0;
    if constexpr (PACKED) {
#pragma unroll
        for (int j = 0; j < Q + K; j++)
            kw |= (key_word_t)key[j] << (8 * (int)sizeof(terminal_t) * j);
    }

    ss = se = ps = pe = -1;
    for (int64_t it = 0; it < MAX_ITERS; it++) {
        int64_t slot = (int64_t)h;
        HashRecord r;
        if constexpr (VECTOR_RECORD) {
            union { int4 raw; HashRecord rec; } u;
            u.raw = __ldg((const int4*)&HASH_TABLE[slot]);
            r = u.rec;
        } else {
            r = HASH_TABLE[slot];
        }
        if (r.ss == SPAN_NONE) return false;  // empty slot: key is absent
        bool match;
        if constexpr (PACKED) {
            key_word_t rw;
            memcpy(&rw, r.key, sizeof rw);   // register move, not a load
            match = rw == kw;
        } else {
            match = true;
#pragma unroll
            for (int64_t j = 0; j < Q + K; j++) {
                if (r.key[j] != key[j]) { match = false; break; }
            }
        }
        if (match) {
            ss = (int32_t)r.ss;
            se = (int32_t)r.se;
            ps = (int32_t)r.ps;
            pe = (int32_t)r.pe;
            return true;
        }
        h = (h + 1) % (uint64_t)HASH_TABLE_SIZE;
    }
    return false;
}

__device__ __forceinline__ bool
lookupSpans(const terminal_t* key,
            int32_t& ss, int32_t& se, int32_t& ps, int32_t& pe)
{
    return lookupSpansImpl<PACKED_PROBE>(key, ss, se, ps, pe);
}

// Scan input functors (map source element → scanned index_t value).
struct BracketDelta {
    __device__ __forceinline__ index_t operator()(bracket_t b) const {
        return bracket_is_left(b) ? (index_t)1 : (index_t)-1;
    }
};
struct ArityMinusOne {
    __device__ __forceinline__ index_t operator()(production_t p) const {
        return (index_t)PRODUCTION_TO_ARITY[p] - (index_t)1;
    }
};
struct IsTermFlag {
    __device__ __forceinline__ index_t operator()(production_t p) const {
        return PRODUCTION_TO_TERMINAL_IS_VALID[p] ? (index_t)1 : (index_t)0;
    }
};

// Component-wise add of two u32 lanes packed in a u64 (bracket lengths in
// the high word, production lengths in the low word).  The lanes are added
// separately so a low-lane carry can never spill into the high lane; both
// running totals stay < 2^31 (see runParserPipeline / the PSE int casts).
struct PairAdd {
    __device__ __forceinline__ uint64_t operator()(uint64_t a, uint64_t b) const {
        return (((a >> 32) + (b >> 32)) << 32) | (uint64_t)((uint32_t)a + (uint32_t)b);
    }
};

// ---------------------------------------------------------------------------
// Fused kernel parameter block (passed by value through
// cudaLaunchCooperativeKernel).  Capacities:
//   max_bk = m * MAX_BRACKETS_PER_POSITION   (brackets, deltas, match)
//   max_pr = m * MAX_PRODS_PER_POSITION      (productions)
// ---------------------------------------------------------------------------

struct FusedBufs {
    const terminal_t* d_arr;        // [m] sentinel-extended tokens
    index_t           m;

    index_t* d_totals;              // [2] num_brackets, num_prods

    bracket_t* d_brackets;          // [max_bk]
    index_t*   d_scan;              // [max(max_bk,max_pr)] deltas → inclusive scan → depths
    index_t*   d_match;             // [max(max_bk,max_pr)] PSE result (index_t; -1 = no match)

    production_t* d_productions;    // [max_pr]

    int* d_valid;

    // Decoupled-lookback scratch for the single-pass Phase A+B+C and D
    // scans.  The statuses and the ticket counters live in the host-owned
    // d_lb_arena and are reset (to Invalid == 0 / ticket 0) by one
    // cudaMemsetAsync per launch; aggregates/prefixes need no reset (only
    // read after a status published in the same launch).
    //
    // Phase A+B+C's (slen, plen) scan is a two-component sum; each
    // component is an index_t-sized u32, and using PairStates keeps the
    // two components in separate arrays (SoA) sharing one status stream.
    // This avoids the packed-u64 layout that dragged 8-byte traffic through
    // the warp scan for two 4-byte values, matching the split we did for
    // the lexer's MaxAdd scan.
    PairStates<index_t, uint32_t, uint32_t> states_ab;
    uint32_t* d_dyn_ab;             // Phase A+B+C ticket counter
    States<index_t, index_t> states_d;
    uint32_t* d_dyn_d;              // Phase D ticket counter

    // SPT (PSE) scratch, sized for max_bk elements (combined mode:
    // max(max_bk, max_pr) — reused by the parents PSE).
    unsigned* d_unres;
    index_t*  d_block_mins;
    index_t*  d_block_warp_mins;
    index_t*  d_tree;
    index_t*  d_prefix_min;
    unsigned* d_p3_next;
    unsigned* d_p1_next;            // PSE Phase 1 tile ticket counter (lives in
                                    // d_lb_arena; self-resets in-kernel, so E
                                    // and H share it)

#ifdef HAS_LEXER
    // Combined mode (parse_int): parents + compact-tree assembly.
    // d_scan/d_match are reused for the parents scan and PSE;
    // d_match holds the (old-numbering) parent vector after Phase H.
    bool           with_parents;    // runtime: skip G/H/I on token-only paths
    index_t        num_lexemes;
    // Lookback scratch for the single-pass Phase G and I scans (own States
    // and ticket counter each — no reuse across scans, so no in-kernel
    // resets).
    States<index_t, index_t> states_g;
    uint32_t*      d_dyn_g;
    States<index_t, index_t> states_i;
    uint32_t*      d_dyn_i;
    production_t*  d_tree_prods;    // [max_pr] compacted tree production ids
    index_t*       d_tree_parents;  // [max_pr] compacted tree parent indices
    index_t*       d_token_parents; // [max_pr] per lexeme: parent tree index
#endif
};

// ---------------------------------------------------------------------------
// The fused cooperative kernel
// ---------------------------------------------------------------------------

template<uint32_t BLOCK_SIZE, uint32_t ITEMS_PER_THREAD>
__global__ void
parserFusedKernel(FusedBufs b)
{
    cg::grid_group grid = cg::this_grid();
    const index_t m     = b.m;
    const index_t grank = (index_t)grid.thread_rank();
    const index_t gsz   = (index_t)grid.size();

    // ---- Phase A+B+C: fused lookup + packed pair scan + direct scatter ----
    // One ticket-loop pass: each thread loads its Q+K+ITEMS_PER_THREAD-1 terminal
    // window once (sentinel-extended) and slides it across its ITEMS_PER_THREAD
    // consecutive positions; the (slen, plen) pair is packed into one u64
    // (slen high, plen low) and scanned once with decoupled lookback; the
    // exclusive offsets come straight out of the scan in registers and the
    // STACKS/PRODUCTIONS spans are scattered directly.  The thread owning
    // position m-1 writes d_totals, which the grid.sync() makes uniform.
    {
        constexpr index_t TILE = (index_t)BLOCK_SIZE * (index_t)ITEMS_PER_THREAD;
        constexpr int WIN = Q + K + ITEMS_PER_THREAD - 1;
        const index_t num_tiles_ab = (m + TILE - (index_t)1) / TILE;
        uint32_t tile;
        while ((tile = dynamicIndex(b.d_dyn_ab)) < (uint32_t)num_tiles_ab) {
            const index_t offset = (index_t)tile * TILE
                                 + (index_t)threadIdx.x * (index_t)ITEMS_PER_THREAD;
            terminal_t w[WIN];
#pragma unroll
            for (int j = 0; j < WIN; j++) {
                int64_t idx = (int64_t)offset + j - Q;
                w[j] = (idx < 0 || idx >= (int64_t)m) ? EMPTY_TERMINAL : b.d_arr[idx];
            }
            int32_t  ss[ITEMS_PER_THREAD], ps[ITEMS_PER_THREAD];
            int32_t  slen[ITEMS_PER_THREAD], plen[ITEMS_PER_THREAD];
            // Two independent scalar u32 register tiles instead of a packed
            // u64 tile.  Each block-local scan runs a single-instruction
            // Add combine; nvcc gets to co-schedule the two scans (ILP).
            uint32_t items_s[ITEMS_PER_THREAD], items_p[ITEMS_PER_THREAD];
#pragma unroll
            for (int j = 0; j < ITEMS_PER_THREAD; j++) {
                ss[j] = ps[j] = 0;
                slen[j] = plen[j] = 0;
                index_t gid = offset + (index_t)j;
                if (gid < m) {
                    int32_t se, pe;
                    bool valid = lookupSpans(w + j, ss[j], se, ps[j], pe);
                    if (!valid) atomicAnd(b.d_valid, 0);
                    slen[j] = valid ? se - ss[j] : 0;
                    plen[j] = valid ? pe - ps[j] : 0;
                }
                items_s[j] = (uint32_t)slen[j];
                items_p[j] = (uint32_t)plen[j];
            }
            // Two block-local scans (Add<u32>), then a single fused
            // decoupled-lookback round over the SoA PairStates.  Both cub
            // BlockScans share one TempStorage — they run sequentially in
            // the same thread, so the storage is only live during one at a
            // time.
            using BlockScan32 = cub::BlockScan<uint32_t, (int)BLOCK_SIZE>;
            __shared__ typename BlockScan32::TempStorage scan_temp;
            const uint32_t s_agg =
                scanRegLocal<uint32_t, index_t, Add<uint32_t>, ITEMS_PER_THREAD, (index_t)BLOCK_SIZE>(
                    items_s, scan_temp, Add<uint32_t>());
            const uint32_t p_agg =
                scanRegLocal<uint32_t, index_t, Add<uint32_t>, ITEMS_PER_THREAD, (index_t)BLOCK_SIZE>(
                    items_p, scan_temp, Add<uint32_t>());
            uint32_t s_prefix_u32, p_prefix_u32;
            lookbackPrefixPair<uint32_t, uint32_t, index_t, Add<uint32_t>, Add<uint32_t>>(
                b.states_ab, Add<uint32_t>(), Add<uint32_t>(), (uint32_t)0, (uint32_t)0, tile,
                s_agg, p_agg, s_prefix_u32, p_prefix_u32);
#pragma unroll
            for (int j = 0; j < ITEMS_PER_THREAD; j++) {
                index_t gid = offset + (index_t)j;
                if (gid < m) {
                    index_t s_incl = (index_t)(s_prefix_u32 + items_s[j]);
                    index_t p_incl = (index_t)(p_prefix_u32 + items_p[j]);
                    index_t soff = s_incl - (index_t)slen[j];
                    index_t poff = p_incl - (index_t)plen[j];
                    for (int32_t t = 0; t < slen[j]; t++)
                        b.d_brackets[soff + (index_t)t] = STACKS[ss[j] + t];
                    for (int32_t t = 0; t < plen[j]; t++)
                        b.d_productions[poff + (index_t)t] = PRODUCTIONS[ps[j] + t];
                    if (gid == m - (index_t)1) {
                        b.d_totals[0] = s_incl;
                        b.d_totals[1] = p_incl;
                    }
                }
            }
        }
    }
    grid.sync();

    const index_t num_brackets = b.d_totals[0];
    const index_t num_prods    = b.d_totals[1];

    if (num_brackets != (index_t)0) {   // grid-uniform
        // ---- Phase D: single-pass inclusive delta scan (decoupled lookback)
        //      → depths + validity ----
        // Deltas (±1) are computed in registers from d_brackets; one pass:
        // ticket loop over 1024-element tiles, cub::BlockScan + lookback
        // prefix, validity checked in-pass, depth written to d_scan.
        // Blocks fall through to the grid.sync() when tickets run out.
        {
            constexpr index_t TILE = (index_t)BLOCK_SIZE * (index_t)ITEMS_PER_THREAD;
            const index_t num_tiles_b = (num_brackets + TILE - (index_t)1) / TILE;
            uint32_t tile;
            while ((tile = dynamicIndex(b.d_dyn_d)) < (uint32_t)num_tiles_b) {
                const index_t offset = (index_t)tile * TILE
                                     + (index_t)threadIdx.x * (index_t)ITEMS_PER_THREAD;
                bracket_t brs[ITEMS_PER_THREAD];
                index_t   items[ITEMS_PER_THREAD];
#pragma unroll
                for (int j = 0; j < ITEMS_PER_THREAD; j++) {
                    index_t gid = offset + (index_t)j;
                    bool in = gid < num_brackets;
                    brs[j]   = in ? b.d_brackets[gid] : (bracket_t)0;
                    items[j] = in ? BracketDelta()(brs[j]) : (index_t)0;
                }
                index_t prefix =
                    scanReg<index_t, index_t, Add<index_t>, ITEMS_PER_THREAD, (index_t)BLOCK_SIZE>(
                        items, b.states_d, Add<index_t>(), (index_t)0, tile);
#pragma unroll
                for (int j = 0; j < ITEMS_PER_THREAD; j++) {
                    index_t gid = offset + (index_t)j;
                    if (gid < num_brackets) {
                        index_t s = prefix + items[j];
                        if (s < (index_t)0) atomicAnd(b.d_valid, 0);
                        if (gid == num_brackets - (index_t)1 && s != (index_t)0)
                            atomicAnd(b.d_valid, 0);
                        b.d_scan[gid] = s - (bracket_is_left(brs[j]) ? (index_t)1 : (index_t)0);  // depth
                    }
                }
            }
        }
        grid.sync();

        // ---- Phase E: PSE(<=) over depths (apsepDeviceSPT, pse.cu) ----
        {
            constexpr int B = (int)BLOCK_SIZE * ITEMS_PER_THREAD;
            const int nb         = (int)num_brackets;   // host guarantees ≤ INT_MAX
            const int spt_blocks = (nb + B - 1) / B;
            int M = 1;
            while (M < spt_blocks) M <<= 1;
            apsepDeviceSPT<index_t, (int)BLOCK_SIZE, ITEMS_PER_THREAD, true>(
                grid, b.d_scan, b.d_match, nb, spt_blocks, M, M - 1,
                b.d_unres, b.d_block_mins, b.d_block_warp_mins,
                b.d_tree, b.d_prefix_min, b.d_p3_next, b.d_p1_next);
        }
        grid.sync();

        // ---- Phase F: bracket symbol check ----
        for (index_t i = grank; i < num_brackets; i += gsz) {
            if (bracket_is_left(b.d_brackets[i])) continue;
            index_t j = b.d_match[i];
            if (j < (index_t)0 || bracket_unpack(b.d_brackets[j]) != bracket_unpack(b.d_brackets[i]))
                atomicAnd(b.d_valid, 0);
        }
    }

#ifdef HAS_LEXER
    // ---- Combined mode: parents + CST node assembly (parse_int) ----
    if (!b.with_parents) return;   // grid-uniform (kernel parameter)
    // No grid.sync() needed here: Phase F touches only d_brackets/d_match/
    // d_valid, Phase G writes d_scan (last read in Phase E, fenced at the
    // post-E sync) and reads d_productions (fenced after Phase A+B+C); the
    // sync after G orders F's d_match reads before H overwrites d_match.

    if (num_prods == (index_t)0) {
        // safe_zip: no productions must mean no lexemes
        if (grank == 0 && b.num_lexemes != (index_t)0) atomicAnd(b.d_valid, 0);
        return;
    }

    // ---- Phase G: exclusive scan of (arity - 1) over the productions ----
    // Single ticket-loop pass (decoupled lookback): (arity - 1) deltas are
    // computed in registers from d_productions and the exclusive value
    // incl - delta is written straight to d_scan.
    {
        constexpr index_t TILE = (index_t)BLOCK_SIZE * (index_t)ITEMS_PER_THREAD;
        const index_t num_tiles_g = (num_prods + TILE - (index_t)1) / TILE;
        uint32_t tile;
        while ((tile = dynamicIndex(b.d_dyn_g)) < (uint32_t)num_tiles_g) {
            const index_t offset = (index_t)tile * TILE
                                 + (index_t)threadIdx.x * (index_t)ITEMS_PER_THREAD;
            index_t deltas[ITEMS_PER_THREAD];
            index_t items[ITEMS_PER_THREAD];
#pragma unroll
            for (int j = 0; j < ITEMS_PER_THREAD; j++) {
                index_t gid = offset + (index_t)j;
                deltas[j] = (gid < num_prods) ? ArityMinusOne()(b.d_productions[gid])
                                              : (index_t)0;
                items[j]  = deltas[j];
            }
            index_t prefix =
                scanReg<index_t, index_t, Add<index_t>, ITEMS_PER_THREAD, (index_t)BLOCK_SIZE>(
                    items, b.states_g, Add<index_t>(), (index_t)0, tile);
#pragma unroll
            for (int j = 0; j < ITEMS_PER_THREAD; j++) {
                index_t gid = offset + (index_t)j;
                if (gid < num_prods)
                    b.d_scan[gid] = prefix + items[j] - deltas[j];   // exclusive scan value
            }
        }
    }
    grid.sync();

    // ---- Phase H: PSE(<=) over the scan → parent vector (in d_match) ----
    {
        constexpr int B = (int)BLOCK_SIZE * ITEMS_PER_THREAD;
        const int np         = (int)num_prods;      // host guarantees ≤ INT_MAX
        const int spt_blocks = (np + B - 1) / B;
        int M = 1;
        while (M < spt_blocks) M <<= 1;
        apsepDeviceSPT<index_t, (int)BLOCK_SIZE, ITEMS_PER_THREAD, true>(
            grid, b.d_scan, b.d_match, np, spt_blocks, M, M - 1,
            b.d_unres, b.d_block_mins, b.d_block_warp_mins,
            b.d_tree, b.d_prefix_min, b.d_p3_next, b.d_p1_next);
    }
    grid.sync();

    // ---- Phase I: is-term scan + compact-tree assembly ----
    // Terminal-production slots are dropped from the tree; indices are
    // remapped by subtracting the inclusive is-term scan (t_incl).  Parents
    // always point at production nodes, so their remap is the same
    // subtraction.  Terminal slot k (in order) belongs to lexeme
    // t_incl[i] - 1 and contributes token_parents[lexeme] instead.
    // A single ticket-loop lookback pass materializes the FULL inclusive
    // scan in d_scan — the remap below reads d_scan[p_old] at random
    // positions — and checks safe_zip in-pass.  There is no parents-fixup
    // pass: p_old is clamped when read instead.
    {
        constexpr index_t TILE = (index_t)BLOCK_SIZE * (index_t)ITEMS_PER_THREAD;
        const index_t num_tiles_i = (num_prods + TILE - (index_t)1) / TILE;
        uint32_t tile;
        while ((tile = dynamicIndex(b.d_dyn_i)) < (uint32_t)num_tiles_i) {
            const index_t offset = (index_t)tile * TILE
                                 + (index_t)threadIdx.x * (index_t)ITEMS_PER_THREAD;
            index_t items[ITEMS_PER_THREAD];
#pragma unroll
            for (int j = 0; j < ITEMS_PER_THREAD; j++) {
                index_t gid = offset + (index_t)j;
                items[j] = (gid < num_prods) ? IsTermFlag()(b.d_productions[gid])
                                             : (index_t)0;
            }
            index_t prefix =
                scanReg<index_t, index_t, Add<index_t>, ITEMS_PER_THREAD, (index_t)BLOCK_SIZE>(
                    items, b.states_i, Add<index_t>(), (index_t)0, tile);
#pragma unroll
            for (int j = 0; j < ITEMS_PER_THREAD; j++) {
                index_t gid = offset + (index_t)j;
                if (gid < num_prods) {
                    index_t incl = prefix + items[j];
                    b.d_scan[gid] = incl;
                    // safe_zip: #terminal productions must equal #lexemes
                    if (gid == num_prods - (index_t)1 && incl != b.num_lexemes)
                        atomicAnd(b.d_valid, 0);
                }
            }
        }
    }
    grid.sync();
    // Blocked remap: each thread owns ITEMS_PER_THREAD consecutive productions and
    // issues all loads for its batch (including the random d_scan[p_old]
    // gathers, via __ldg) before consuming any — the loop is latency-bound,
    // so memory-level parallelism, not bandwidth, is what this buys.
    {
        constexpr index_t TILE = (index_t)BLOCK_SIZE * (index_t)ITEMS_PER_THREAD;
        const index_t num_tiles_r = (num_prods + TILE - (index_t)1) / TILE;
        for (index_t t = (index_t)blockIdx.x; t < num_tiles_r; t += (index_t)gridDim.x) {
            const index_t offset = t * TILE + (index_t)threadIdx.x * (index_t)ITEMS_PER_THREAD;
            production_t prod[ITEMS_PER_THREAD];
            index_t t_incl[ITEMS_PER_THREAD], p_old[ITEMS_PER_THREAD], p_scan[ITEMS_PER_THREAD];
#pragma unroll
            for (int j = 0; j < ITEMS_PER_THREAD; j++) {
                index_t gid = offset + (index_t)j;
                if (gid < num_prods) {
                    prod[j]   = b.d_productions[gid];
                    t_incl[j] = b.d_scan[gid];
                    index_t p_raw = b.d_match[gid];
                    p_old[j] = (gid == (index_t)0 || p_raw < (index_t)0)
                             ? (index_t)0 : p_raw;
                } else {
                    p_old[j] = (index_t)0;
                }
            }
#pragma unroll
            for (int j = 0; j < ITEMS_PER_THREAD; j++)
                p_scan[j] = __ldg(&b.d_scan[p_old[j]]);
#pragma unroll
            for (int j = 0; j < ITEMS_PER_THREAD; j++) {
                index_t gid = offset + (index_t)j;
                if (gid < num_prods) {
                    index_t p_new = p_old[j] - p_scan[j];
                    if (PRODUCTION_TO_TERMINAL_IS_VALID[prod[j]]) {
                        index_t lex = t_incl[j] - (index_t)1;
                        if (lex < b.num_lexemes) b.d_token_parents[lex] = p_new;
                        else                     atomicAnd(b.d_valid, 0);
                    } else {
                        index_t j2 = gid - t_incl[j];
                        b.d_tree_prods[j2]   = prod[j];
                        b.d_tree_parents[j2] = p_new;
                    }
                }
            }
        }
    }
#endif
}

// ---------------------------------------------------------------------------
// Host side: allocation, launch, readback
// ---------------------------------------------------------------------------

template<typename T>
struct DevBuf {
    T* ptr = nullptr;
    DevBuf() = default;
    explicit DevBuf(size_t n) {
        if (n > 0) gpuAssert(cudaMalloc(&ptr, n * sizeof(T)));
    }
    ~DevBuf() { if (ptr) cudaFree(ptr); }
    DevBuf(const DevBuf&) = delete;
    DevBuf& operator=(const DevBuf&) = delete;
    T* get() { return ptr; }
    const T* get() const { return ptr; }
};

struct ParserFused {
    FusedBufs   bufs{};
    terminal_t* d_arr   = nullptr;   // owned; bufs.d_arr aliases it
    uint32_t    P       = 0;         // cooperative grid size
    index_t     max_m   = 0;
    index_t     max_bk  = 0;
    index_t     max_pr  = 0;
    // Lookback arena: all scan ticket counters followed by all status
    // arrays, contiguous so one cudaMemsetAsync per launch resets them.
    uint8_t*    d_lb_arena = nullptr;
    size_t      lb_arena_bytes = 0;
};

static ParserFused allocParserFused(index_t max_m) {
    constexpr index_t B = (index_t)FUSED_BS * FUSED_IPT;
    constexpr index_t W = B / 32;

    ParserFused p;
    p.max_m  = max_m;
    p.max_bk = std::max<index_t>(max_m * (index_t)MAX_BRACKETS_PER_POSITION, 1);
    p.max_pr = std::max<index_t>(max_m * (index_t)MAX_PRODS_PER_POSITION, 1);

    // Combined mode reuses d_scan32/d_match and the SPT scratch for
    // the parents phases, which run over up to max_pr elements.
#ifdef HAS_LEXER
    const index_t scan_n = std::max(p.max_bk, p.max_pr);
#else
    const index_t scan_n = p.max_bk;
#endif
    const index_t tiles_m    = (max_m + (index_t)FUSED_BS - 1) / (index_t)FUSED_BS;
    const index_t tiles_sc   = (scan_n + (index_t)FUSED_BS - 1) / (index_t)FUSED_BS;
    const index_t spt_blocks = (scan_n + B - 1) / B;
    const index_t M          = (index_t)nextPow2((int)spt_blocks);

    FusedBufs& b = p.bufs;
    gpuAssert(cudaMalloc(&p.d_arr,        (size_t)max_m * sizeof(terminal_t)));
    gpuAssert(cudaMalloc(&b.d_totals,     2 * sizeof(index_t)));
    gpuAssert(cudaMalloc(&b.d_brackets,   (size_t)p.max_bk * sizeof(bracket_t)));
    gpuAssert(cudaMalloc(&b.d_scan,       (size_t)scan_n * sizeof(index_t)));
    gpuAssert(cudaMalloc(&b.d_match,      (size_t)scan_n * sizeof(index_t)));
    gpuAssert(cudaMalloc(&b.d_productions, (size_t)p.max_pr * sizeof(production_t)));
    gpuAssert(cudaMalloc(&b.d_valid,      sizeof(int)));
    gpuAssert(cudaMalloc(&b.d_unres,      (size_t)spt_blocks * (size_t)(B / 32) * sizeof(unsigned)));
    gpuAssert(cudaMalloc(&b.d_block_mins, (size_t)spt_blocks * sizeof(index_t)));
    gpuAssert(cudaMalloc(&b.d_block_warp_mins, (size_t)spt_blocks * (size_t)W * sizeof(index_t)));
    gpuAssert(cudaMalloc(&b.d_tree,       (size_t)(2 * M - 1) * sizeof(index_t)));
    gpuAssert(cudaMalloc(&b.d_prefix_min, (size_t)spt_blocks * sizeof(index_t)));
    gpuAssert(cudaMalloc(&b.d_p3_next,    sizeof(unsigned)));
    b.d_arr = p.d_arr;

    // Single-pass lookback scan scratch (1024-element tiles; Phase A+B+C
    // over up to max_m positions, Phase D over up to max_bk brackets,
    // Phases G and I over up to max_pr productions).  Arena layout:
    // [u32 ticket counters][statuses AB][statuses D][statuses G][statuses I].
    {
        constexpr index_t LB_TILE = (index_t)FUSED_BS * (index_t)FUSED_IPT;
        const index_t lb_tiles_m = (max_m + LB_TILE - 1) / LB_TILE;
        const index_t lb_tiles_d = (p.max_bk + LB_TILE - 1) / LB_TILE;
#ifdef HAS_LEXER
        const index_t lb_tiles_p = (p.max_pr + LB_TILE - 1) / LB_TILE;
#else
        const index_t lb_tiles_p = 0;
#endif
        const size_t counters_bytes = 5 * sizeof(uint32_t);
        p.lb_arena_bytes = counters_bytes
                         + ((size_t)lb_tiles_m + (size_t)lb_tiles_d
                            + 2 * (size_t)lb_tiles_p)
                           * sizeof(AtomicStatus);
        gpuAssert(cudaMalloc(&p.d_lb_arena, p.lb_arena_bytes));
        uint32_t*     ctr = (uint32_t*)p.d_lb_arena;
        AtomicStatus* st  = (AtomicStatus*)(p.d_lb_arena + counters_bytes);
        b.d_dyn_ab  = ctr + 3;
        b.d_p1_next = ctr + 4;
        b.states_ab.num_blocks = lb_tiles_m;
        b.states_ab.statuses   = st + lb_tiles_d + 2 * lb_tiles_p;
        gpuAssert(cudaMalloc((void**)&b.states_ab.a_aggregates,
                             (size_t)lb_tiles_m * sizeof(uint32_t)));
        gpuAssert(cudaMalloc((void**)&b.states_ab.a_prefixes,
                             (size_t)lb_tiles_m * sizeof(uint32_t)));
        gpuAssert(cudaMalloc((void**)&b.states_ab.b_aggregates,
                             (size_t)lb_tiles_m * sizeof(uint32_t)));
        gpuAssert(cudaMalloc((void**)&b.states_ab.b_prefixes,
                             (size_t)lb_tiles_m * sizeof(uint32_t)));
        b.d_dyn_d = ctr + 0;
        b.states_d.num_blocks = lb_tiles_d;
        b.states_d.statuses   = st;
        gpuAssert(cudaMalloc((void**)&b.states_d.aggregates,
                             (size_t)lb_tiles_d * sizeof(index_t)));
        gpuAssert(cudaMalloc((void**)&b.states_d.prefixes,
                             (size_t)lb_tiles_d * sizeof(index_t)));
#ifdef HAS_LEXER
        b.d_dyn_g = ctr + 1;
        b.states_g.num_blocks = lb_tiles_p;
        b.states_g.statuses   = st + lb_tiles_d;
        gpuAssert(cudaMalloc((void**)&b.states_g.aggregates,
                             (size_t)lb_tiles_p * sizeof(index_t)));
        gpuAssert(cudaMalloc((void**)&b.states_g.prefixes,
                             (size_t)lb_tiles_p * sizeof(index_t)));
        b.d_dyn_i = ctr + 2;
        b.states_i.num_blocks = lb_tiles_p;
        b.states_i.statuses   = st + lb_tiles_d + lb_tiles_p;
        gpuAssert(cudaMalloc((void**)&b.states_i.aggregates,
                             (size_t)lb_tiles_p * sizeof(index_t)));
        gpuAssert(cudaMalloc((void**)&b.states_i.prefixes,
                             (size_t)lb_tiles_p * sizeof(index_t)));
#endif
    }

#ifdef HAS_LEXER
    gpuAssert(cudaMalloc(&b.d_tree_prods,    (size_t)p.max_pr * sizeof(production_t)));
    gpuAssert(cudaMalloc(&b.d_tree_parents,  (size_t)p.max_pr * sizeof(index_t)));
    gpuAssert(cudaMalloc(&b.d_token_parents, (size_t)p.max_pr * sizeof(index_t)));
    b.with_parents = false;
    b.num_lexemes  = 0;
#endif

    int bps = 0, sms = 0;
    gpuAssert(cudaOccupancyMaxActiveBlocksPerMultiprocessor(
        &bps, parserFusedKernel<FUSED_BS, FUSED_IPT>, FUSED_BS, 0));
    gpuAssert(cudaDeviceGetAttribute(&sms, cudaDevAttrMultiProcessorCount, 0));
    index_t needed   = std::max<index_t>(std::max(tiles_m, tiles_sc), 1);
    index_t resident = std::max<index_t>((index_t)bps * (index_t)sms, 1);
    p.P = (uint32_t)std::min(resident, needed);
    return p;
}

static void freeParserFused(ParserFused& p) {
    FusedBufs& b = p.bufs;
    cudaFree(p.d_arr);
    cudaFree(b.d_totals);
    cudaFree(b.d_brackets); cudaFree(b.d_scan);
    cudaFree(b.d_match);
    cudaFree(b.d_productions);
    cudaFree(b.d_valid);
    cudaFree(b.d_unres);
    cudaFree(b.d_block_mins);
    cudaFree(b.d_block_warp_mins);
    cudaFree(b.d_tree);
    cudaFree(b.d_prefix_min);
    cudaFree(b.d_p3_next);
    cudaFree(p.d_lb_arena);
    cudaFree((void*)b.states_ab.a_aggregates);
    cudaFree((void*)b.states_ab.a_prefixes);
    cudaFree((void*)b.states_ab.b_aggregates);
    cudaFree((void*)b.states_ab.b_prefixes);
    cudaFree((void*)b.states_d.aggregates);
    cudaFree((void*)b.states_d.prefixes);
#ifdef HAS_LEXER
    cudaFree(b.d_tree_prods);
    cudaFree(b.d_tree_parents);
    cudaFree(b.d_token_parents);
    cudaFree((void*)b.states_g.aggregates);
    cudaFree((void*)b.states_g.prefixes);
    cudaFree((void*)b.states_i.aggregates);
    cudaFree((void*)b.states_i.prefixes);
#endif
    p = ParserFused{};
}

// Launch the fused kernel for m ≤ max_m positions (input already on device).
static void launchParserFused(ParserFused& p, index_t m) {
    p.bufs.m = m;
    // Reset lookback ticket counters and statuses (Invalid == 0) so repeated
    // launches on the same buffers stay correct (same stream → ordered).
    gpuAssert(cudaMemsetAsync(p.d_lb_arena, 0, p.lb_arena_bytes));
    void* args[] = { (void*)&p.bufs };
    gpuAssert(cudaLaunchCooperativeKernel(
        (void*)parserFusedKernel<FUSED_BS, FUSED_IPT>, p.P, FUSED_BS, args, 0, nullptr));
    gpuAssert(cudaGetLastError());
}

// Copy input, run, read back validity and productions.
static bool runParserFused(ParserFused& p, const terminal_t* h_arr, index_t m,
                           std::vector<production_t>& out_prods)
{
    gpuAssert(cudaMemcpy(p.d_arr, h_arr, (size_t)m * sizeof(terminal_t), cudaMemcpyHostToDevice));
    int one = 1;
    gpuAssert(cudaMemcpy(p.bufs.d_valid, &one, sizeof(int), cudaMemcpyHostToDevice));

#ifdef HAS_LEXER
    p.bufs.with_parents = false;
#endif
    launchParserFused(p, m);
    gpuAssert(cudaDeviceSynchronize());

    int h_valid = 0;
    gpuAssert(cudaMemcpy(&h_valid, p.bufs.d_valid, sizeof(int), cudaMemcpyDeviceToHost));
    out_prods.clear();
    if (!h_valid) return false;

    index_t totals[2];
    gpuAssert(cudaMemcpy(totals, p.bufs.d_totals, 2 * sizeof(index_t), cudaMemcpyDeviceToHost));
    index_t num_prods = totals[1];
    if (num_prods > (index_t)0) {
        out_prods.resize((size_t)num_prods);
        gpuAssert(cudaMemcpy(out_prods.data(), p.bufs.d_productions,
                             (size_t)num_prods * sizeof(production_t), cudaMemcpyDeviceToHost));
    }
    return true;
}

// ---------------------------------------------------------------------------
// One-shot pipeline used by the CLI
// ---------------------------------------------------------------------------

bool runParserPipeline(const terminal_t* h_tokens, uint64_t n,
                       std::vector<production_t>& out_prods)
{
    if (n > (uint64_t)(std::numeric_limits<index_t>::max() - 2)) return false;
    index_t m = (index_t)(n + 2);

    // PSE indices (and SPT sizes) are ints: reject inputs whose bracket
    // capacity bound cannot be guaranteed to fit.
    if ((uint64_t)m * (uint64_t)MAX_BRACKETS_PER_POSITION > (uint64_t)INT_MAX)
        return false;

    std::vector<terminal_t> h_arr((size_t)m);
    h_arr[0] = START_TERMINAL;
    for (index_t i = 0; i < (index_t)n; i++) h_arr[(size_t)(i + 1)] = h_tokens[(size_t)i];
    h_arr[(size_t)m - 1] = END_TERMINAL;

    ParserFused p = allocParserFused(m);
    bool ok = runParserFused(p, h_arr.data(), m, out_prods);
    freeParserFused(p);
    return ok;
}

#ifdef HAS_LEXER
// ---------------------------------------------------------------------------
// Combined mode: run the fused kernel with the parents/parse_int phases over
// lexer output that already lives on the device.
// ---------------------------------------------------------------------------

struct BothTree {
    std::vector<production_t> prods;         // [num_tree_nodes] production ids
    std::vector<index_t>      parents;       // [num_tree_nodes] parent tree indices
    std::vector<index_t>      token_parents; // [num_lexemes] parent tree index per token
};

static bool runBothFused(ParserFused& p,
                         const terminal_t* d_tokens, index_t n,
                         BothTree& out)
{
    index_t m = n + (index_t)2;
    terminal_t sent = START_TERMINAL;
    gpuAssert(cudaMemcpy(p.d_arr, &sent, sizeof(terminal_t), cudaMemcpyHostToDevice));
    if (n > (index_t)0)
        gpuAssert(cudaMemcpy(p.d_arr + 1, d_tokens, (size_t)n * sizeof(terminal_t), cudaMemcpyDeviceToDevice));
    sent = END_TERMINAL;
    gpuAssert(cudaMemcpy(p.d_arr + (size_t)(m - 1), &sent, sizeof(terminal_t), cudaMemcpyHostToDevice));
    int one = 1;
    gpuAssert(cudaMemcpy(p.bufs.d_valid, &one, sizeof(int), cudaMemcpyHostToDevice));

    p.bufs.with_parents = true;
    p.bufs.num_lexemes  = n;

    launchParserFused(p, m);
    gpuAssert(cudaDeviceSynchronize());

    int h_valid = 0;
    gpuAssert(cudaMemcpy(&h_valid, p.bufs.d_valid, sizeof(int), cudaMemcpyDeviceToHost));
    if (!h_valid) return false;

    index_t totals[2];
    gpuAssert(cudaMemcpy(totals, p.bufs.d_totals, 2 * sizeof(index_t), cudaMemcpyDeviceToHost));
    // Validity guarantees #terminal slots == n, so the compacted tree has
    // np - n nodes.
    const index_t nt = totals[1] - n;

    out.prods.resize((size_t)nt);
    out.parents.resize((size_t)nt);
    out.token_parents.resize((size_t)n);
    if (nt > (index_t)0) {
        gpuAssert(cudaMemcpy(out.prods.data(), p.bufs.d_tree_prods,
                             (size_t)nt * sizeof(production_t), cudaMemcpyDeviceToHost));
        gpuAssert(cudaMemcpy(out.parents.data(), p.bufs.d_tree_parents,
                             (size_t)nt * sizeof(index_t), cudaMemcpyDeviceToHost));
    }
    if (n > (index_t)0)
        gpuAssert(cudaMemcpy(out.token_parents.data(), p.bufs.d_token_parents,
                             (size_t)n * sizeof(index_t), cudaMemcpyDeviceToHost));
    return true;
}
#endif // HAS_LEXER
