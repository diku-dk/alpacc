// pse.cu — previous smaller element (PSE) primitive, vendored from the APSEP
// project (https://github.com/CoderDue/APSEP, src/apsep.cuh, SPT kernel).
//
// Computes, for every i, the nearest j < i with in[j] < in[i] (INCL=false)
// or in[j] <= in[i] (INCL=true), else -1.  The LLP parser uses INCL=true for
// bracket matching (over depths) and parent vectors (over the arity scan).
//
// The algorithm lives in apsepDeviceSPT, a __device__ function taking the
// cooperative grid handle, so it can run either standalone (apsepKernelSPT
// wrapper, one cooperative launch) or as a phase of a larger cooperative
// kernel (the fused LLP parser in parser.cu).
//
// Single cooperative launch (grid.sync()), three phases:
//   Phase 1: blocked layout, in-register sequential ANSV per thread, warp
//            prefix-min + pointer-jumping chain, unresolved bitmask d_unres.
//   Phase 2: segment min-tree over block mins, one level per grid.sync(),
//            then per-block exclusive prefix-min by tree ascent.
//   Phase 3: bitmask-driven tree ascent+descent lookups, chunk-min + leaf
//            ballots, dense -1-fill fast path; 32-word groups drawn from an
//            atomic ticket queue, pending lookups pooled per warp and run
//            as 32 independent per-lane ascents (load balancing).
//
// Grid must fit on-chip for grid.sync(): launch exactly
// cudaOccupancyMaxActiveBlocksPerMultiprocessor x num_SMs physical blocks
// via cudaLaunchCooperativeKernel (allocSPTScratch queries this).
//
// Constraints (static_asserted): IPT == 4, 4-byte T, B/32 <= 32.
// Measured on GTX 1660 Ti at N=32M, int keys: 98/182/174 GB/s useful
// bandwidth on random/descending/ascending input; INCL=true costs nothing
// (compile-time predicate) and is ~1.5x faster than strict on tie-heavy
// input, since ties then resolve intra-block.

#include <limits.h>
#include <float.h>
#include <cooperative_groups.h>
namespace cg = cooperative_groups;

template <typename T> struct ApsepInfinity {
    __host__ __device__ static T value();
};
template <> struct ApsepInfinity<int> {
    __host__ __device__ static int value() { return INT_MAX; }
};
template <> struct ApsepInfinity<unsigned> {
    __host__ __device__ static unsigned value() { return UINT_MAX; }
};
template <> struct ApsepInfinity<long> {
    __host__ __device__ static long value() { return LONG_MAX; }
};
template <> struct ApsepInfinity<long long> {
    __host__ __device__ static long long value() { return LLONG_MAX; }
};
template <> struct ApsepInfinity<float> {
    __host__ __device__ static float value() { return FLT_MAX; }
};
template <> struct ApsepInfinity<double> {
    __host__ __device__ static double value() { return DBL_MAX; }
};

static inline int nextPow2(int x) {
    int p = 1;
    while (p < x) p <<= 1;
    return p;
}

template <typename T, int BLOCK_SIZE, int IPT, bool INCL = false>
__device__
void apsepDeviceSPT(
        cg::grid_group          grid,
        const T* __restrict__   d_in,
        T*                      d_out,
        int                     n,
        int                     num_blocks,
        int                     M,            // next pow2 >= num_blocks
        int                     leaf_offset,  // M - 1
        unsigned* __restrict__  d_unres,      // 1 bit/element: needs inter-block lookup
        T* __restrict__         d_block_mins,
        T* __restrict__         d_block_warp_mins,
        T* __restrict__         d_tree,       // 2*M-1 nodes
        T* __restrict__         d_prefix_min, // prefix_min[b] = min(block_min[0..b-1]), INF for b=0
        unsigned* __restrict__  d_p3_next,    // Phase 3 work-queue ticket counter
        unsigned* __restrict__  d_p1_next)    // Phase 1 tile ticket counter (must be 0 on entry;
                                              // reset in-kernel after use, see Phase 2)
{
    constexpr int B         = BLOCK_SIZE * IPT;
    constexpr int NUM_WARPS = BLOCK_SIZE / 32;
    constexpr int W         = B / 32;
    const     T   INF       = ApsepInfinity<T>::value();

    const int phys_bid   = (int)blockIdx.x;
    const int num_phys   = (int)gridDim.x;
    const int lane       = threadIdx.x & 31;
    const int warp_id    = threadIdx.x >> 5;

    // Double-buffered per-tile shared state: the block loop alternates
    // buffers by iteration parity, so it needs no loop-bottom barrier —
    // fast warps may load the next tile (other parity) while stragglers
    // still read this one.  The mid-loop __syncthreads() bounds the skew
    // to one iteration, so two tiles never share a buffer.
    __shared__ T s_elems_buf[2][B];
    __shared__ T s_tmin_buf[2][BLOCK_SIZE];
    __shared__ T s_warp_min_buf[2][NUM_WARPS];
    __shared__ T s_chunk_min_buf[2][NUM_WARPS * (32 / 8)];

    static_assert(IPT == 4, "blocked Phase 1 assumes IPT == 4");
    static_assert(sizeof(T) == 4 || sizeof(T) == 8, "blocked Phase 1 assumes 4- or 8-byte T");
    static_assert(W <= 32, "warp-cooperative Phase 3 assumes W <= 32");

    // lt(a, b): "a qualifies as the answer for query value b".  INCL=false
    // is APSEP (strict previous-smaller); INCL=true is PSE(<=) (previous
    // smaller or equal, e.g. LLP bracket matching / parent vectors).  Every
    // query comparison in all three phases must go through this predicate;
    // INCL is a compile-time constant, so the branch folds away.
    // (Macro; #undef'd at the end of the kernel.)
    #define lt(a, b) (INCL ? ((a) <= (b)) : ((a) < (b)))

    // -------------------------------------------------------------------------
    // Phase 1 (blocked layout): each thread owns IPT consecutive elements
    // (one int4 load), resolves within-thread matches sequentially in
    // registers, and runs the warp machinery (prefix-min scan, pointer-
    // jumping ANSV chain) over per-thread mins — 4x fewer shuffles than the
    // striped layout, and descending runs resolve with no shuffles at all.
    // -------------------------------------------------------------------------
    // Dynamic tile scheduling: blocks draw tile ids from a global ticket
    // counter, so tile-workload variance (divergent resolution walks) does
    // not pile up on the grid.sync after Phase 1 the way static striding
    // lets it.  The *next* ticket is prefetched just before the existing
    // mid-loop barrier, which then also publishes it — no extra barrier
    // per tile, so the double-buffer skew allowance is preserved.
    __shared__ int s_next_tile[2];
    if (threadIdx.x == 0)
        s_next_tile[0] = (int)atomicAdd(d_p1_next, 1u);
    __syncthreads();
    int parity   = 0;
    int block_id = s_next_tile[0];
    while (block_id < num_blocks) {
        T* const s_elems     = s_elems_buf[parity];
        T* const s_tmin      = s_tmin_buf[parity];
        T* const s_warp_min  = s_warp_min_buf[parity];
        T* const s_chunk_min = s_chunk_min_buf[parity];
        const int glb_offs = block_id * B;
        const int tbase    = IPT * threadIdx.x;   // block-relative
        const bool full    = (glb_offs + B <= n);

        T v[IPT];
        if (full) {
            if constexpr (sizeof(T) == 4) {
                int4 raw = *reinterpret_cast<const int4*>(d_in + glb_offs + tbase);
                v[0] = raw.x; v[1] = raw.y; v[2] = raw.z; v[3] = raw.w;
                *reinterpret_cast<int4*>(s_elems + tbase) = raw;
            } else {
                longlong2 r0 = *reinterpret_cast<const longlong2*>(d_in + glb_offs + tbase);
                longlong2 r1 = *reinterpret_cast<const longlong2*>(d_in + glb_offs + tbase + 2);
                v[0] = (T)r0.x; v[1] = (T)r0.y; v[2] = (T)r1.x; v[3] = (T)r1.y;
                *reinterpret_cast<longlong2*>(s_elems + tbase)     = r0;
                *reinterpret_cast<longlong2*>(s_elems + tbase + 2) = r1;
            }
        } else {
            #pragma unroll
            for (int i = 0; i < IPT; i++) {
                int gid = glb_offs + tbase + i;
                v[i] = (gid < n) ? d_in[gid] : INF;
                s_elems[tbase + i] = v[i];
            }
        }

        // In-thread sequential ANSV (block-relative result, -1 if none)
        int res[IPT];
        res[0] = -1;
        res[1] = lt(v[0], v[1]) ? tbase     : -1;
        res[2] = lt(v[1], v[2]) ? tbase + 1 : lt(v[0], v[2]) ? tbase : -1;
        res[3] = lt(v[2], v[3]) ? tbase + 2 : lt(v[1], v[3]) ? tbase + 1
                                            : lt(v[0], v[3]) ? tbase : -1;

        const T tmin = min(min(v[0], v[1]), min(v[2], v[3]));
        s_tmin[threadIdx.x] = tmin;

        // Warp inclusive prefix-min over thread mins
        T c = tmin;
        #pragma unroll
        for (int step = 1; step <= 16; step <<= 1) {
            T nb = __shfl_up_sync(0xffffffff, c, step);
            if (lane >= step) c = min(c, nb);
        }
        const T carry = __shfl_up_sync(0xffffffff, c, 1);  // valid for lane>0
        if (lane == 31) s_warp_min[warp_id] = c;

        // Chunk mins (32 consecutive elements = 8 threads) for Phase 3
        {
            T om = tmin;
            om = min(om, __shfl_xor_sync(0xffffffff, om, 1));
            om = min(om, __shfl_xor_sync(0xffffffff, om, 2));
            om = min(om, __shfl_xor_sync(0xffffffff, om, 4));
            if ((lane & 7) == 0) {
                d_block_warp_mins[(size_t)block_id * W + warp_id * (32/8) + (lane >> 3)] = om;
                s_chunk_min[warp_id * (32/8) + (lane >> 3)] = om;
            }
        }

        if (threadIdx.x == 0)
            s_next_tile[parity ^ 1] = (int)atomicAdd(d_p1_next, 1u);

        __syncthreads();  // covers cross-thread s_elems/s_tmin/s_warp_min reads
                          // (and the s_next_tile prefetch above) and bounds
                          // the double-buffer skew to one iteration

        // Thread-level ANSV chain over tmins (pointer jumping; each lane's
        // query is its own tmin, so the gap-bound argument of the original
        // element-level algorithm carries over verbatim).
        int chain = -1;
        {
            bool has = (lane > 0) && lt(carry, tmin);
            if (__any_sync(0xffffffff, has)) {
                int g = has ? lane - 1 : -1;
                while (true) {
                    int src = (g >= 0) ? g : 0;
                    T   tg  = __shfl_sync(0xffffffff, tmin, src);
                    int gg  = __shfl_sync(0xffffffff, g,    src);
                    bool jump = (g >= 0) && !lt(tg, tmin);
                    if (!__any_sync(0xffffffff, jump)) break;
                    if (jump) g = gg;
                }
                chain = g;
            }
        }

        // Per-element resolution.  Locally-unresolved elements of a thread
        // are its prefix minima (non-increasing), so the spine walk position
        // `cur` is monotone across i and can be reused.
        unsigned bal[IPT];
        int cur = lane - 1;
        #pragma unroll
        for (int i = 0; i < IPT; i++) {
            const bool active = full || (glb_offs + tbase + i < n);
            const T val = v[i];
            bool pending = active && (res[i] < 0) && (lane > 0) && lt(carry, val);

            if (__any_sync(0xffffffff, pending)) {
                // Walk cur along the chain while !lt(tmin[cur], val).  Threads
                // strictly between cur and lane never satisfy lt(tmin, val), so
                // the walk cannot skip the answer; pending guarantees the
                // answer exists, so cur lands on it (never -1).
                while (true) {
                    int src = (pending && cur >= 0) ? cur : 0;
                    T   tg  = __shfl_sync(0xffffffff, tmin,  src);
                    int cg  = __shfl_sync(0xffffffff, chain, src);
                    bool step = pending && (cur >= 0) && !lt(tg, val);
                    if (!__any_sync(0xffffffff, step)) break;
                    if (step) cur = cg;
                }
                if (pending) {
                    int tt = warp_id * 32 + cur;
                    const T* e = s_elems + IPT * tt;
                    int k = lt(e[3], val) ? 3 : lt(e[2], val) ? 2 : lt(e[1], val) ? 1 : 0;
                    res[i] = IPT * tt + k;
                }
            }

            // Cross-warp fallback: warp min, then 8-thread chunk min, then
            // thread min, then exact.  Each level is a guaranteed-hit filter
            // (a qualifying min implies a qualifying entry below), so the
            // scans are warps + 4 chunks + 8 tmins instead of warps + 32.
            if (active && res[i] < 0) {
                for (int w = warp_id - 1; w >= 0 && res[i] < 0; w--) {
                    if (lt(s_warp_min[w], val)) {
                        for (int c = (32/8) - 1; c >= 0; c--) {
                            if (lt(s_chunk_min[(32/8) * w + c], val)) {
                                for (int tt = 32 * w + 8 * c + 7; ; tt--) {
                                    if (lt(s_tmin[tt], val)) {
                                        const T* e = s_elems + IPT * tt;
                                        int k = lt(e[3], val) ? 3 : lt(e[2], val) ? 2
                                              : lt(e[1], val) ? 1 : 0;
                                        res[i] = IPT * tt + k;
                                        break;
                                    }
                                }
                                break;
                            }
                        }
                    }
                }
            }

            if (active && res[i] >= 0) d_out[glb_offs + tbase + i] = (T)(glb_offs + res[i]);
            bal[i] = __ballot_sync(0xffffffff, active && res[i] < 0);
        }

        // Publish unresolved bits (Phase 3 writes each unresolved d_out byte
        // exactly once).  The word for chunk c of this warp packs the 4
        // ballots' bytes c — element 4j+i of the chunk maps to bit 8i+j; the
        // Phase 3 reader uses the same permutation (mybit).
        if (lane < 32/8) {
            unsigned word = ((bal[0] >> (8 * lane)) & 0xffu)
                          | (((bal[1] >> (8 * lane)) & 0xffu) << 8)
                          | (((bal[2] >> (8 * lane)) & 0xffu) << 16)
                          | (((bal[3] >> (8 * lane)) & 0xffu) << 24);
            d_unres[(unsigned)glb_offs / 32 + warp_id * (32/8) + lane] = word;
        }

        if (threadIdx.x == 0) {
            T bmin = s_warp_min[0];
            #pragma unroll
            for (int w = 1; w < NUM_WARPS; w++) bmin = min(bmin, s_warp_min[w]);
            d_block_mins[block_id] = bmin;
        }

        block_id = s_next_tile[parity ^ 1];
        parity ^= 1;
    }

    // -------------------------------------------------------------------------
    // grid.sync(): all block_mins are now populated
    // -------------------------------------------------------------------------
    grid.sync();

    // -------------------------------------------------------------------------
    // Phase 2: build segment min-tree bottom-up
    // Fill leaves: d_tree[leaf_offset + i] = (i < num_blocks) ? block_mins[i] : INF
    // -------------------------------------------------------------------------
    for (int i = phys_bid * BLOCK_SIZE + threadIdx.x; i < M; i += num_phys * BLOCK_SIZE)
        d_tree[leaf_offset + i] = (i < num_blocks) ? d_block_mins[i] : INF;

    // Reset the ticket counters; the "tree fully built" grid.sync below
    // publishes them before any warp draws a Phase 3 ticket, and long
    // before the *next* apsepDeviceSPT call draws a Phase 1 ticket (E and
    // H share both counters via this self-reset; the grid.sync above
    // guarantees all Phase 1 tickets of this call are already drawn).
    if (phys_bid == 0 && threadIdx.x == 0) { *d_p3_next = 0u; *d_p1_next = 0u; }

    // Reduce level by level
    {
        int level_size  = M / 2;
        int level_start = M / 2 - 1;
        while (level_size > 0) {
            grid.sync();
            for (int i = phys_bid * BLOCK_SIZE + threadIdx.x;
                 i < level_size;
                 i += num_phys * BLOCK_SIZE) {
                int node = level_start + i;
                d_tree[node] = min(d_tree[2 * node + 1], d_tree[2 * node + 2]);
            }
            level_size  >>= 1;
            level_start  = (level_start - 1) / 2;
        }
    }

    // -------------------------------------------------------------------------
    // grid.sync(): tree is fully built
    // -------------------------------------------------------------------------
    grid.sync();

    // Exclusive prefix-min: d_prefix_min[b] = min(block_min[0..b-1]).
    // Computed per block by ascending the already-built min-tree and taking
    // the min over left siblings on the leaf-to-root path (their subtrees
    // cover exactly leaves [0, b)).  O(log M) L2-cached reads per block and
    // a single pass, replacing the Hillis-Steele scan's log2(M) grid.sync
    // passes and 8 MB of DRAM traffic.
    for (int b = phys_bid * BLOCK_SIZE + threadIdx.x; b < num_blocks; b += num_phys * BLOCK_SIZE) {
        T pm = INF;
        int node = leaf_offset + b;
        while (node > 0) {
            if (node % 2 == 0)
                pm = min(pm, __ldg(&d_tree[node - 1]));
            node = (node - 1) / 2;
        }
        d_prefix_min[b] = pm;
    }
    grid.sync();

    // -------------------------------------------------------------------------
    // Phase 3: resolve elements flagged in d_unres, driven directly by the
    // bitmask words (one word = 32 elements), warps grid-striding over words.
    // No per-block sweep, no shared queue, no __syncthreads: a zero word is
    // skipped after one cached read, so the fixed cost is the N/8-byte
    // bitmask read instead of a B-element scan per logical block (measured
    // ~1.2 ms of Phase 3 overhead on random/ascending at N=32M).
    // -------------------------------------------------------------------------
    const int num_words  = (n + 31) >> 5;
    const int num_groups = (num_words + 31) >> 5;
    const int mybit      = 8 * (lane & 3) + (lane >> 2);  // blocked-P1 bit permutation

    // Each warp takes an aligned group of 32 consecutive words (one coalesced
    // load), drawn from a global atomic ticket queue rather than a static
    // grid stride: word groups that need real inter-block lookups are orders
    // of magnitude more expensive than all-zero groups and cluster on
    // structured input, so static ownership left most warps idling at the
    // trailing grid.sync while a few stragglers worked (measured ~56% of the
    // LLP parser's PSE time).  Groups are independent (each unresolved d_out
    // byte is written exactly once; tree/chunk-min/d_in are read-only here),
    // so out-of-order processing is safe.
    //
    // Within a group, elements that need a tree lookup are pooled into a
    // per-warp worklist and resolved 32 at a time, each lane running an
    // independent tree ascent: the ascent is the long dependent-load chain,
    // and resolving one element per warp-iteration made the warp's time per
    // group the *sum* of its elements' ascent latencies instead of ~max.
    __shared__ unsigned short s_wl[NUM_WARPS][64];  // group-local ids: word*32+lane
    for (;;) {
        int group;
        if (lane == 0) group = (int)atomicAdd(d_p3_next, 1u);
        group = __shfl_sync(0xffffffff, group, 0);
        if (group >= num_groups) break;
        const int wbase = group * 32;
        const int wl = wbase + lane;
        const unsigned um_l = (wl < num_words) ? __ldg(&d_unres[wl]) : 0u;

        // Per-lane metadata for this lane's word, loaded once per group and
        // shuffled into the per-word loop (guard lanes are clamped; their
        // words are never processed since um_l == 0).  eo_l is the
        // whole-block early-out: unresolved elements within a block form a
        // non-increasing sequence (were an earlier one smaller, the later
        // one would have resolved intra-block), and the block's first
        // element is always unresolved — so it is the max of them all.  If
        // prefix_min fails lt() against it, every unresolved element in the
        // block resolves to -1 (holds for both < and <= semantics).
        const int  bid_l = min(wl / W, num_blocks - 1);
        const T    pm_l  = __ldg(&d_prefix_min[bid_l]);
        const bool eo_l  = !lt(pm_l, __ldg(&d_in[(size_t)bid_l * B]));

        // Dense fast path (descending worst case): every word in the group
        // fully unresolved and early-out — the whole span is -1.
        // Coalesced 16-byte vector stores, no per-word loop.
        if (__all_sync(0xffffffff, eo_l && um_l == 0xffffffffu)) {
            if constexpr (sizeof(T) == 4) {
                int4* out4 = reinterpret_cast<int4*>(d_out) + (size_t)wbase * 8 + lane;
                const int4 m1 = make_int4(-1, -1, -1, -1);
                #pragma unroll
                for (int i = 0; i < 8; i++) out4[i * 32] = m1;
            } else {
                // sizeof(T)==8: each longlong2 covers 2 elements; need 16 stores
                // to cover the same 32 words × 32 elements/word = 1024 elements.
                longlong2* out2 = reinterpret_cast<longlong2*>(d_out) + (size_t)wbase * 16 + lane;
                const longlong2 m1 = make_longlong2(-1LL, -1LL);
                #pragma unroll
                for (int i = 0; i < 16; i++) out2[i * 32] = m1;
            }
            continue;
        }

        // Resolve a batch of k (<= 32) pooled worklist entries.  Each lane
        // takes one entry and runs the tree ascent+descent independently
        // (divergent but parallel — 32 dependent-load chains in flight
        // instead of one); the cheap two-ballot finish stays cooperative.
        auto resolveBatch = [&](int k) {
            const int e    = (lane < k) ? (int)s_wl[warp_id][lane] : 0;
            const int gid  = wbase * 32 + e;
            const int blk  = min((wbase + (e >> 5)) / W, num_blocks - 1);
            const T   qval = (lane < k) ? __ldg(&d_in[gid]) : INF;

            int found_block = -1;
            if (lane < k) {
                int node = leaf_offset + blk;
                while (node > 0) {
                    if (node % 2 == 0) {
                        int left_sib = node - 1;
                        if (lt(__ldg(&d_tree[left_sib]), qval)) {
                            node = left_sib;
                            while (node < leaf_offset) {
                                int rc = 2 * node + 2;
                                node = lt(__ldg(&d_tree[rc]), qval) ? rc : (2 * node + 1);
                            }
                            found_block = node - leaf_offset;
                            break;
                        }
                    }
                    node = (node - 1) / 2;
                }
                if (found_block < 0) d_out[gid] = (T)-1;
            }

            unsigned hits = __ballot_sync(0xffffffff, found_block >= 0);
            while (hits) {
                const int b  = __ffs(hits) - 1;
                hits &= hits - 1;
                const T   q  = __shfl_sync(0xffffffff, qval, b);
                const int fb = __shfl_sync(0xffffffff, found_block, b);
                const int g  = __shfl_sync(0xffffffff, gid, b);

                // lt(fb's block_min, q), so both ballots are nonzero.
                const T* wm = d_block_warp_mins + (size_t)fb * W;
                T wv = (lane < W) ? __ldg(&wm[lane]) : INF;
                // lane < W guard: under INCL, lt(INF, INT_MAX-valued q)
                // is true, so padding lanes must not enter the ballot.
                unsigned wmask = __ballot_sync(0xffffffff, lane < W && lt(wv, q));
                int wstar = 31 - __clz(wmask);
                // fb < the querying block, so all B of its elements are < n:
                // reading d_in here is safe and identical to the old leaves.
                const T* bl = d_in + (size_t)fb * B + wstar * 32;
                unsigned lmask = __ballot_sync(0xffffffff, lt(__ldg(&bl[lane]), q));
                if (lane == 0)
                    d_out[g] = (T)(fb * B + wstar * 32 + (31 - __clz(lmask)));
            }
        };

        unsigned nz = __ballot_sync(0xffffffff, um_l != 0);
        int wl_count = 0;   // uniform across the warp

        while (nz) {
            const int j = __ffs(nz) - 1;
            nz &= nz - 1;
            const unsigned um = __shfl_sync(0xffffffff, um_l, j);
            const int w = wbase + j;

            const T    prefix_min_b = __shfl_sync(0xffffffff, pm_l, j);
            const bool eo           = __shfl_sync(0xffffffff, (int)eo_l, j);
            const int  base         = w * 32;
            const bool mine         = (um >> mybit) & 1u;

            if (eo) {
                if (mine) d_out[base + lane] = (T)-1;
                continue;
            }

            const int gid = base + lane;
            T val = mine ? __ldg(&d_in[gid]) : INF;    // coalesced masked load
            const bool need = mine && lt(prefix_min_b, val);
            if (mine && !need) d_out[gid] = (T)-1;

            // Pool the remaining bits into the worklist; flush 32 at a time.
            const unsigned pend = __ballot_sync(0xffffffff, need);
            const int pos = wl_count + __popc(pend & ((1u << lane) - 1));
            if (need) s_wl[warp_id][pos] = (unsigned short)(j * 32 + lane);
            wl_count += __popc(pend);

            if (wl_count >= 32) {
                __syncwarp();          // publish appended entries
                resolveBatch(32);
                const int rem = wl_count - 32;
                const unsigned short ce =
                    (lane < rem) ? s_wl[warp_id][32 + lane] : (unsigned short)0;
                __syncwarp();          // batch + carry reads done before overwrite
                if (lane < rem) s_wl[warp_id][lane] = ce;
                wl_count = rem;
            }
        }

        // Drain (invariant: wl_count < 32 after every word)
        if (wl_count > 0) {
            __syncwarp();
            resolveBatch(wl_count);
        }
    }
    #undef lt
}

// Thin standalone wrapper: one cooperative launch running only the PSE.
template <typename T, int BLOCK_SIZE, int IPT, bool INCL = false>
__global__
void apsepKernelSPT(
        const T* __restrict__   d_in,
        T*                      d_out,
        int                     n,
        int                     num_blocks,
        int                     M,
        int                     leaf_offset,
        unsigned* __restrict__  d_unres,
        T* __restrict__         d_block_mins,
        T* __restrict__         d_block_warp_mins,
        T* __restrict__         d_tree,
        T* __restrict__         d_prefix_min,
        unsigned* __restrict__  d_p3_next,
        unsigned* __restrict__  d_p1_next)
{
    apsepDeviceSPT<T, BLOCK_SIZE, IPT, INCL>(
        cg::this_grid(), d_in, d_out, n, num_blocks, M, leaf_offset,
        d_unres, d_block_mins, d_block_warp_mins, d_tree, d_prefix_min,
        d_p3_next, d_p1_next);
}

template <typename T, int BLOCK_SIZE = 128, int IPT = 4>
struct SPTScratch {
    unsigned* d_unres       = nullptr;  // 1 bit/element: needs inter-block lookup
    T*    d_block_mins      = nullptr;
    T*    d_block_warp_mins = nullptr;
    T*    d_tree            = nullptr;
    T*    d_prefix_min      = nullptr;  // prefix_min[b] = min(block_min[0..b-1])
    unsigned* d_p3_next     = nullptr;  // Phase 3 ticket counter
    unsigned* d_p1_next     = nullptr;  // Phase 1 tile ticket counter
    int   num_blocks        = 0;
    int   M                 = 0;
    int   leaf_offset       = 0;
    int   num_phys          = 0;
};

template <typename T, int BLOCK_SIZE = 128, int IPT = 4, bool INCL = false>
SPTScratch<T, BLOCK_SIZE, IPT> allocSPTScratch(int n) {
    constexpr int B = BLOCK_SIZE * IPT;
    constexpr int W = B / 32;

    SPTScratch<T, BLOCK_SIZE, IPT> s;
    s.num_blocks  = (n + B - 1) / B;
    s.M           = nextPow2(s.num_blocks);
    s.leaf_offset = s.M - 1;

    // Determine physical block count: occupancy x SM count
    int blocks_per_sm = 0;
    gpuAssert(cudaOccupancyMaxActiveBlocksPerMultiprocessor(
        &blocks_per_sm,
        apsepKernelSPT<T, BLOCK_SIZE, IPT, INCL>,
        BLOCK_SIZE, 0));
    cudaDeviceProp prop;
    gpuAssert(cudaGetDeviceProperties(&prop, 0));
    s.num_phys = blocks_per_sm * prop.multiProcessorCount;

    gpuAssert(cudaMalloc(&s.d_unres,           (size_t)s.num_blocks * (B / 32) * sizeof(unsigned)));
    gpuAssert(cudaMalloc(&s.d_block_mins,      (size_t)s.num_blocks      * sizeof(T)));
    gpuAssert(cudaMalloc(&s.d_block_warp_mins, (size_t)s.num_blocks * W  * sizeof(T)));
    gpuAssert(cudaMalloc(&s.d_tree,            (size_t)(2 * s.M - 1)     * sizeof(T)));
    gpuAssert(cudaMalloc(&s.d_prefix_min,      (size_t)s.num_blocks      * sizeof(T)));
    gpuAssert(cudaMalloc(&s.d_p3_next,         sizeof(unsigned)));
    gpuAssert(cudaMalloc(&s.d_p1_next,         sizeof(unsigned)));
    // Phase 1 draws tickets before any in-kernel reset can run, so the
    // counter must start at zero; afterwards the kernel self-resets it.
    gpuAssert(cudaMemset(s.d_p1_next, 0, sizeof(unsigned)));
    return s;
}

template <typename T, int BLOCK_SIZE = 128, int IPT = 4>
void freeSPTScratch(SPTScratch<T, BLOCK_SIZE, IPT>& s) {
    cudaFree(s.d_unres);
    cudaFree(s.d_block_mins);
    cudaFree(s.d_block_warp_mins);
    cudaFree(s.d_tree);
    cudaFree(s.d_prefix_min);
    cudaFree(s.d_p3_next);
    cudaFree(s.d_p1_next);
    s = SPTScratch<T, BLOCK_SIZE, IPT>{};
}

template <typename T, int BLOCK_SIZE = 128, int IPT = 4, bool INCL = false>
void runSPT(const T* d_in, T* d_out, int n, SPTScratch<T, BLOCK_SIZE, IPT>& s) {
    if (n <= 0) return;

    void* args[] = {
        (void*)&d_in, (void*)&d_out, (void*)&n,
        (void*)&s.num_blocks, (void*)&s.M, (void*)&s.leaf_offset,
        (void*)&s.d_unres, (void*)&s.d_block_mins,
        (void*)&s.d_block_warp_mins, (void*)&s.d_tree,
        (void*)&s.d_prefix_min, (void*)&s.d_p3_next, (void*)&s.d_p1_next
    };
    gpuAssert(cudaLaunchCooperativeKernel(
        (void*)apsepKernelSPT<T, BLOCK_SIZE, IPT, INCL>,
        s.num_phys, BLOCK_SIZE, args, 0, nullptr));
    gpuAssert(cudaGetLastError());
}

template <typename T, int BLOCK_SIZE = 128, int IPT = 4, bool INCL = false>
void launchSPT(const T* d_in, T* d_out, int n) {
    if (n <= 0) return;
    SPTScratch<T, BLOCK_SIZE, IPT> s = allocSPTScratch<T, BLOCK_SIZE, IPT, INCL>(n);
    runSPT<T, BLOCK_SIZE, IPT, INCL>(d_in, d_out, n, s);
    gpuAssert(cudaDeviceSynchronize());
    freeSPTScratch<T, BLOCK_SIZE, IPT>(s);
}
