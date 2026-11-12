#include <iostream>
#include <assert.h>
#include <cuda_runtime.h>
#include <stdint.h>
#include <unistd.h>
#define gpuAssert(x) _gpuAssert(x, __FILE__, __LINE__)
#define numBlocks(size, block_size, items_per_thread) std::max<size_t>(1, (size + block_size * items_per_thread - 1) / (block_size * items_per_thread))


int _gpuAssert(cudaError_t code, const char *fname, int lineno) {
  if(code != cudaSuccess) {
    printf("GPU Error: %s, File: %s, Line: %i\n", cudaGetErrorString(code), fname, lineno);
    fflush(stdout);
    return -1;
  }
  return 0;
}

void compute_descriptors(float* measurements, size_t size, size_t bytes) {
  double sample_mean = 0;
  double sample_variance = 0;
  double sample_gbps = 0;
  double factor = bytes / (1000 * size);

  for (size_t i = 0; i < size; i++) {
    double diff = max(1e3 * measurements[i], 0.5);
    sample_mean += diff / size;
    sample_variance += (diff * diff) / size;
    sample_gbps += factor / diff;
  }
  double sample_std = sqrt(sample_variance);
  double bound = (0.95 * sample_std) / sqrt(size);

  printf("%.0lfμs ", sample_mean);
  printf("(95%% CI: [%.1lfμs, %.1lfμs]); ", sample_mean - bound, sample_mean + bound);
  printf("%.0lfGB/s\n", sample_gbps);
}

const uint8_t LG_WARP = 5;
const uint8_t WARP = 1 << LG_WARP;

// ---------------------------------------------------------------------------
// Blocked I/O helpers: vectorized global↔register↔shared transfers.
//
// The key optimisation for sub-64-bit element types (e.g. uint8_t): each
// thread issues one uint64_t load per VEC_PER_THREAD register slot instead
// of ITEMS_PER_THREAD single-byte loads, reducing instruction count and
// making coalescing explicit.
//
// Naming convention:
//   glbToReg      – global → registers (packed as VEC = uint64_t)
//   regToShmem    – registers (packed) → shared memory (unpacked as T)
//   glbToShmem    – global → shared (vectorized, combined; adds __syncthreads)
//   shmemToGlb    – shared → global (element-wise; adds __syncthreads)
//   glbToShmemCpy – naive scalar fallback (kept for types where sizeof(T)==sizeof(VEC))
//
// Template parameters:
//   T              – element type (e.g. uint8_t, uint32_t)
//   VEC            – vector load type (e.g. uint64_t); must be >= sizeof(T)
//   BLOCK_SIZE     – threads per block (compile-time)
//   ITEMS_PER_THREAD – logical elements per thread
//   I              – index type (e.g. uint32_t)
//
// VEC_PER_THREAD = ceil(ITEMS_PER_THREAD * sizeof(T) / sizeof(VEC)):
//   number of VEC-sized register slots per thread.
// ---------------------------------------------------------------------------

// Number of VEC slots needed per thread to hold ITEMS_PER_THREAD elements of T.
template<typename T, typename VEC, uint32_t ITEMS_PER_THREAD>
__host__ __device__ constexpr uint32_t vecPerThread() {
    return (ITEMS_PER_THREAD * (uint32_t)sizeof(T) + (uint32_t)sizeof(VEC) - 1)
           / (uint32_t)sizeof(VEC);
}

// Load ITEMS_PER_THREAD elements of T per thread from global memory into
// VEC-sized register slots (reg[VEC_PER_THREAD]).  Out-of-bounds positions
// leave the corresponding bytes in `reg` uninitialised (callers guard with
// `gid < size` before use, or use the shmem variant which fills `ne`).
template<typename T, typename VEC, uint32_t BLOCK_SIZE, uint32_t ITEMS_PER_THREAD, typename I>
__device__ __forceinline__ void
glbToReg(I glb_offs, I size, const T* __restrict__ d_src,
          VEC reg[vecPerThread<T, VEC, ITEMS_PER_THREAD>()])
{
    constexpr uint32_t VPT    = vecPerThread<T, VEC, ITEMS_PER_THREAD>();
    constexpr uint32_t EPV    = (uint32_t)sizeof(VEC) / (uint32_t)sizeof(T); // elements per VEC
    const T* __restrict__ src = d_src + glb_offs;
    const I n_remaining       = size - glb_offs;    // elements left from our start
#pragma unroll
    for (uint32_t v = 0; v < VPT; v++) {
        // Global index of the first element in this VEC slot for this thread.
        I elem0 = (I)(v * BLOCK_SIZE + threadIdx.x) * EPV;
        if (elem0 + EPV <= n_remaining) {
            // Full vector load: all EPV elements are in bounds.
            reg[v] = *reinterpret_cast<const VEC*>(src + elem0);
        } else {
            // Partial: byte-by-byte, guarded.
            uint8_t* bytes = reinterpret_cast<uint8_t*>(&reg[v]);
#pragma unroll
            for (uint32_t b = 0; b < sizeof(VEC); b++) {
                I gid = elem0 + b / (uint32_t)sizeof(T);
                bytes[b] = (gid < n_remaining)
                    ? reinterpret_cast<const uint8_t*>(src + elem0)[b]
                    : 0;
            }
        }
    }
}

// Unpack VEC register slots into shared memory as T elements.
// Writes ITEMS_PER_THREAD entries at logical positions [threadIdx.x*IPT ..
// threadIdx.x*IPT + IPT) in a striped layout compatible with glbToReg above.
// No __syncthreads: caller must synchronise before reading shmem.
template<typename T, typename VEC, uint32_t BLOCK_SIZE, uint32_t ITEMS_PER_THREAD>
__device__ __forceinline__ void
regToShmem(const VEC reg[vecPerThread<T, VEC, ITEMS_PER_THREAD>()],
            volatile T shmem[BLOCK_SIZE * ITEMS_PER_THREAD])
{
    constexpr uint32_t VPT = vecPerThread<T, VEC, ITEMS_PER_THREAD>();
    constexpr uint32_t EPV = (uint32_t)sizeof(VEC) / (uint32_t)sizeof(T);
#pragma unroll
    for (uint32_t v = 0; v < VPT; v++) {
        const T* elems = reinterpret_cast<const T*>(&reg[v]);
#pragma unroll
        for (uint32_t e = 0; e < EPV; e++) {
            uint32_t lid = (v * BLOCK_SIZE + threadIdx.x) * EPV + e;
            if (lid < BLOCK_SIZE * ITEMS_PER_THREAD)
                shmem[lid] = elems[e];
        }
    }
}

// Vectorized global → shared: combines glbToReg + regToShmem, fills
// out-of-bounds positions with `ne`, then __syncthreads.
template<typename T, typename VEC, uint32_t BLOCK_SIZE, uint32_t ITEMS_PER_THREAD, typename I>
__device__ __forceinline__ void
glbToShmem(I glb_offs, I size, T ne,
            const T* __restrict__ d_src,
            volatile T shmem[BLOCK_SIZE * ITEMS_PER_THREAD])
{
    constexpr uint32_t VPT = vecPerThread<T, VEC, ITEMS_PER_THREAD>();
    VEC reg[VPT];
    glbToReg<T, VEC, BLOCK_SIZE, ITEMS_PER_THREAD>(glb_offs, size, d_src, reg);
    regToShmem<T, VEC, BLOCK_SIZE, ITEMS_PER_THREAD>(reg, shmem);
    // regToShmem writes in a vector-striped layout while the ne-fill below is
    // element-striped, so the same OOB slot is touched by two different
    // threads; without this barrier the zero-fill and the ne-fill race.
    __syncthreads();
    // Fill out-of-bounds with ne (only the partial tail thread needs this).
#pragma unroll
    for (uint32_t i = 0; i < ITEMS_PER_THREAD; i++) {
        uint32_t lid = i * BLOCK_SIZE + threadIdx.x;
        I gid = glb_offs + lid;
        if (gid >= size) shmem[lid] = ne;
    }
    __syncthreads();
}

// Scalar global → shared (naive, for types where sizeof(T) == sizeof(VEC)
// or when vectorization is not beneficial).  Adds __syncthreads.
template<typename T, typename I, uint32_t ITEMS_PER_THREAD>
__device__ __forceinline__ void
glbToShmemCpy(I glb_offs, I size, T ne,
               const T* __restrict__ d_src,
               volatile T* shmem)
{
#pragma unroll
    for (uint32_t i = 0; i < ITEMS_PER_THREAD; i++) {
        uint32_t lid = i * blockDim.x + threadIdx.x;
        I gid = glb_offs + lid;
        shmem[lid] = (gid < size) ? d_src[gid] : ne;
    }
    __syncthreads();
}

// Coalesced shared → global write of `count` contiguous T elements starting
// at element offset `base` in d_dst.  Packs sizeof(VEC)/sizeof(T) elements
// into one VEC register per store so sub-VEC types (e.g. uint8_t terminals)
// issue wide stores instead of per-element ones.  `base` is not VEC-aligned
// in general, so a scalar head runs up to the first aligned position and a
// scalar tail covers the remainder (d_dst itself is cudaMalloc-aligned).
// No __syncthreads: the caller must synchronise before (shmem visible) and
// after (before reusing shmem).
template<typename T, typename VEC, uint32_t BLOCK_SIZE, typename I>
__device__ __forceinline__ void
shmemToGlbVec(I base, I count, T* d_dst, const volatile T* shmem)
{
    constexpr I EPV = (I)(sizeof(VEC) / sizeof(T));
    const I head = min(count, (EPV - (I)(base % EPV)) % EPV);
    if (threadIdx.x < head)
        d_dst[base + (I)threadIdx.x] = shmem[threadIdx.x];
    const I n_vec = (count - head) / EPV;
    for (I v = (I)threadIdx.x; v < n_vec; v += (I)BLOCK_SIZE) {
        VEC tmp;
        T* elems = reinterpret_cast<T*>(&tmp);
#pragma unroll
        for (I e = 0; e < EPV; e++)
            elems[e] = shmem[head + v * EPV + e];
        *reinterpret_cast<VEC*>(d_dst + base + head + v * EPV) = tmp;
    }
    const I tail_start = head + n_vec * EPV;
    if ((I)threadIdx.x < count - tail_start)
        d_dst[base + tail_start + (I)threadIdx.x] = shmem[tail_start + (I)threadIdx.x];
}

// Shared → global (element-wise, no packing needed).  Adds __syncthreads.
template<typename T, typename I, uint32_t ITEMS_PER_THREAD>
__device__ __forceinline__ void
shmemToGlbCpy(I glb_offs, I size,
               T* d_dst,
               const volatile T* shmem)
{
#pragma unroll
    for (uint32_t i = 0; i < ITEMS_PER_THREAD; i++) {
        uint32_t lid = i * blockDim.x + threadIdx.x;
        I gid = glb_offs + lid;
        if (gid < size) d_dst[gid] = shmem[lid];
    }
    __syncthreads();
}
