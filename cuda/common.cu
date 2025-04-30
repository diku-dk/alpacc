#include <iostream>
#include <assert.h>
#include <cuda_runtime.h>
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

const unsigned char LG_WARP = 5;
const unsigned char WARP = 1 << LG_WARP;

template<typename T, typename I, I ITEMS_PER_THREAD>
__device__ inline void
glbToShmemCpy(const I glb_offs,
              const I size,
              const T ne,
              T* d_read,
              volatile T* shmem_write) {
#pragma unroll
  for (I i = 0; i < ITEMS_PER_THREAD; i++) {
    I lid = i * blockDim.x + threadIdx.x;
    I gid = glb_offs + lid;
    shmem_write[lid] = gid < size ? d_read[gid] : ne;
  }
  __syncthreads();
}

template<typename T, typename I, I ITEMS_PER_THREAD>
__device__ inline void
shmemToGlbCpy(const I glb_offs,
              const I size,
              T* d_write,
              volatile T* shmem_read) {
#pragma unroll
  for (I i = 0; i < ITEMS_PER_THREAD; i++) {
    I lid = blockDim.x * i + threadIdx.x;
    I gid = glb_offs + lid;
    if (gid < size)
      d_write[gid] = shmem_read[lid];
  }
  __syncthreads();
}

template<typename T, typename I, typename OP, I ITEMS_PER_THREAD>
__device__ inline void
scanThread(volatile T* shmem,
           volatile T* shmem_aux,
           OP op) {
  const I offset = threadIdx.x * ITEMS_PER_THREAD;
  const I upper = offset + ITEMS_PER_THREAD;
  T acc = shmem[offset];
#pragma unroll
  for (I lid = offset + 1; lid < upper; lid++) {
    T tmp = shmem[lid];
    acc = op(acc, tmp);
    shmem[lid] = acc;
  }
  shmem_aux[threadIdx.x] = acc;
  __syncthreads();
}

template<typename T, typename I, typename OP>
__device__ inline T
scanWarp(volatile T* shmem,
         OP op,
         const unsigned char lane) {
  unsigned char h;

#pragma unroll
  for (unsigned char d = 0; d < LG_WARP; d++)
    if ((h = 1 << d) <= lane)
      shmem[threadIdx.x] = op(shmem[threadIdx.x - h], shmem[threadIdx.x]);
    
  return shmem[threadIdx.x];
}

template<typename T, typename I, typename OP>
__device__ inline void
scanBlock(volatile T* shmem,
          OP op) {
  const unsigned char lane = threadIdx.x & (WARP - 1);
  const I warpid = threadIdx.x >> LG_WARP;

  T res = scanWarp<T, I, OP>(shmem, op, lane);
  __syncthreads();

  if (lane == (WARP - 1))
    shmem[warpid] = res;
  __syncthreads();

  if (warpid == 0)
    scanWarp<T, I, OP>(shmem, op, lane);
  __syncthreads();

  if (warpid > 0)
    res = op(shmem[warpid-1], res);
  __syncthreads();

  shmem[threadIdx.x] = res;
  __syncthreads();
}

template<typename T, typename I, typename OP, I ITEMS_PER_THREAD>
__device__ inline void
addAuxBlockScan(volatile T* shmem,
                volatile T* shmem_aux,
                OP op) {
  if (threadIdx.x > 0) {
    const I offset = threadIdx.x * ITEMS_PER_THREAD;
    const I upper = offset + ITEMS_PER_THREAD;
    const T val = shmem_aux[threadIdx.x - 1];
#pragma unroll
    for (I lid = offset; lid < upper; lid++) {
      shmem[lid] = op(val, shmem[lid]);
    }
  }
  __syncthreads();
}

template<typename T, typename I, typename OP, I ITEMS_PER_THREAD>
__device__ inline void
scanBlock(volatile T* block,
          volatile T* block_aux,
          OP op) {
  scanThread<T, I, OP, ITEMS_PER_THREAD>(block, block_aux, op);

  scanBlock<T, I, OP>(block_aux, op);

  addAuxBlockScan<T, I, OP, ITEMS_PER_THREAD>(block, block_aux, op);
}

__device__ inline unsigned int dynamicIndex(volatile unsigned int* dyn_idx_ptr) {
  volatile __shared__ unsigned int dyn_idx;
    
  if (threadIdx.x == 0)
    dyn_idx = atomicAdd(const_cast<unsigned int*>(dyn_idx_ptr), 1);
    
  __syncthreads();
  return dyn_idx;
}

enum Status: unsigned char {
  Invalid = 0,
  Aggregate = 1,
  Prefix = 2,
};

template<typename I, typename T>
struct States {
  T* aggregates;
  T* prefixes;
  Status* statuses;
  I num_blocks;

  States(I num_blocks) : num_blocks(num_blocks) {
    cudaMalloc((void**)&aggregates, num_blocks * sizeof(T));
    cudaMalloc((void**)&prefixes, num_blocks * sizeof(T));
    cudaMalloc((void**)&statuses, num_blocks * sizeof(Status));
    cudaMemset((void*) statuses, Invalid, num_blocks * sizeof(Status));
  }

  States() {
  }

  void cleanUp() {
    if (aggregates) cudaFree((void*) aggregates);
    if (prefixes) cudaFree((void*) prefixes);
    if (statuses) cudaFree((void*) statuses);
  }
};

__device__ inline Status
combine(Status a, Status b) {
  if (b == Aggregate)
    return a;
  return b;
}

template<typename T, typename I, typename OP>
__device__ inline void
scanWarp(volatile T* values,
         volatile Status* statuses,
         OP op,
         const unsigned char lane) {
  unsigned char h;
  const I tid = threadIdx.x;

#pragma unroll
  for (unsigned char d = 0; d < LG_WARP; d++) {
    if ((h = 1 << d) <= lane) {
      bool is_not_aggregate = statuses[tid] != Aggregate;
      values[tid] = is_not_aggregate ? values[tid] : op(values[tid - h], values[tid]);
      statuses[tid] = combine(statuses[tid - h], statuses[tid]);
    }
  }
}

template<typename T, typename I, typename OP, I ITEMS_PER_THREAD>
__device__ inline T
decoupledLookbackScan(volatile States<I, T> states,
                      volatile T* shmem,
                      OP op,
                      T ne,
                      uint32_t dyn_idx,
                      bool write_back = true) {
  volatile __shared__ T values[WARP];
  volatile __shared__ Status statuses[WARP];
  volatile __shared__ T shmem_prefix;
  const uint8_t lane = threadIdx.x & (WARP - 1);
  const bool is_first = threadIdx.x == 0;

  if (is_first) {
    states.statuses[dyn_idx] = Invalid;
  }
  __syncthreads();

  T aggregate = shmem[ITEMS_PER_THREAD * blockDim.x - 1];
  if (is_first) {
    states.aggregates[dyn_idx] = aggregate;
  }
  
  if (dyn_idx == 0 && is_first) {
    states.prefixes[dyn_idx] = aggregate;
  }
  __threadfence();
  if (dyn_idx == 0 && is_first) {
    states.statuses[dyn_idx] = Prefix;
  } else if (is_first) {
    states.statuses[dyn_idx] = Aggregate;
  }

  T prefix = ne;
  if (threadIdx.x < WARP && dyn_idx != 0) {
    I lookback_idx = threadIdx.x + dyn_idx;
    I lookback_warp = WARP;
    Status status = Aggregate;
    do {
      if (lookback_warp <= lookback_idx) {
        I idx = lookback_idx - lookback_warp;
        status = states.statuses[idx];
        statuses[threadIdx.x] = status;
        values[threadIdx.x] = status == Prefix ? states.prefixes[idx] : states.aggregates[idx];
      } else {
        statuses[threadIdx.x] = Aggregate;
        values[threadIdx.x] = ne;
      }

      scanWarp<T, I, OP>(values, statuses, op, lane);

      T result = values[WARP - 1];
      status = statuses[WARP - 1];

      if (status == Invalid)
        continue;
          
      if (is_first) {
        prefix = op(result, prefix);
      }

      lookback_warp += WARP;
    } while (status != Prefix);
  }

  if (is_first) {
    shmem_prefix = prefix;
  }

  __syncthreads();

  if (is_first) {
    states.prefixes[dyn_idx] = op(prefix, aggregate);
    __threadfence();
    states.statuses[dyn_idx] = Prefix;
  }
  
  prefix = shmem_prefix;
  if (write_back) {
    const I offset = threadIdx.x * ITEMS_PER_THREAD;
    const I upper = offset + ITEMS_PER_THREAD;
#pragma unroll
    for (I lid = offset; lid < upper; lid++) {
      shmem[lid] = op(prefix, shmem[lid]);
    }
  }
  __syncthreads();
  return prefix;
}

template<typename T, typename I, typename OP, I ITEMS_PER_THREAD>
__device__ inline T
scan(volatile T* block,
     volatile T* block_aux,
     volatile States<I, T> states,
     OP op,
     T ne,
     uint32_t dyn_idx,
     bool write_back = true) {
    
  scanBlock<T, I, OP, ITEMS_PER_THREAD>(block, block_aux, op);

  return decoupledLookbackScan<T, I, OP, ITEMS_PER_THREAD>(states, block, op, ne, dyn_idx, write_back);
}

template<typename I>
struct Add {
  __device__ __forceinline__ I operator()(I a, I b) const {
    return a + b;
  }
};
