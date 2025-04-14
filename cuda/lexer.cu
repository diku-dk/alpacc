#include <iostream>
#include <vector>
#include <assert.h>
#include <cuda_runtime.h>
#include <cstdint>
#include <math.h>
#include <cstdint>
#define gpuAssert(x) _gpuAssert(x, __FILE__, __LINE__)

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

template<typename T>
struct State {
  T aggregate;
  T prefix;
  Status status = Invalid;
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
      values[tid] = is_not_aggregate ? const_cast<T*>(values)[tid] : op(values[tid - h], values[tid]);
      statuses[tid] = combine(statuses[tid - h], statuses[tid]);
    }
  }
}

template<typename T, typename I, typename OP, I ITEMS_PER_THREAD>
__device__ inline T
decoupledLookbackScanNoWrite(volatile State<T>* states,
                             volatile T* shmem,
                             OP op,
                             const T ne,
                             unsigned int dyn_idx) {
  volatile __shared__ T values[WARP];
  volatile __shared__ Status statuses[WARP];
  volatile __shared__ T shmem_prefix;
  const unsigned char lane = threadIdx.x & (WARP - 1);
  const bool is_first = threadIdx.x == 0;

  T aggregate = shmem[ITEMS_PER_THREAD * blockDim.x - 1];

  if (is_first) {
    states[dyn_idx].aggregate = aggregate;
  }
    
  if (dyn_idx == 0 && is_first) {
    states[dyn_idx].prefix = aggregate;
  }
    
  __threadfence();
  if (dyn_idx == 0 && is_first) {
    states[dyn_idx].status = Prefix;
  } else if (is_first) {
    states[dyn_idx].status = Aggregate;
  }

  T prefix = ne;
  if (threadIdx.x < WARP && dyn_idx != 0) {
    I lookback_idx = threadIdx.x + dyn_idx;
    I lookback_warp = WARP;
    Status status = Aggregate;
    do {
      if (lookback_warp <= lookback_idx) {
        I idx = lookback_idx - lookback_warp;
        status = states[idx].status;
        statuses[threadIdx.x] = status;
        values[threadIdx.x] = status == Prefix ? states[idx].prefix : states[idx].aggregate;
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
    states[dyn_idx].prefix = op(prefix, aggregate);
    __threadfence();
    states[dyn_idx].status = Prefix;
  }
    
  return shmem_prefix;
}

template<typename T, typename I, typename OP, I ITEMS_PER_THREAD>
__device__ inline void
decoupledLookbackScan(volatile State<T>* states,
                      volatile T* shmem,
                      OP op,
                      const T ne,
                      uint32_t dyn_idx) {
  volatile __shared__ T values[WARP];
  volatile __shared__ Status statuses[WARP];
  volatile __shared__ T shmem_prefix;
  const uint8_t lane = threadIdx.x & (WARP - 1);
  const bool is_first = threadIdx.x == 0;

  T aggregate = shmem[ITEMS_PER_THREAD * blockDim.x - 1];

  if (is_first) {
    states[dyn_idx].aggregate = aggregate;
    states[dyn_idx].status = Invalid;
  }
    
  if (dyn_idx == 0 && is_first) {
    states[dyn_idx].prefix = aggregate;
  }
    
  __threadfence();
  if (dyn_idx == 0 && is_first) {
    states[dyn_idx].status = Prefix;
  } else if (is_first) {
    states[dyn_idx].status = Aggregate;
  }

  T prefix = ne;
  if (threadIdx.x < WARP && dyn_idx != 0) {
    I lookback_idx = threadIdx.x + dyn_idx;
    I lookback_warp = WARP;
    Status status = Aggregate;
    do {
      if (lookback_warp <= lookback_idx) {
        I idx = lookback_idx - lookback_warp;
        status = states[idx].status;
        statuses[threadIdx.x] = status;
        values[threadIdx.x] = status == Prefix ? states[idx].prefix : states[idx].aggregate;
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
    states[dyn_idx].prefix = op(prefix, aggregate);
    __threadfence();
    states[dyn_idx].status = Prefix;
  }
    
  prefix = shmem_prefix;
  const I offset = threadIdx.x * ITEMS_PER_THREAD;
  const I upper = offset + ITEMS_PER_THREAD;
#pragma unroll
  for (I lid = offset; lid < upper; lid++) {
    shmem[lid] = op(prefix, shmem[lid]);
  }
  __syncthreads();
}

template<typename T, typename I, typename OP, I ITEMS_PER_THREAD>
__device__ inline void
scan(volatile T* block,
     volatile T* block_aux,
     volatile State<T>* states,
     OP op,
     const T ne,
     uint32_t dyn_idx) {
    
  scanBlock<T, I, OP, ITEMS_PER_THREAD>(block, block_aux, op);

  decoupledLookbackScan<T, I, OP, ITEMS_PER_THREAD>(states, block, op, ne, dyn_idx);
}

__device__ __host__ __forceinline__ state_t get_index(state_t state) {
  return (state & ENDO_MASK) >> ENDO_OFFSET;
}

__device__ __host__ __forceinline__ token_t get_token(state_t state) {
  return (state & TOKEN_MASK) >> TOKEN_OFFSET;
}

__device__ __host__ __forceinline__ bool is_produce(state_t state) {
  return (state & PRODUCE_MASK) >> PRODUCE_OFFSET;
}

template<typename I>
struct LexerCtx {
  state_t* d_to_state;
  state_t* d_compose;
  unsigned char* d_in;
  token_t* d_token_out;
  I* d_index_out;
  bool* d_accept;
  volatile State<state_t>* d_state_states;
  volatile State<I>* d_index_states;
  const I NUM_BLOCKS;
  const I SIZE;
  volatile unsigned int* d_dyn_block_index;
  volatile I* d_new_size;
  volatile bool* d_is_valid;

  LexerCtx(const I size,
           const I num_blocks) : SIZE(size),
                                 NUM_BLOCKS(num_blocks) {
    gpuAssert(cudaMalloc(&d_to_state, sizeof(h_to_state)));
    cudaMemcpy(d_to_state, h_to_state, sizeof(h_to_state),
                 cudaMemcpyHostToDevice);
    gpuAssert(cudaMalloc(&d_compose, sizeof(h_compose)));
    cudaMemcpy(d_compose, h_compose, sizeof(h_compose),
                 cudaMemcpyHostToDevice);
    gpuAssert(cudaMalloc(&d_accept, sizeof(h_accept)));
    cudaMemcpy(d_accept, h_accept, sizeof(h_accept),
                 cudaMemcpyHostToDevice);
    gpuAssert(cudaMalloc((void**)&d_in, SIZE * sizeof(unsigned char)));
    gpuAssert(cudaMalloc((void**)&d_token_out, SIZE * sizeof(token_t)));
    gpuAssert(cudaMalloc((void**)&d_index_out, SIZE * sizeof(I)));
    
    I STATE_STATES_BYTES = NUM_BLOCKS * sizeof(State<state_t>);
    I INDEX_STATES_BYTES = NUM_BLOCKS * sizeof(State<I>);
      
    gpuAssert(cudaMalloc((void**)&d_index_states, INDEX_STATES_BYTES));
    gpuAssert(cudaMalloc((void**)&d_state_states, STATE_STATES_BYTES));

    gpuAssert(cudaMalloc((void**)&d_dyn_block_index, sizeof(unsigned char)));
    gpuAssert(cudaMalloc((void**)&d_new_size, sizeof(I)));
    gpuAssert(cudaMalloc((void**)&d_is_valid, sizeof(bool)));

    cudaMemset((void*)d_dyn_block_index, 0, sizeof(unsigned char));
    cudaMemset((void*)d_new_size, 0, sizeof(I));
    cudaMemset((void*)d_is_valid, false, sizeof(bool));
  }

  void cleanup() {
    if (d_to_state) cudaFree(d_to_state);
    if (d_compose) cudaFree(d_compose);
    if (d_accept) cudaFree(d_accept);
    if (d_in) cudaFree(d_in);
    if (d_token_out) cudaFree(d_token_out);
    if (d_index_out) cudaFree(d_index_out);
    if (d_index_states) cudaFree((void*)d_index_states);
    if (d_state_states) cudaFree((void*)d_state_states);
    if (d_dyn_block_index) cudaFree((void*)d_dyn_block_index);
    if (d_new_size) cudaFree((void*)d_new_size);
    if (d_is_valid) cudaFree((void*)d_is_valid);
  }

  __device__ __host__ __forceinline__
  state_t operator()(const state_t &a, const state_t &b) const {
    return d_compose[get_index(b) * NUM_STATES + get_index(a)];
  }

  __device__ __host__ __forceinline__
  state_t operator()(const volatile state_t &a, const volatile state_t &b) const {
    return d_compose[get_index(b) * NUM_STATES + get_index(a)];
  }

  __device__ __host__ __forceinline__
  state_t to_state(const char &a) const {
    return d_to_state[a];
  }

  void reset_dyn_index() const {
    cudaMemset((void*)d_dyn_block_index, 0, sizeof(unsigned int));
  }

  void reset_is_valid() const {
    cudaMemset((void*)d_is_valid, false, sizeof(bool));
  }

  void reset_new_size() const {
    cudaMemset(d_new_size, 0, sizeof(I));
  }

  bool get_is_valid() const {
    bool h_is_valid = false;
    gpuAssert(cudaMemcpy(&h_is_valid, (const void*) d_is_valid, sizeof(bool), cudaMemcpyDeviceToHost));
    return h_is_valid;
  }

  I get_new_size() const {
    I h_new_size = I();
    gpuAssert(cudaMemcpy(&h_new_size, (const void*) d_new_size, sizeof(I), cudaMemcpyDeviceToHost));
    return h_new_size;
  }

  void copy_out(token_t* tokens, I* indices) const {
    I s = get_new_size();
    gpuAssert(cudaMemcpy(tokens, d_token_out, s * sizeof(token_t), cudaMemcpyDeviceToHost));
    gpuAssert(cudaMemcpy(indices, d_index_out, s * sizeof(I), cudaMemcpyDeviceToHost));
  }
};

template<typename I>
struct Add {
  __device__ __forceinline__ I operator()(I a, I b) const {
    return a + b;
  }
};

template<typename I, I BLOCK_SIZE, I ITEMS_PER_THREAD>
__global__ void
lexer(LexerCtx<I> ctx) {
  volatile __shared__ state_t states[ITEMS_PER_THREAD * BLOCK_SIZE];
  volatile __shared__ I indices[ITEMS_PER_THREAD * BLOCK_SIZE];
  volatile __shared__ I indices_aux[BLOCK_SIZE];
  __shared__ state_t next_block_first_state;
  volatile state_t* states_aux = (volatile state_t*) indices;
  const I REG_MEM = 1 + ITEMS_PER_THREAD / sizeof(unsigned long long);
  unsigned long long copy_reg[REG_MEM];
  unsigned char *chars_reg = (unsigned char*) copy_reg;
  unsigned int is_produce_state = 0;

  unsigned int dyn_index = dynamicIndex(ctx.d_dyn_block_index);
  I glb_offs = dyn_index * BLOCK_SIZE * ITEMS_PER_THREAD;
    
  states_aux[threadIdx.x] = ctx.to_state(threadIdx.x);
    
  if (threadIdx.x == I()) {
    next_block_first_state = IDENTITY;
  }

  __syncthreads();

#pragma unroll
  for (I i = 0; i < REG_MEM; i++) {
    I uint64_lid = i * blockDim.x + threadIdx.x;
    I lid = sizeof(unsigned long long) * uint64_lid;
    I gid = glb_offs + lid;
    if (gid + sizeof(unsigned long long) < ctx.SIZE) {
      copy_reg[i] = *((unsigned long long*) (gid + (unsigned char*) ctx.d_in));
    } else {
      for (I j = 0; j < sizeof(unsigned long long); j++) {
        I loc_gid = gid + j;
        if (loc_gid < ctx.SIZE) {
          chars_reg[sizeof(unsigned long long) * i + j] = ctx.d_in[loc_gid];
        }
      }
    }
  }
    
#pragma unroll
  for (I i = 0; i < REG_MEM; i++) {
    I lid = i * blockDim.x + threadIdx.x;
    I _gid = glb_offs + sizeof(unsigned long long) * lid;
    for (I j = 0; j < sizeof(unsigned long long); j++) {
      I gid = _gid + j;
      I lid_off = sizeof(unsigned long long) * lid + j;
      I reg_off = sizeof(unsigned long long) * i + j;
      bool is_in_block = lid_off < ITEMS_PER_THREAD * BLOCK_SIZE; 
      if (gid < ctx.SIZE && is_in_block) {
        states[lid_off] = states_aux[chars_reg[reg_off]];
      } else if (is_in_block) {
        states[lid_off] = IDENTITY;
      } else if (lid_off == ITEMS_PER_THREAD * BLOCK_SIZE) {
        next_block_first_state = states_aux[chars_reg[reg_off]];
      }
    }
  }

  __syncthreads();

  scan<state_t, I, LexerCtx<I>, ITEMS_PER_THREAD>(states, states_aux, ctx.d_state_states, ctx, IDENTITY, dyn_index);

#pragma unroll
  for (I i = 0; i < ITEMS_PER_THREAD; i++) {
    I lid = i * blockDim.x + threadIdx.x;
    I gid = glb_offs + lid;
    bool temp = false;
    if (gid < ctx.SIZE) {
      temp = gid != 0 && is_produce(states[lid]);
    }
    is_produce_state |= temp << i;
    indices[lid] = temp;
  }

  __syncthreads();

  scanBlock<I, I, Add<I>, ITEMS_PER_THREAD>(indices, indices_aux, Add<I>());

  I prefix = decoupledLookbackScanNoWrite<I, I, Add<I>, ITEMS_PER_THREAD>(ctx.d_index_states, indices, Add<I>(), I(), dyn_index);

#pragma unroll
  for (I i = 0; i < ITEMS_PER_THREAD; i++) {
    I lid = blockDim.x * i + threadIdx.x;
    I gid = glb_offs + lid;
    if (gid < ctx.SIZE && ((is_produce_state >> i) & 1)) {
      I offset = Add<I>()(prefix, indices[lid]) - 1;
      ctx.d_index_out[offset] = gid;
      if (lid == 0) {
        ctx.d_token_out[offset] = get_token(states[lid]);
      } else {
        ctx.d_token_out[offset] = get_token(ctx.d_state_states[dyn_index - 1].prefix);
      }
    }
  }
    
  if (dyn_index == ctx.NUM_BLOCKS - 1 && threadIdx.x == blockDim.x - 1) {
    *ctx.d_new_size = Add<I>()(prefix, indices[ITEMS_PER_THREAD * BLOCK_SIZE - 1]);
    *ctx.d_is_valid = ctx.d_accept[states[ITEMS_PER_THREAD * BLOCK_SIZE - 1]];
  }
}

int main(int32_t argc, char *argv[]) {
  using index = unsigned int;
  const index size = 14;
  const index BLOCK_SIZE = 256;
  const index ITEMS_PER_THREAD = 31;
  const index NUM_BLOCKS = (size + BLOCK_SIZE * ITEMS_PER_THREAD - 1) / (BLOCK_SIZE * ITEMS_PER_THREAD);
  LexerCtx ctx = LexerCtx<index>(size, NUM_BLOCKS);
  const unsigned char input[14] = {'t', 'e', 's', 't', ' ', 't', 'e', 's', 't', ' ', 't', 'e', 's', 't'};

  gpuAssert(cudaMemcpy(ctx.d_in, input, sizeof(input), cudaMemcpyHostToDevice));
  lexer<index, BLOCK_SIZE, ITEMS_PER_THREAD><<<NUM_BLOCKS, BLOCK_SIZE>>>(ctx);
  size_t s = ctx.get_new_size();
  token_t* tokens = (token_t*) malloc(s * sizeof(token_t));
  index* indices = (index*) malloc(s * sizeof(index));
  ctx.copy_out(tokens, indices);

  for (size_t i = 0; i < s; i++) {
    printf("%i, ", tokens[i]);
  }
  printf("\n");
  for (size_t i = 0; i < s; i++) {
    printf("%llu, ", indices[i]);
  }
  
  ctx.cleanup();
  return 0;
}

