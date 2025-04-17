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
__device__ inline T
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
  return prefix;
}

template<typename T, typename I, typename OP, I ITEMS_PER_THREAD>
__device__ inline T
scan(volatile T* block,
     volatile T* block_aux,
     volatile State<T>* states,
     OP op,
     const T ne,
     uint32_t dyn_idx) {
    
  scanBlock<T, I, OP, ITEMS_PER_THREAD>(block, block_aux, op);

  return decoupledLookbackScan<T, I, OP, ITEMS_PER_THREAD>(states, block, op, ne, dyn_idx);
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

state_t get_index_cpu(state_t state) {
  return (state & ENDO_MASK) >> ENDO_OFFSET;
}

token_t get_token_cpu(state_t state) {
  return (state & TOKEN_MASK) >> TOKEN_OFFSET;
}

bool is_produce_cpu(state_t state) {
  return (state & PRODUCE_MASK) >> PRODUCE_OFFSET;
}

template<typename I, typename J>
struct LexerCtx {
  state_t* d_to_state;
  state_t* d_compose;
  J offset = 0;
  volatile State<state_t>* d_state_states;
  volatile State<I>* d_index_states;
  const I NUM_BLOCKS;
  const I CHUNK_SIZE;
  volatile unsigned int* d_dyn_block_index;
  volatile I* d_new_size;
  volatile state_t* d_last_state;
  state_t init_state = IDENTITY;

  LexerCtx(const I chunk_size,
           const I num_blocks) : CHUNK_SIZE(chunk_size),
                                 NUM_BLOCKS(num_blocks) {
    gpuAssert(cudaMalloc(&d_to_state, sizeof(h_to_state)));
    cudaMemcpy(d_to_state, h_to_state, sizeof(h_to_state),
                 cudaMemcpyHostToDevice);
    gpuAssert(cudaMalloc(&d_compose, sizeof(h_compose)));
    cudaMemcpy(d_compose, h_compose, sizeof(h_compose),
                 cudaMemcpyHostToDevice);
    
    I STATE_STATES_BYTES = NUM_BLOCKS * sizeof(State<state_t>);
    I INDEX_STATES_BYTES = NUM_BLOCKS * sizeof(State<I>);
      
    gpuAssert(cudaMalloc((void**)&d_index_states, INDEX_STATES_BYTES));
    gpuAssert(cudaMalloc((void**)&d_state_states, STATE_STATES_BYTES));

    gpuAssert(cudaMalloc((void**)&d_dyn_block_index, sizeof(unsigned int)));
    gpuAssert(cudaMalloc((void**)&d_new_size, sizeof(I)));
    gpuAssert(cudaMalloc((void**)&d_last_state, sizeof(state_t)));

    cudaMemset((void*)d_dyn_block_index, 0, sizeof(unsigned int));
    cudaMemset((void*)d_new_size, 0, sizeof(I));
    cudaMemset((void*)d_last_state, IDENTITY, sizeof(state_t));
  }

  void cleanup() {
    if (d_to_state) cudaFree(d_to_state);
    if (d_compose) cudaFree(d_compose);
    if (d_index_states) cudaFree((void*)d_index_states);
    if (d_state_states) cudaFree((void*)d_state_states);
    if (d_dyn_block_index) cudaFree((void*)d_dyn_block_index);
    if (d_new_size) cudaFree((void*)d_new_size);
    if (d_last_state) cudaFree((void*)d_last_state);
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

  void reset_last_state() const {
    cudaMemset((void*)d_last_state, false, sizeof(bool));
  }

  void reset_new_size() const {
    cudaMemset(d_new_size, 0, sizeof(I));
  }

  state_t get_last_state() const {
    bool h_last_state = false;
    gpuAssert(cudaMemcpy(&h_last_state, (const void*) d_last_state, sizeof(bool), cudaMemcpyDeviceToHost));
    return h_last_state;
  }

  I tokens_size() const {
    I h_new_size = I();
    gpuAssert(cudaMemcpy(&h_new_size, (const void*) d_new_size, sizeof(I), cudaMemcpyDeviceToHost));
    return h_new_size;
  }
};

template<typename I>
struct Add {
  __device__ __forceinline__ I operator()(I a, I b) const {
    return a + b;
  }
};

template<typename I, typename J, I BLOCK_SIZE, I ITEMS_PER_THREAD>
__global__ void
lexer(LexerCtx<I, J> ctx, const I size, unsigned char* d_string, token_t* d_tokens, J* d_indices) {
  volatile __shared__ state_t states[ITEMS_PER_THREAD * BLOCK_SIZE];
  volatile __shared__ I indices[ITEMS_PER_THREAD * BLOCK_SIZE];
  volatile __shared__ I indices_aux[BLOCK_SIZE];
  volatile state_t* states_aux = (volatile state_t*) indices;
  const I REG_MEM = 1 + ITEMS_PER_THREAD / sizeof(unsigned long long);
  unsigned long long copy_reg[REG_MEM];
  unsigned char *chars_reg = (unsigned char*) copy_reg;
  unsigned int is_produce_state = 0;

  unsigned int dyn_index = dynamicIndex(ctx.d_dyn_block_index);
  I glb_offs = dyn_index * BLOCK_SIZE * ITEMS_PER_THREAD;
    
  states_aux[threadIdx.x] = ctx.to_state(threadIdx.x);

  __syncthreads();

#pragma unroll
  for (I i = 0; i < REG_MEM; i++) {
    I uint64_lid = i * blockDim.x + threadIdx.x;
    I lid = sizeof(unsigned long long) * uint64_lid;
    I gid = glb_offs + lid;
    if (gid + sizeof(unsigned long long) < size) {
      copy_reg[i] = *((unsigned long long*) (gid + (unsigned char*) d_string));
    } else {
      for (I j = 0; j < sizeof(unsigned long long); j++) {
        I loc_gid = gid + j;
        if (loc_gid < size) {
          chars_reg[sizeof(unsigned long long) * i + j] = d_string[loc_gid];
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
      if (gid < size && is_in_block) {
        if (gid != 0) {
          states[lid_off] = states_aux[chars_reg[reg_off]];
        } else {
          states[lid_off] = ctx(ctx.init_state, states_aux[chars_reg[reg_off]]);
        }
      } else if (is_in_block) {
        states[lid_off] = IDENTITY;
      }
    }
  }

  __syncthreads();

  state_t state_prefix = scan<state_t, I, LexerCtx<I, J>, ITEMS_PER_THREAD>(states, states_aux, ctx.d_state_states, ctx, IDENTITY, dyn_index);

#pragma unroll
  for (I i = 0; i < ITEMS_PER_THREAD; i++) {
    I lid = i * blockDim.x + threadIdx.x;
    I gid = glb_offs + lid;
    bool temp = gid < size && is_produce(states[lid]);
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
    if (gid < size && ((is_produce_state >> i) & 1)) {
      I offset = Add<I>()(prefix, indices[lid]) - 1;
      d_indices[offset] = ((J) gid) + ctx.offset;
      if (lid != 0) {
        d_tokens[offset] = get_token(states[lid - 1]);
      } else if (dyn_index != 0) {
        d_tokens[offset] = get_token(state_prefix);
      } else {
        d_tokens[offset] = get_token(ctx.init_state);
      }
    }
  }

  if (dyn_index == gridDim.x - 1 && threadIdx.x == blockDim.x - 1) {
    I new_size = Add<I>()(prefix, indices[ITEMS_PER_THREAD * BLOCK_SIZE - 1]);
    *ctx.d_new_size = new_size;
    *ctx.d_last_state = states[ITEMS_PER_THREAD * BLOCK_SIZE - 1];
  }
}


struct WriteBinary {
  void operator()(size_t i, token_t s) const {
    unsigned char* buffer[sizeof(size_t) + sizeof(token_t)];
    memcpy(buffer, &i, sizeof(size_t));
    memcpy(buffer + sizeof(size_t), &s, sizeof(token_t));
    fwrite(buffer, sizeof(size_t) + sizeof(token_t), 1, stdout);
  }
};

struct WriteAscii {
  void operator()(size_t i, token_t s) const {
    printf("%lu %lu\n" , (size_t) i, (size_t) s);
  }
};


template<typename PRINT>
int lexer_stream(PRINT print) {
  size_t chunk_size = 100 * (1 << 20); // 100MiB
  
  unsigned char* h_string = (unsigned char*) malloc(chunk_size * sizeof(unsigned char));
  token_t* h_tokens = (token_t*) malloc(chunk_size * sizeof(token_t));
  size_t* h_indices = (size_t*) malloc(chunk_size * sizeof(size_t));
  assert(h_string != NULL);
  assert(h_tokens != NULL);
  assert(h_indices != NULL);

  unsigned char* d_string;
  token_t* d_tokens;
  size_t* d_indices;
  gpuAssert(cudaMalloc((void**)&d_string, chunk_size * sizeof(unsigned char)));
  gpuAssert(cudaMalloc((void**)&d_tokens, chunk_size * sizeof(token_t)));
  gpuAssert(cudaMalloc((void**)&d_indices, chunk_size * sizeof(size_t)));

  const unsigned int BLOCK_SIZE = 256;
  const unsigned int ITEMS_PER_THREAD = 31;
  const unsigned int NUM_BLOCKS = (chunk_size + BLOCK_SIZE * ITEMS_PER_THREAD - 1) / (BLOCK_SIZE * ITEMS_PER_THREAD);
  LexerCtx ctx = LexerCtx<unsigned int, size_t>(chunk_size, NUM_BLOCKS);

  size_t prev_index = 0;
  size_t new_size = 0;

  while (true) {
    size_t bytes = fread(h_string, sizeof(unsigned char), chunk_size, stdin);
    if (bytes == 0) {
      if (new_size == 0) {
        break;
      }
      token_t token = get_token_cpu(ctx.get_last_state());
      print(prev_index, token);
      break;
    }

    gpuAssert(cudaMemcpy(d_string, h_string, bytes, cudaMemcpyHostToDevice));

    ctx.reset_dyn_index();
    unsigned int num_blocks = (bytes + BLOCK_SIZE * ITEMS_PER_THREAD - 1) / (BLOCK_SIZE * ITEMS_PER_THREAD);
    lexer<unsigned int, size_t, BLOCK_SIZE, ITEMS_PER_THREAD><<<num_blocks, BLOCK_SIZE>>>(ctx, bytes, d_string, d_tokens, d_indices);
    gpuAssert(cudaDeviceSynchronize());
    gpuAssert(cudaPeekAtLastError());

    new_size = ctx.tokens_size();

    cudaMemcpy(h_tokens, d_tokens, new_size * sizeof(token_t), cudaMemcpyDeviceToHost);
    cudaMemcpy(h_indices, d_indices, new_size * sizeof(size_t), cudaMemcpyDeviceToHost);


    for (size_t i = 0; i < new_size; i++) {
      if (i == 0 ) {
        print(prev_index, h_tokens[i]);
      } else {
        print(h_indices[i - 1], h_tokens[i]);
      }
    }

    prev_index = h_indices[new_size - 1];

    if (bytes != chunk_size) {
      token_t token = get_token_cpu(ctx.get_last_state());
      print(prev_index, token);
      break;
    }

    ctx.init_state = ctx.get_last_state();
    ctx.offset += bytes;
  }

  std::cout << std::flush;

  int success = h_accept[ctx.get_last_state()] ? 0 : -1;
  
  ctx.cleanup();

  free(h_string);
  free(h_tokens);
  free(h_indices);
  cudaFree(d_string);
  cudaFree(d_tokens);
  cudaFree(d_indices);
  return success;
}

int main(int32_t argc, char *argv[]) {
  return lexer_stream<WriteBinary>(WriteBinary());
}


