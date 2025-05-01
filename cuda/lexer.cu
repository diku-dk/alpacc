__device__ __host__ __forceinline__
state_t get_index(state_t state) {
  return (state & ENDO_MASK) >> ENDO_OFFSET;
}

__device__ __host__ __forceinline__
token_t get_token(state_t state) {
  return (state & TOKEN_MASK) >> TOKEN_OFFSET;
}

__device__ __host__ __forceinline__
bool is_produce(state_t state) {
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

template<typename T>
struct TakeRight {
  const T identity = std::numeric_limits<T>::max();

  __device__ __forceinline__ T operator()(T a, T b) const {
    if (b == identity) {
      return a;
    }

    return b;
  }
};

template<typename I, typename J>
struct LexerCtx {

private:
  J offset = 0;
  state_t* d_to_state;
  state_t* d_compose;
  volatile unsigned int* d_dyn_block_index;
  volatile state_t* d_new_last_state;
  volatile state_t* d_old_last_state;
  I* d_new_size;
  volatile J* d_new_last_start;
  volatile J* d_old_last_start;

  void swapLastStart() {
    J h_last_start;
    gpuAssert(cudaMemcpy(&h_last_start, (const void*) d_new_last_start, sizeof(J), cudaMemcpyDeviceToHost));
    gpuAssert(cudaMemcpy((void *) d_new_last_start, (const void*) d_old_last_start, sizeof(J), cudaMemcpyDeviceToDevice));
    gpuAssert(cudaMemcpy((void *) d_old_last_start, &h_last_start, sizeof(J), cudaMemcpyHostToDevice));
  }

  void swapLastState() {
  state_t h_last_state;
  gpuAssert(cudaMemcpy(&h_last_state, (const void*) d_new_last_state, sizeof(state_t), cudaMemcpyDeviceToHost));
  gpuAssert(cudaMemcpy((void *) d_new_last_state, (const void*) d_old_last_state, sizeof(state_t), cudaMemcpyDeviceToDevice));
  gpuAssert(cudaMemcpy((void *) d_old_last_state, &h_last_state, sizeof(state_t), cudaMemcpyHostToDevice));
}

  void resetDynamicIndex() const {
    cudaMemset((void*)d_dyn_block_index, 0, sizeof(unsigned int));
  }

  void updateOffset() {
    offset += CHUNK_SIZE;
  }

  void resetNewSize() const {
    cudaMemset(d_new_size, 0, sizeof(I));
  }

public:
  const I CHUNK_SIZE;
  States<I, state_t> d_state_states;
  States<I, I> d_index_states;
  States<I, I> d_take_right_states;
  TakeRight<I> take_right = TakeRight<I>();

  LexerCtx(const I chunk_size,
           const I block_size,
           const I items_per_thread) : CHUNK_SIZE(chunk_size) {
    I num_blocks = numBlocks(chunk_size, block_size, items_per_thread);
    gpuAssert(cudaMalloc(&d_to_state, sizeof(h_to_state)));
    cudaMemcpy(d_to_state, h_to_state, sizeof(h_to_state),
                 cudaMemcpyHostToDevice);
    gpuAssert(cudaMalloc(&d_compose, sizeof(h_compose)));
    cudaMemcpy(d_compose, h_compose, sizeof(h_compose),
                 cudaMemcpyHostToDevice);

    d_index_states = States<I, I>(num_blocks);
    d_take_right_states = States<I, I>(num_blocks);
    d_state_states = States<I, state_t>(num_blocks);

    gpuAssert(cudaMalloc((void**)&d_dyn_block_index, sizeof(unsigned int)));
    gpuAssert(cudaMalloc((void**)&d_new_size, sizeof(I)));
    gpuAssert(cudaMalloc((void**)&d_new_last_state, sizeof(state_t)));
    gpuAssert(cudaMalloc((void**)&d_old_last_state, sizeof(state_t)));
    gpuAssert(cudaMalloc((void**)&d_new_last_start, sizeof(J)));
    gpuAssert(cudaMalloc((void**)&d_old_last_start, sizeof(J)));

    cudaMemset((void*)d_dyn_block_index, 0, sizeof(unsigned int));
    cudaMemset((void*)d_new_size, I(), sizeof(I));
    cudaMemset((void*)d_new_last_state, IDENTITY, sizeof(state_t));
    cudaMemset((void*)d_old_last_state, IDENTITY, sizeof(state_t));
    cudaMemset((void*)d_new_last_start, J(), sizeof(J));
    cudaMemset((void*)d_old_last_start, J(), sizeof(J));
  }

  void cleanUp() {
    if (d_to_state) cudaFree(d_to_state);
    if (d_new_last_start) cudaFree((void*)d_new_last_start);
    if (d_old_last_start) cudaFree((void*)d_old_last_start);
    if (d_compose) cudaFree(d_compose);
    if (d_dyn_block_index) cudaFree((void*)d_dyn_block_index);
    if (d_new_size) cudaFree((void*)d_new_size);
    if (d_new_last_state) cudaFree((void*)d_new_last_state);
    if (d_old_last_state) cudaFree((void*)d_old_last_state);
    d_index_states.cleanUp();
    d_state_states.cleanUp();
    d_take_right_states.cleanUp();
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
  state_t toState(const char &a) const {
    return d_to_state[a];
  }

  __device__ __host__ __forceinline__
  J addOffset(I i) const {
    return i + offset;
  }

  __device__ __forceinline__
  unsigned int getDynamicIndex() const {
    return dynamicIndex(d_dyn_block_index);
  }

  __device__ __host__ __forceinline__
  void setLastState(state_t state) const {
    *d_new_last_state = state;
  }

  __device__ __host__ __forceinline__
  state_t getLastState() const {
    return *d_old_last_state;
  }

  __device__ __host__ __forceinline__
  void setNewSize(I size) const {
    *d_new_size = size;
  }

  __device__ __host__ __forceinline__
  void setLastStart(J i) const {
    *d_new_last_start = i;
  }

  __device__ __host__ __forceinline__
  J getLastStart() const {
    return *d_old_last_start;
  }

  bool isAccept() const {
    state_t h_last_state;
    gpuAssert(cudaMemcpy(&h_last_state, (const void*) d_new_last_state, sizeof(state_t), cudaMemcpyDeviceToHost));
    return h_accept[get_index_cpu(h_last_state)];
  }

  I tokensSize() const {
    I h_new_size = I();
    gpuAssert(cudaMemcpy(&h_new_size, (const void*) d_new_size, sizeof(I), cudaMemcpyDeviceToHost));
    return h_new_size;
  }

  void update() {
    resetDynamicIndex();
    swapLastStart();
    swapLastState();
    updateOffset();
  }
};

template<typename I, typename J, I BLOCK_SIZE, I ITEMS_PER_THREAD>
__global__ void
lexer(LexerCtx<I, J> ctx, unsigned char* d_string, token_t* d_tokens, J* d_starts, J* d_ends, const I size, const bool is_last_chunk) {
  volatile __shared__ state_t states[ITEMS_PER_THREAD * BLOCK_SIZE];
  volatile __shared__ I indices[ITEMS_PER_THREAD * BLOCK_SIZE];
  volatile __shared__ I indices_aux[BLOCK_SIZE];
  __shared__ state_t next_block_first_state;
  volatile state_t* states_aux = (volatile state_t*) indices;
  const I REG_MEM = 1 + ITEMS_PER_THREAD / sizeof(unsigned long long);
  unsigned long long copy_reg[REG_MEM];
  unsigned char *chars_reg = (unsigned char*) copy_reg;
  unsigned int is_produce_state = 0;

  unsigned int dyn_index = ctx.getDynamicIndex();
  I glb_offs = dyn_index * BLOCK_SIZE * ITEMS_PER_THREAD;

  if (threadIdx.x == I()) {
    next_block_first_state = IDENTITY;
  }

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
          if (gid == 0) {
            states[lid_off] = ctx(ctx.getLastState(), reinterpret_cast<state_t>(ctx.toState(chars_reg[reg_off])));
          } else {
            states[lid_off] = ctx.toState(chars_reg[reg_off]);
          }
      } else if (is_in_block) {
          states[lid_off] = IDENTITY;
      } else if (lid_off == ITEMS_PER_THREAD * BLOCK_SIZE) {
          next_block_first_state = ctx.toState(chars_reg[reg_off]);
      }
    }
  }

  __syncthreads();

  scan<state_t, I, LexerCtx<I, J>, ITEMS_PER_THREAD>(states, states_aux, ctx.d_state_states, ctx, IDENTITY, dyn_index);

#pragma unroll
  for (I i = 0; i < ITEMS_PER_THREAD; i++) {
    I lid = i * blockDim.x + threadIdx.x;
    I gid = glb_offs + lid;
    bool is_next_produce = false;
    if (gid < size) {
      state_t state = states[lid];
#ifdef IGNORE_TOKEN
      bool is_not_ignore = get_token(state) != IGNORE_TOKEN;
#else
      bool is_not_ignore = true;
#endif
      if (lid == ITEMS_PER_THREAD * BLOCK_SIZE - 1) {
        is_next_produce = is_produce(ctx(state, next_block_first_state));
      } else {
        is_next_produce = is_produce(states[lid + 1]);
      }

      if (is_last_chunk) {
        is_next_produce |= gid == size - 1;
        is_next_produce &= is_not_ignore;
      } else {
        is_next_produce &= is_not_ignore && gid != size - 1;
      }

      indices[lid] = is_produce(state) ? gid : ctx.take_right.identity;
    } else {
      indices[lid] = ctx.take_right.identity;
    }
    is_produce_state |= is_next_produce << i;
  }

  __syncthreads();

  scan<I, I, TakeRight<I>, ITEMS_PER_THREAD>(indices, indices_aux, ctx.d_take_right_states, ctx.take_right, ctx.take_right.identity, dyn_index);

  I starts[ITEMS_PER_THREAD];
  volatile __shared__ I last_start;

#pragma unroll
  for (I i = 0; i < ITEMS_PER_THREAD; i++) {
    I lid = i * blockDim.x + threadIdx.x;
    I gid = glb_offs + lid;

    if (gid < size) {
      starts[i] = indices[lid];
      indices[lid] = (is_produce_state >> i) & 1;

      if (gid == size - 1) {
        last_start = starts[i];
      }
      
    } else {
      indices[lid] = 0;
    }
  }

  __syncthreads();

  I prefix = scan<I, I, Add<I>, ITEMS_PER_THREAD>(indices, indices_aux, ctx.d_index_states, Add<I>(), I(), dyn_index, false);

  #pragma unroll
  for (I i = 0; i < ITEMS_PER_THREAD; i++) {
    I lid = blockDim.x * i + threadIdx.x;
    I gid = glb_offs + lid;
    if (gid < size && ((is_produce_state >> i) & 1)) {
      I offset = Add<I>()(prefix, indices[lid]) - 1;
      if (offset == I() && starts[i] == ctx.take_right.identity) {
        d_starts[offset] = ctx.getLastStart();
      } else {
        d_starts[offset] = ctx.addOffset(starts[i]);
      }
      d_ends[offset] = ctx.addOffset(gid + 1);
      d_tokens[offset] = get_token(states[lid]);
    }
  }

  if (dyn_index == gridDim.x - 1 && threadIdx.x == blockDim.x - 1) {
    I new_size = Add<I>()(prefix, indices[ITEMS_PER_THREAD * BLOCK_SIZE - 1]);
    ctx.setNewSize(new_size);
    ctx.setLastState(states[ITEMS_PER_THREAD * BLOCK_SIZE - 1]);
    
    if (last_start != ctx.take_right.identity) {
      ctx.setLastStart(ctx.addOffset(last_start));
    } else {
      ctx.setLastStart(ctx.getLastStart());
    }
  }
}


struct WriteBinary {
  void operator()(size_t i, size_t j, token_t s) const {
    unsigned char* buffer[2 * sizeof(size_t) + sizeof(token_t)];
    memcpy(buffer, &i, sizeof(size_t));
    memcpy(buffer + sizeof(size_t), &j, sizeof(size_t));
    memcpy(buffer + 2 * sizeof(size_t), &s, sizeof(token_t));
    fwrite(buffer, 2 * sizeof(size_t) + sizeof(token_t), 1, stdout);
  }
};

struct WriteAscii {
  void operator()(size_t i, size_t j, token_t s) const {
    printf("%lu %lu %lu\n", i, j, (size_t) s);
  }
};


struct NoWrite {
  void operator()(size_t i, size_t j, token_t s) const {
    // No operation
  }
};

bool read_chunk(FILE* file, unsigned char* buffer, size_t chunk_size, size_t* bytes_read) {
    *bytes_read = fread(buffer, sizeof(unsigned char), chunk_size, file);
    
    bool is_not_done = (*bytes_read == chunk_size) && !feof(file);
    
    if (*bytes_read == chunk_size) {
      int next_char = fgetc(file);
      if (next_char != EOF) {
        ungetc(next_char, file);
        is_not_done = true;
      } else {
        is_not_done = false;
      }
    }
    
    return is_not_done;
}

template<typename PRINT>
int lexer_stream(PRINT print, bool timeit = false) {
  const unsigned int chunk_size = 64; // 100 * (1 << 20); // 100MiB
  
  unsigned char* h_string = (unsigned char*) malloc(chunk_size * sizeof(unsigned char));
  token_t* h_tokens = (token_t*) malloc(chunk_size * sizeof(token_t));
  size_t* h_starts = (size_t*) malloc(chunk_size * sizeof(size_t));
  size_t* h_ends = (size_t*) malloc(chunk_size * sizeof(size_t));
  assert(h_string != NULL);
  assert(h_tokens != NULL);
  assert(h_starts != NULL);
  assert(h_ends != NULL);

  unsigned char* d_string;
  token_t* d_tokens;
  size_t* d_starts;
  size_t* d_ends;
  gpuAssert(cudaMalloc((void**)&d_string, chunk_size * sizeof(unsigned char)));
  gpuAssert(cudaMalloc((void**)&d_tokens, chunk_size * sizeof(token_t)));
  gpuAssert(cudaMalloc((void**)&d_starts, chunk_size * sizeof(size_t)));
  gpuAssert(cudaMalloc((void**)&d_ends, chunk_size * sizeof(size_t)));

  const unsigned int BLOCK_SIZE = 32;
  assert(WARP <= BLOCK_SIZE);
  const unsigned int ITEMS_PER_THREAD = 1;
  LexerCtx ctx = LexerCtx<unsigned int, size_t>(chunk_size, BLOCK_SIZE, ITEMS_PER_THREAD);

  size_t new_size = 0;
  size_t final_size = 0;
  float time = 0;
  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);

  bool is_not_done = true;
  while (is_not_done) {
    size_t bytes;
    is_not_done = read_chunk(stdin, h_string, chunk_size, &bytes);
    final_size += bytes;

    gpuAssert(cudaMemcpy(d_string, h_string, bytes, cudaMemcpyHostToDevice));

    const unsigned int num_blocks = numBlocks(bytes, BLOCK_SIZE, ITEMS_PER_THREAD);
    cudaEventRecord(start, 0);
    lexer<unsigned int, size_t, BLOCK_SIZE, ITEMS_PER_THREAD><<<num_blocks, BLOCK_SIZE>>>(ctx, d_string, d_tokens, d_starts, d_ends, bytes, !is_not_done);
    gpuAssert(cudaDeviceSynchronize());
    cudaEventRecord(stop, 0);
    cudaEventSynchronize(stop);
    float temp = 0;
    cudaEventElapsedTime(&temp, start, stop);
    gpuAssert(cudaPeekAtLastError());
    time += temp;

    new_size = ctx.tokensSize();

    cudaMemcpy(h_tokens, d_tokens, new_size * sizeof(token_t), cudaMemcpyDeviceToHost);
    cudaMemcpy(h_starts, d_starts, new_size * sizeof(size_t), cudaMemcpyDeviceToHost);
    cudaMemcpy(h_ends, d_ends, new_size * sizeof(size_t), cudaMemcpyDeviceToHost);

    for (size_t i = 0; i < new_size; i++) {
      print(h_starts[i], h_ends[i], h_tokens[i]);
    }

    ctx.update();
  }

  if (timeit) {
    printf("Time: %.2fms\n", time);
  }

  fflush(stdout);

  int success = ctx.isAccept() ? 0 : -1;
  
  ctx.cleanUp();

  free(h_string);
  free(h_tokens);
  free(h_starts);
  cudaFree(d_string);
  cudaFree(d_tokens);
  cudaFree(d_starts);
  return success;
}

template<unsigned int BLOCK_SIZE, unsigned int ITEMS_PER_THREAD>
bool lexer_full(
  LexerCtx<unsigned int, size_t> ctx, 
  unsigned char* d_string,
  token_t* d_tokens,
  size_t* d_starts,
  size_t* d_ends,
  size_t chunk_size, 
  size_t size,
  size_t* new_size) {
  assert(chunk_size <= size);
  assert(size != 0);
  assert(d_string != NULL);
  assert(d_tokens == NULL);
  assert(d_starts == NULL);
  assert(d_ends == NULL);
  assert(WARP <= BLOCK_SIZE);
  assert(ITEMS_PER_THREAD > 1);

  cudaMalloc((void**)&d_tokens, chunk_size * sizeof(token_t));
  cudaMalloc((void**)&d_starts, chunk_size * sizeof(size_t));
  cudaMalloc((void**)&d_ends, chunk_size * sizeof(size_t));

  size_t prev_index = 0;
  size_t temp_new_size = 0;
  size_t alloc_size = chunk_size;
  float time = 0;
  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);

  for (size_t offset = 0; offset < size; offset+=chunk_size) {
    unsigned int bytes = min(chunk_size, size - offset);

    const unsigned int NUM_BLOCKS = numBlocks(bytes, BLOCK_SIZE, ITEMS_PER_THREAD);
    cudaEventRecord(start, 0);
    lexer<unsigned int, size_t, BLOCK_SIZE, ITEMS_PER_THREAD><<<NUM_BLOCKS, BLOCK_SIZE>>>(ctx, d_string, d_tokens, d_starts, d_ends, bytes, offset < size - chunk_size);
    gpuAssert(cudaDeviceSynchronize());
    cudaEventRecord(stop, 0);
    cudaEventSynchronize(stop);
    float temp = 0;
    cudaEventElapsedTime(&temp, start, stop);
    gpuAssert(cudaPeekAtLastError());
    time += temp;

    temp_new_size += ctx.tokensSize();

    if (alloc_size < temp_new_size + chunk_size) {
      while (alloc_size < temp_new_size + chunk_size) {
        alloc_size *= 2;
      }
      cudaMalloc((void**)&d_tokens, alloc_size * sizeof(token_t));
      cudaMalloc((void**)&d_starts, alloc_size * sizeof(size_t));
      cudaMalloc((void**)&d_ends, alloc_size * sizeof(size_t));
    }

    ctx.update();
  }

  *new_size = temp_new_size;
  
  cudaMalloc((void**)&d_tokens, temp_new_size * sizeof(token_t));
  cudaMalloc((void**)&d_starts, temp_new_size * sizeof(size_t));
  cudaMalloc((void**)&d_ends, temp_new_size * sizeof(size_t));

  return ctx.isAccept();
}
