// Calculate fixed overhead (independent of ITEMS_PER_THREAD)
template<typename I, typename state_t, uint32_t BLOCK_SIZE>
constexpr size_t calculate_lexer_fixed_overhead() {
  return sizeof(I) * BLOCK_SIZE +           // indices_aux
          sizeof(state_t) +                  // next_block_first_state
          sizeof(I) +                        // last_start
          sizeof(I) * WARP +                 // values
          sizeof(Status) * WARP +            // statuses
          sizeof(I);                         // shmem_prefix
}

// Calculate memory cost dependent on ITEMS_PER_THREAD (runtime parameter version)
template<typename I, typename state_t, uint32_t BLOCK_SIZE>
constexpr size_t calculate_lexer_variable_cost(uint32_t items_per_thread) {
  size_t states_bytes = sizeof(state_t) * items_per_thread * BLOCK_SIZE;
  size_t indices_bytes = sizeof(I) * items_per_thread * BLOCK_SIZE;
  size_t states_aux_bytes = sizeof(state_t) * BLOCK_SIZE;
  size_t buffer_bytes = (indices_bytes > states_aux_bytes) ? indices_bytes : states_aux_bytes;
  
  return states_bytes + buffer_bytes;
}

// Total shared memory usage
template<typename I, typename state_t, uint32_t BLOCK_SIZE, uint32_t ITEMS_PER_THREAD>
constexpr size_t calculate_lexer_shared_memory_usage() {
  return calculate_lexer_fixed_overhead<I, state_t, BLOCK_SIZE>() +
         calculate_lexer_variable_cost<I, state_t, BLOCK_SIZE>(ITEMS_PER_THREAD);
}

// Compile-time calculation of maximum ITEMS_PER_THREAD
template<typename I, typename state_t, uint32_t BLOCK_SIZE, uint32_t SHARED_MEMORY>
constexpr uint32_t calculate_lexer_max_items_per_thread() {
  constexpr size_t usable_shmem = static_cast<size_t>(SHARED_MEMORY * 0.9);
  constexpr size_t fixed_overhead = calculate_lexer_fixed_overhead<I, state_t, BLOCK_SIZE>();
  
  uint32_t max_items = 1;
  for (uint32_t items = 1; items <= 1024; items++) {
    size_t total = fixed_overhead + calculate_lexer_variable_cost<I, state_t, BLOCK_SIZE>(items);
    
    if (total <= usable_shmem) {
      max_items = items;
    } else {
      break;
    }
  }
  
  return max_items;
}

__device__ __host__ __forceinline__
state_t get_index(state_t state) {
  return (state & ENDO_MASK) >> ENDO_OFFSET;
}

__device__ __host__ __forceinline__
terminal_t get_terminal(state_t state) {
  return static_cast<terminal_t>((state & TERMINAL_MASK) >> TERMINAL_OFFSET);
}

__device__ __host__ __forceinline__
bool is_produce(state_t state) {
  return (state & PRODUCE_MASK) >> PRODUCE_OFFSET;
}

// CPU-only versions (if you still need them separately)
state_t get_index_cpu(state_t state) {
  return (state & ENDO_MASK) >> ENDO_OFFSET;
}

terminal_t get_terminal_cpu(state_t state) {
  return static_cast<terminal_t>((state & TERMINAL_MASK) >> TERMINAL_OFFSET);
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
  volatile uint32_t* d_dyn_block_index;
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
    cudaMemset((void*)d_dyn_block_index, 0, sizeof(uint32_t));
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

    gpuAssert(cudaMalloc((void**)&d_dyn_block_index, sizeof(uint32_t)));
    gpuAssert(cudaMalloc((void**)&d_new_size, sizeof(I)));
    gpuAssert(cudaMalloc((void**)&d_new_last_state, sizeof(state_t)));
    gpuAssert(cudaMalloc((void**)&d_old_last_state, sizeof(state_t)));
    gpuAssert(cudaMalloc((void**)&d_new_last_start, sizeof(J)));
    gpuAssert(cudaMalloc((void**)&d_old_last_start, sizeof(J)));

    cudaMemset((void*)d_dyn_block_index, 0, sizeof(uint32_t));
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
  uint32_t getDynamicIndex() const {
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

  I terminalsSize() const {
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
lexer(LexerCtx<I, J> ctx, uint8_t* d_string, terminal_t* d_terminals, J* d_starts, J* d_ends, const I size, const bool is_last_chunk) {
  constexpr size_t indices_bytes = ITEMS_PER_THREAD * BLOCK_SIZE * sizeof(I);
  constexpr size_t states_aux_bytes = BLOCK_SIZE * sizeof(state_t);
  constexpr size_t max_bytes = (indices_bytes > states_aux_bytes) ? indices_bytes : states_aux_bytes;
  volatile __shared__ state_t states[ITEMS_PER_THREAD * BLOCK_SIZE];
  volatile __shared__ I indices_aux[BLOCK_SIZE];
  volatile __shared__ uint8_t shared_buffer[max_bytes];
  volatile I* indices = (volatile I*) shared_buffer;
  volatile state_t* states_aux = (volatile state_t*) shared_buffer;
  __shared__ state_t next_block_first_state;
  const I REG_MEM = 1 + ITEMS_PER_THREAD / sizeof(uint64_t);
  uint64_t copy_reg[REG_MEM];
  uint8_t *chars_reg = (uint8_t*) copy_reg;
  uint32_t is_produce_state = 0;

  uint32_t dyn_index = ctx.getDynamicIndex();
  I glb_offs = dyn_index * BLOCK_SIZE * ITEMS_PER_THREAD;

  if (threadIdx.x == I()) {
    next_block_first_state = IDENTITY;
  }

#pragma unroll
  for (I i = 0; i < REG_MEM; i++) {
    I uint64_lid = i * blockDim.x + threadIdx.x;
    I lid = sizeof(uint64_t) * uint64_lid;
    I gid = glb_offs + lid;
    if (gid + sizeof(uint64_t) < size) {
      copy_reg[i] = *((uint64_t*) (gid + (uint8_t*) d_string));
    } else {
      for (I j = 0; j < sizeof(uint64_t); j++) {
        I loc_gid = gid + j;
        if (loc_gid < size) {
          chars_reg[sizeof(uint64_t) * i + j] = d_string[loc_gid];
        }
      }
    }
  }
    
#pragma unroll
  for (I i = 0; i < REG_MEM; i++) {
    I lid = i * blockDim.x + threadIdx.x;
    I _gid = glb_offs + sizeof(uint64_t) * lid;
    for (I j = 0; j < sizeof(uint64_t); j++) {
      I gid = _gid + j;
      I lid_off = sizeof(uint64_t) * lid + j;
      I reg_off = sizeof(uint64_t) * i + j;
      bool is_in_block = lid_off < ITEMS_PER_THREAD * BLOCK_SIZE; 
      if (gid < size && is_in_block) {
          if (gid == 0) {
            states[lid_off] = ctx(ctx.getLastState(), reinterpret_cast<state_t>(ctx.toState(chars_reg[reg_off])));
          } else {
            states[lid_off] = ctx.toState(chars_reg[reg_off]);
          }
      } else if (is_in_block) {
          states[lid_off] = IDENTITY;
      } else if (lid_off == ITEMS_PER_THREAD * BLOCK_SIZE && !is_last_chunk) {
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
#ifdef IGNORE_TERMINAL
      bool is_not_ignore = get_terminal(state) != IGNORE_TERMINAL;
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
        is_next_produce &= is_not_ignore;
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
      d_terminals[offset] = get_terminal(states[lid]);
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
  void operator()(size_t i, size_t j, terminal_t s) const {
    uint8_t* buffer[2 * sizeof(size_t) + sizeof(terminal_t)];
    memcpy(buffer, &i, sizeof(size_t));
    memcpy(buffer + sizeof(size_t), &j, sizeof(size_t));
    memcpy(buffer + 2 * sizeof(size_t), &s, sizeof(terminal_t));
    fwrite(buffer, 2 * sizeof(size_t) + sizeof(terminal_t), 1, stdout);
  }
};

struct WriteAscii {
  void operator()(size_t i, size_t j, terminal_t s) const {
    printf("%lu %lu %lu\n", i, j, (size_t) s);
  }
};


struct NoWrite {
  void operator()(size_t i, size_t j, terminal_t s) const {
    // No operation
  }
};

bool read_chunk(FILE* file, uint8_t* buffer, size_t chunk_size, size_t* bytes_read) {
    *bytes_read = fread(buffer, sizeof(uint8_t), chunk_size, file);
    
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

template<typename PRINT, uint32_t CHUNK_SIZE, uint32_t BLOCK_SIZE, uint32_t ITEMS_PER_THREAD>
int lexer_stream(PRINT print, bool timeit = false) {
  
  uint8_t* h_string = (uint8_t*) malloc(CHUNK_SIZE * sizeof(uint8_t));
  terminal_t* h_terminals = (terminal_t*) malloc(CHUNK_SIZE * sizeof(terminal_t));
  size_t* h_starts = (size_t*) malloc(CHUNK_SIZE * sizeof(size_t));
  size_t* h_ends = (size_t*) malloc(CHUNK_SIZE * sizeof(size_t));
  assert(h_string != NULL);
  assert(h_terminals != NULL);
  assert(h_starts != NULL);
  assert(h_ends != NULL);

  constexpr size_t required_shmem = 
      calculate_lexer_shared_memory_usage<uint32_t, state_t, BLOCK_SIZE, ITEMS_PER_THREAD>();
  
  int device;
  cudaGetDevice(&device);
  cudaDeviceProp prop;
  cudaGetDeviceProperties(&prop, device);
  
  assert(required_shmem <= prop.sharedMemPerBlock && 
          "Kernel requires more shared memory than available on device!");

  uint8_t* d_string;
  terminal_t* d_terminals;
  size_t* d_starts;
  size_t* d_ends;
  gpuAssert(cudaMalloc((void**)&d_string, CHUNK_SIZE * sizeof(uint8_t)));
  gpuAssert(cudaMalloc((void**)&d_terminals, CHUNK_SIZE * sizeof(terminal_t)));
  gpuAssert(cudaMalloc((void**)&d_starts, CHUNK_SIZE * sizeof(size_t)));
  gpuAssert(cudaMalloc((void**)&d_ends, CHUNK_SIZE * sizeof(size_t)));

  assert(WARP <= BLOCK_SIZE);
  LexerCtx ctx = LexerCtx<uint32_t, size_t>(CHUNK_SIZE, BLOCK_SIZE, ITEMS_PER_THREAD);

  size_t new_size = 0;
  size_t final_size = 0;
  float time = 0;
  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);

  bool is_not_done = true;
  while (is_not_done) {
    size_t bytes;
    is_not_done = read_chunk(stdin, h_string, CHUNK_SIZE, &bytes);
    final_size += bytes;

    gpuAssert(cudaMemcpy(d_string, h_string, bytes, cudaMemcpyHostToDevice));

    const uint32_t num_blocks = numBlocks(bytes, BLOCK_SIZE, ITEMS_PER_THREAD);
    cudaEventRecord(start, 0);
    lexer<uint32_t, size_t, BLOCK_SIZE, ITEMS_PER_THREAD><<<num_blocks, BLOCK_SIZE>>>(ctx, d_string, d_terminals, d_starts, d_ends, bytes, !is_not_done);
    gpuAssert(cudaDeviceSynchronize());
    cudaEventRecord(stop, 0);
    cudaEventSynchronize(stop);
    float temp = 0;
    cudaEventElapsedTime(&temp, start, stop);
    gpuAssert(cudaPeekAtLastError());
    time += temp;

    new_size = ctx.terminalsSize();

    cudaMemcpy(h_terminals, d_terminals, new_size * sizeof(terminal_t), cudaMemcpyDeviceToHost);
    cudaMemcpy(h_starts, d_starts, new_size * sizeof(size_t), cudaMemcpyDeviceToHost);
    cudaMemcpy(h_ends, d_ends, new_size * sizeof(size_t), cudaMemcpyDeviceToHost);

    for (size_t i = 0; i < new_size; i++) {
      print(h_starts[i], h_ends[i], h_terminals[i]);
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
  free(h_terminals);
  free(h_starts);
  cudaFree(d_string);
  cudaFree(d_terminals);
  cudaFree(d_starts);
  return success;
}

template<uint32_t CHUNK_SIZE, uint32_t BLOCK_SIZE, uint32_t ITEMS_PER_THREAD>
bool lexer_full(
  LexerCtx<uint32_t, size_t> ctx, 
  uint8_t* d_string,
  terminal_t* d_terminals,
  size_t* d_starts,
  size_t* d_ends,
  size_t size,
  size_t* new_size) {
  assert(size != 0);
  assert(d_string != NULL);
  assert(d_terminals == NULL);
  assert(d_starts == NULL);
  assert(d_ends == NULL);
  assert(WARP <= BLOCK_SIZE);
  assert(ITEMS_PER_THREAD > 1);

  constexpr size_t required_shmem = 
      calculate_lexer_shared_memory_usage<uint32_t, state_t, BLOCK_SIZE, ITEMS_PER_THREAD>();
  
  int device;
  cudaGetDevice(&device);
  cudaDeviceProp prop;
  cudaGetDeviceProperties(&prop, device);
  
  assert(required_shmem <= prop.sharedMemPerBlock && 
          "Kernel requires more shared memory than available on device!");

  cudaMalloc((void**)&d_terminals, CHUNK_SIZE * sizeof(terminal_t));
  cudaMalloc((void**)&d_starts, CHUNK_SIZE * sizeof(size_t));
  cudaMalloc((void**)&d_ends, CHUNK_SIZE * sizeof(size_t));

  size_t prev_index = 0;
  size_t temp_new_size = 0;
  size_t alloc_size = CHUNK_SIZE;
  float time = 0;
  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);

  for (size_t offset = 0; offset < size; offset+=CHUNK_SIZE) {
    uint32_t bytes = min((size_t) CHUNK_SIZE, size - offset);

    const uint32_t NUM_BLOCKS = numBlocks(bytes, BLOCK_SIZE, ITEMS_PER_THREAD);
    cudaEventRecord(start, 0);
    lexer<uint32_t, size_t, BLOCK_SIZE, ITEMS_PER_THREAD><<<NUM_BLOCKS, BLOCK_SIZE>>>(ctx, d_string, d_terminals, d_starts, d_ends, bytes, offset < size - CHUNK_SIZE);
    gpuAssert(cudaDeviceSynchronize());
    cudaEventRecord(stop, 0);
    cudaEventSynchronize(stop);
    float temp = 0;
    cudaEventElapsedTime(&temp, start, stop);
    gpuAssert(cudaPeekAtLastError());
    time += temp;

    temp_new_size += ctx.terminalsSize();

    if (alloc_size < temp_new_size + CHUNK_SIZE) {
      while (alloc_size < temp_new_size + CHUNK_SIZE) {
        alloc_size *= 2;
      }
      cudaMalloc((void**)&d_terminals, alloc_size * sizeof(terminal_t));
      cudaMalloc((void**)&d_starts, alloc_size * sizeof(size_t));
      cudaMalloc((void**)&d_ends, alloc_size * sizeof(size_t));
    }

    ctx.update();
  }

  *new_size = temp_new_size;
  
  cudaMalloc((void**)&d_terminals, temp_new_size * sizeof(terminal_t));
  cudaMalloc((void**)&d_starts, temp_new_size * sizeof(size_t));
  cudaMalloc((void**)&d_ends, temp_new_size * sizeof(size_t));

  return ctx.isAccept();
}
