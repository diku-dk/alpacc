#include <cuda/atomic>
#include <cub/cub.cuh>

template<typename T, typename I, typename OP>
__device__ inline T
scanWarp(volatile T* shmem,
         OP op,
         const uint8_t lane) {
  uint8_t h;

#pragma unroll
  for (uint8_t d = 0; d < LG_WARP; d++) {
    if ((h = 1 << d) <= lane)
      shmem[threadIdx.x] = op(shmem[threadIdx.x - h], shmem[threadIdx.x]);
    __syncwarp();
  }

  return shmem[threadIdx.x];
}

template<typename T, typename I, typename OP>
__device__ inline void
scanBlock(volatile T* shmem,
          OP op) {
  const uint8_t lane = threadIdx.x & (WARP - 1);
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

__device__ inline uint32_t dynamicIndex(volatile uint32_t* dyn_idx_ptr) {
  volatile __shared__ uint32_t dyn_idx;

  if (threadIdx.x == 0)
    dyn_idx = atomicAdd(const_cast<uint32_t*>(dyn_idx_ptr), 1);

  __syncthreads();
  return dyn_idx;
}

enum Status: uint8_t {
  Invalid = 0,
  Aggregate = 1,
  Prefix = 2,
};

// Alias for device-scope atomic status (acquire/release semantics).
using AtomicStatus = cuda::atomic<Status, cuda::thread_scope_device>;

template<typename I, typename T>
struct States {
  volatile T*   aggregates = nullptr;
  volatile T*   prefixes   = nullptr;
  AtomicStatus* statuses   = nullptr;
  I num_blocks = 0;

  States(I num_blocks) : num_blocks(num_blocks) {
    cudaMalloc((void**)&aggregates, num_blocks * sizeof(T));
    cudaMalloc((void**)&prefixes, num_blocks * sizeof(T));
    cudaMalloc((void**)&statuses, num_blocks * sizeof(AtomicStatus));
    cudaMemset((void*) statuses, Invalid, num_blocks * sizeof(AtomicStatus));
  }

  States() {
  }

  void reset() {
    if (statuses) cudaMemset((void*) statuses, Invalid, num_blocks * sizeof(AtomicStatus));
  }

  void cleanUp() {
    if (aggregates) cudaFree((void*) aggregates);
    if (prefixes) cudaFree((void*) prefixes);
    if (statuses) cudaFree((void*) statuses);
    aggregates = nullptr; prefixes = nullptr; statuses = nullptr;
  }
};

// Structure-of-arrays inter-block state buffer for a component-wise scan on
// two independent monoids (TA, OPA) and (TB, OPB).  The two components are
// always published atomically together, so the block-status array is shared
// between them — one atomic release/acquire round per block covers both.
// This avoids the packed-u64 layout that forced both components to share the
// same width (and dragged the wider width's traffic through the reduction
// even when one component would fit in a narrower type).
template<typename I, typename TA, typename TB>
struct PairStates {
  volatile TA*  a_aggregates = nullptr;
  volatile TA*  a_prefixes   = nullptr;
  volatile TB*  b_aggregates = nullptr;
  volatile TB*  b_prefixes   = nullptr;
  AtomicStatus* statuses     = nullptr;
  I num_blocks = 0;

  PairStates(I num_blocks) : num_blocks(num_blocks) {
    cudaMalloc((void**)&a_aggregates, num_blocks * sizeof(TA));
    cudaMalloc((void**)&a_prefixes,   num_blocks * sizeof(TA));
    cudaMalloc((void**)&b_aggregates, num_blocks * sizeof(TB));
    cudaMalloc((void**)&b_prefixes,   num_blocks * sizeof(TB));
    cudaMalloc((void**)&statuses,     num_blocks * sizeof(AtomicStatus));
    cudaMemset((void*) statuses, Invalid, num_blocks * sizeof(AtomicStatus));
  }

  PairStates() {
  }

  void reset() {
    if (statuses) cudaMemset((void*) statuses, Invalid, num_blocks * sizeof(AtomicStatus));
  }

  void cleanUp() {
    if (a_aggregates) cudaFree((void*) a_aggregates);
    if (a_prefixes)   cudaFree((void*) a_prefixes);
    if (b_aggregates) cudaFree((void*) b_aggregates);
    if (b_prefixes)   cudaFree((void*) b_prefixes);
    if (statuses)     cudaFree((void*) statuses);
    a_aggregates = nullptr; a_prefixes = nullptr;
    b_aggregates = nullptr; b_prefixes = nullptr;
    statuses = nullptr;
  }
};

__device__ inline Status
combine(Status a, Status b) {
  if (a == Invalid || b == Invalid)
    return Invalid;
  if (b == Aggregate)
    return a;
  return b;
}

template<typename T, typename I, typename OP>
__device__ inline void
scanWarp(volatile T* values,
         volatile Status* statuses,
         OP op,
         const uint8_t lane) {
  uint8_t h;
  const I tid = threadIdx.x;

#pragma unroll
  for (uint8_t d = 0; d < LG_WARP; d++) {
    if ((h = 1 << d) <= lane) {
      bool is_not_aggregate = statuses[tid] != Aggregate;
      values[tid] = is_not_aggregate ? values[tid] : op(values[tid - h], values[tid]);
      statuses[tid] = combine(statuses[tid - h], statuses[tid]);
    }
    __syncwarp();
  }
}

// __nanosleep is available on sm_70+ (Volta, Turing, Ampere, …).
// For older arches it compiles away to nothing.
#if __CUDA_ARCH__ >= 700
#define LOOKBACK_INITIAL_DELAY() __nanosleep(450u)
#define LOOKBACK_SPIN_DELAY()    __nanosleep(350u)
#else
#define LOOKBACK_INITIAL_DELAY() __threadfence_block()
#define LOOKBACK_SPIN_DELAY()    __threadfence_block()
#endif

// Computes this block's exclusive prefix via decoupled lookback, given the
// block-wide aggregate (valid in all threads). Returns the prefix to all
// threads.
template<typename T, typename I, typename OP>
__device__ inline T
lookbackPrefix(States<I, T> states,
               OP op,
               T ne,
               uint32_t dyn_idx,
               T aggregate) {
  volatile __shared__ T values[WARP];
  volatile __shared__ Status statuses[WARP];
  volatile __shared__ T shmem_prefix;
  const uint8_t lane = threadIdx.x & (WARP - 1);
  const bool is_first = threadIdx.x == 0;

  if (is_first) {
    // relaxed store is fine: the __syncthreads() below acts as a barrier
    // ensuring all threads in this block see Invalid before we proceed.
    states.statuses[dyn_idx].store(Invalid, cuda::memory_order_relaxed);
  }
  __syncthreads();

  if (is_first) {
    states.aggregates[dyn_idx] = aggregate;
  }

  if (dyn_idx == 0 && is_first) {
    states.prefixes[dyn_idx] = aggregate;
  }
  // Release store: memory_order_release makes aggregate/prefix writes visible
  // to all threads that subsequently load this status with memory_order_acquire.
  if (dyn_idx == 0 && is_first) {
    states.statuses[dyn_idx].store(Prefix, cuda::memory_order_release);
  } else if (is_first) {
    states.statuses[dyn_idx].store(Aggregate, cuda::memory_order_release);
  }

  T prefix = ne;
  if (threadIdx.x < WARP && dyn_idx != 0) {
    I lookback_idx = threadIdx.x + dyn_idx;
    I lookback_warp = WARP;
    Status status = Aggregate;
    // Initial delay: give predecessor blocks time to write their aggregate
    // before we poll, matching CUB's L2WriteLatency delay heuristic.
    LOOKBACK_INITIAL_DELAY();
    do {
      if (lookback_warp <= lookback_idx) {
        I idx = lookback_idx - lookback_warp;
        // Acquire load: pairs with the writer's memory_order_release store,
        // ensuring aggregate/prefix writes are visible before we read them.
        Status s = states.statuses[idx].load(cuda::memory_order_acquire);
        statuses[threadIdx.x] = s;
        values[threadIdx.x] = s == Prefix ? states.prefixes[idx] : states.aggregates[idx];
      } else {
        statuses[threadIdx.x] = Aggregate;
        values[threadIdx.x] = ne;
      }

      scanWarp<T, I, OP>(values, statuses, op, lane);

      T result = values[WARP - 1];
      status = statuses[WARP - 1];

      if (status == Invalid) {
        // Any lane saw Invalid — spin-delay before re-polling to avoid
        // hammering the L2 and monopolising the memory bus.
        LOOKBACK_SPIN_DELAY();
        continue;
      }

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
    states.statuses[dyn_idx].store(Prefix, cuda::memory_order_release);
  }

  return shmem_prefix;
}

// Component-wise warp scan on two independent monoids sharing a single
// status array.  Both components take the same "skip when this lane has a
// resolved Prefix" rule — hence one status stream governs both.
template<typename TA, typename TB, typename I, typename OPA, typename OPB>
__device__ inline void
scanWarpPair(volatile TA* a_values,
             volatile TB* b_values,
             volatile Status* statuses,
             OPA op_a,
             OPB op_b,
             const uint8_t lane) {
  uint8_t h;
  const I tid = threadIdx.x;

#pragma unroll
  for (uint8_t d = 0; d < LG_WARP; d++) {
    if ((h = 1 << d) <= lane) {
      bool is_not_aggregate = statuses[tid] != Aggregate;
      a_values[tid] = is_not_aggregate ? a_values[tid] : op_a(a_values[tid - h], a_values[tid]);
      b_values[tid] = is_not_aggregate ? b_values[tid] : op_b(b_values[tid - h], b_values[tid]);
      statuses[tid] = combine(statuses[tid - h], statuses[tid]);
    }
    __syncwarp();
  }
}

// Decoupled-lookback inter-block prefix for a two-component (max/add-style)
// scan with SoA state buffers.  Same handshake protocol as lookbackPrefix
// — one status atomic per block, warp-level scan across the predecessor
// tiles — but the two component aggregates live in separate arrays inside
// PairStates, so neither is forced to widen to the other's type.
//
// Returns the exclusive block prefix for both components via the
// (prefix_a, prefix_b) reference parameters (valid in all threads).
template<typename TA, typename TB, typename I, typename OPA, typename OPB>
__device__ inline void
lookbackPrefixPair(PairStates<I, TA, TB> states,
                   OPA op_a,
                   OPB op_b,
                   TA ne_a,
                   TB ne_b,
                   uint32_t dyn_idx,
                   TA aggregate_a,
                   TB aggregate_b,
                   TA& out_prefix_a,
                   TB& out_prefix_b) {
  volatile __shared__ TA a_values[WARP];
  volatile __shared__ TB b_values[WARP];
  volatile __shared__ Status statuses[WARP];
  volatile __shared__ TA shmem_prefix_a;
  volatile __shared__ TB shmem_prefix_b;
  const uint8_t lane = threadIdx.x & (WARP - 1);
  const bool is_first = threadIdx.x == 0;

  if (is_first) {
    states.statuses[dyn_idx].store(Invalid, cuda::memory_order_relaxed);
  }
  __syncthreads();

  if (is_first) {
    states.a_aggregates[dyn_idx] = aggregate_a;
    states.b_aggregates[dyn_idx] = aggregate_b;
  }

  if (dyn_idx == 0 && is_first) {
    states.a_prefixes[dyn_idx] = aggregate_a;
    states.b_prefixes[dyn_idx] = aggregate_b;
  }
  if (dyn_idx == 0 && is_first) {
    states.statuses[dyn_idx].store(Prefix, cuda::memory_order_release);
  } else if (is_first) {
    states.statuses[dyn_idx].store(Aggregate, cuda::memory_order_release);
  }

  TA prefix_a = ne_a;
  TB prefix_b = ne_b;
  if (threadIdx.x < WARP && dyn_idx != 0) {
    I lookback_idx = threadIdx.x + dyn_idx;
    I lookback_warp = WARP;
    Status status = Aggregate;
    LOOKBACK_INITIAL_DELAY();
    do {
      if (lookback_warp <= lookback_idx) {
        I idx = lookback_idx - lookback_warp;
        Status s = states.statuses[idx].load(cuda::memory_order_acquire);
        statuses[threadIdx.x] = s;
        a_values[threadIdx.x] = s == Prefix ? states.a_prefixes[idx] : states.a_aggregates[idx];
        b_values[threadIdx.x] = s == Prefix ? states.b_prefixes[idx] : states.b_aggregates[idx];
      } else {
        statuses[threadIdx.x] = Aggregate;
        a_values[threadIdx.x] = ne_a;
        b_values[threadIdx.x] = ne_b;
      }

      scanWarpPair<TA, TB, I, OPA, OPB>(a_values, b_values, statuses, op_a, op_b, lane);

      TA result_a = a_values[WARP - 1];
      TB result_b = b_values[WARP - 1];
      status = statuses[WARP - 1];

      if (status == Invalid) {
        LOOKBACK_SPIN_DELAY();
        continue;
      }

      if (is_first) {
        prefix_a = op_a(result_a, prefix_a);
        prefix_b = op_b(result_b, prefix_b);
      }

      lookback_warp += WARP;
    } while (status != Prefix);
  }

  if (is_first) {
    shmem_prefix_a = prefix_a;
    shmem_prefix_b = prefix_b;
  }

  __syncthreads();

  if (is_first) {
    states.a_prefixes[dyn_idx] = op_a(prefix_a, aggregate_a);
    states.b_prefixes[dyn_idx] = op_b(prefix_b, aggregate_b);
    states.statuses[dyn_idx].store(Prefix, cuda::memory_order_release);
  }

  out_prefix_a = shmem_prefix_a;
  out_prefix_b = shmem_prefix_b;
}

// Single-pass device scan over one block's tile: cub::BlockScan (register
// scan, warp shuffles) for the block-level phase + decoupled lookback for
// the inter-block prefix. The tile lives in shared memory in blocked layout
// (thread t owns [t*ITEMS_PER_THREAD, (t+1)*ITEMS_PER_THREAD)); the caller
// must __syncthreads() between writing the tile and calling this.
template<typename T, typename I, typename OP, I ITEMS_PER_THREAD, I BLOCK_SIZE>
__device__ inline T
scan(volatile T* shmem,
     States<I, T> states,
     OP op,
     T ne,
     uint32_t dyn_idx,
     bool write_back = true) {
  using BlockScanT = cub::BlockScan<T, BLOCK_SIZE>;
  __shared__ typename BlockScanT::TempStorage temp_storage;

  T items[ITEMS_PER_THREAD];
  const I offset = threadIdx.x * ITEMS_PER_THREAD;
#pragma unroll
  for (I i = 0; i < ITEMS_PER_THREAD; i++) {
    items[i] = shmem[offset + i];
  }

  T aggregate;
  BlockScanT(temp_storage).InclusiveScan(items, items, op, aggregate);

  const T prefix = lookbackPrefix<T, I, OP>(states, op, ne, dyn_idx, aggregate);

#pragma unroll
  for (I i = 0; i < ITEMS_PER_THREAD; i++) {
    shmem[offset + i] = write_back ? op(prefix, items[i]) : items[i];
  }
  __syncthreads();

  return prefix;
}

// Block-local half of scanReg: cub::BlockScan::InclusiveScan on the tile
// held in blocked-layout registers.  Writes the inclusive scan back into
// items and returns the block-wide aggregate to all threads.  Does NOT
// apply an inter-block prefix — callers that need the exclusive block
// prefix from decoupled lookback call lookbackPrefix separately.
//
// Splitting the scan into block-local + lookback lets callers with two
// independent monoids run the block-local phase separately (each with its
// own scalar operator, shorter critical path per combine) and then perform
// a single fused decoupled-lookback handshake over both, avoiding a second
// inter-block round.  Both phases share TempStorage via the caller's
// declaration; passing it in also lets the caller reuse one shared
// allocation across several scans in the same kernel.
template<typename T, typename I, typename OP, I ITEMS_PER_THREAD, I BLOCK_SIZE>
__device__ inline T
scanRegLocal(T (&items)[ITEMS_PER_THREAD],
             typename cub::BlockScan<T, BLOCK_SIZE>::TempStorage& temp_storage,
             OP op) {
  T aggregate;
  cub::BlockScan<T, BLOCK_SIZE>(temp_storage).InclusiveScan(items, items, op, aggregate);
  return aggregate;
}

// Register-tile variant of scan: the caller supplies the tile in blocked
// layout registers (thread t owns items [t*ITEMS_PER_THREAD, (t+1)*IPT)).
// Items are replaced by their block-local inclusive scan (no inter-block
// prefix applied); the returned value is the exclusive block prefix from
// decoupled lookback, valid in all threads.
template<typename T, typename I, typename OP, I ITEMS_PER_THREAD, I BLOCK_SIZE>
__device__ inline T
scanReg(T (&items)[ITEMS_PER_THREAD],
        States<I, T> states,
        OP op,
        T ne,
        uint32_t dyn_idx) {
  using BlockScanT = cub::BlockScan<T, BLOCK_SIZE>;
  __shared__ typename BlockScanT::TempStorage temp_storage;

  T aggregate = scanRegLocal<T, I, OP, ITEMS_PER_THREAD, BLOCK_SIZE>(items, temp_storage, op);
  return lookbackPrefix<T, I, OP>(states, op, ne, dyn_idx, aggregate);
}

template<typename I>
struct Add {
  __device__ __forceinline__ I operator()(I a, I b) const {
    return a + b;
  }
};

template<typename I>
struct Max {
  __device__ __forceinline__ I operator()(I a, I b) const {
    return a > b ? a : b;
  }
};

