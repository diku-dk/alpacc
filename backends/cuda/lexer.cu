// ---------------------------------------------------------------------------
// Large-tile single-pass lexer.
//
// A block's tile is BLOCK_SIZE * ITEMS_PER_THREAD input bytes (24 KB at the
// default 256 x 96), kept in shared memory; thread t owns the
// ITEMS_PER_THREAD contiguous bytes [t * IPT, (t + 1) * IPT) (its chunk).
// Per tile:
//   A. each thread reduces its chunk to one state (a chain of byte steps from
//      IDENTITY); a block exclusive scan of these aggregates with a decoupled
//      look-back over the preceding tiles gives each thread its incoming
//      state;
//   B. each thread rescans its chunk from the incoming state, writes every
//      byte's terminal in place over the input, and gathers per-byte bits in
//      three-word registers: token ends (the state after the byte produces)
//      and kept tokens (terminal != IGNORE_TOKEN); a second block scan with
//      look-back over (max token start, kept-token count) gives each thread
//      its first output slot and the start of its first token;
//   C. warp-cooperative emission: each warp writes 32 consecutive output
//      slots per step (coalesced).  Lane r finds the lane owning slot r by a
//      binary search over the lanes' inclusive counts and the byte by a
//      k-th-set-bit select in that lane's mask; the start is the owner's
//      previous token end in its mask, else its incoming token start.
// One byte step is two lookups: the byte's class (bytes with the same
// to_state endofunction, a 256-byte table in shared memory) and the step
// table step[s * num_classes + class] = compose(s, to_state[byte]), derived
// in the LexerCtx constructor from h_compose and h_to_state.  The step table
// is NUM_STATES x classes entries (JSON: 823 x 23, 37 KB) instead of the
// NUM_STATES^2 compose table (1.35 MB), so it stays L1-resident.  The scans
// and look-backs use the compose table (a few lookups per thread).
// Each chunk's 16-byte vectors are swizzled in shared memory (vector k of
// thread t at slot k ^ ((t >> 2) & 1)), which removes the 2-way bank
// conflicts of the stride-96 vector accesses.
// The look-backs use static tile indices (blockIdx.x) and one-word tile
// descriptors (status and value together) published with relaxed stores.
// ---------------------------------------------------------------------------

#include <cstdio>
#include <cstdlib>
#include <vector>

// Default tile shape: 256 threads x 96 bytes (tuned on the A100).
// ITEMS_PER_THREAD must be 32, 64 or 96: a multiple of 32 (swizzled chunk
// offsets keep bit 4 free) and at most 96 (three-word bit masks).
constexpr uint32_t LEXER_BLOCK_SIZE = 256;
constexpr uint32_t LEXER_CHUNK      = 96;

#if __CUDA_ARCH__ >= 800
#define ALPACC_LEXER_BOUNDS(BS) __launch_bounds__(BS, 1536 / (BS))   // 1536 threads/SM
#else
#define ALPACC_LEXER_BOUNDS(BS) __launch_bounds__(BS)
#endif

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

// compose(a, b): apply a first, then b.
struct LexerCompose {
  const state_t* table;
  __device__ __forceinline__ state_t operator()(const state_t& a, const state_t& b) const {
    return __ldg(&table[(uint32_t)get_index(a) * NUM_STATES + get_index(b)]);
  }
};

// Token starts and output slots (pass B): per thread the start of the token
// after its last token end, and its number of kept tokens.  Starts are
// encoded as position + 1 relative to the chunk, 0 = no token end seen yet
// (the token started in an earlier chunk: LexerCtx::getLastStart()).
struct MaxAdd {
  uint32_t max;
  uint32_t cnt;
};
struct MaxAddOp {
  __device__ __forceinline__ MaxAdd operator()(const MaxAdd& a, const MaxAdd& b) const {
    return MaxAdd{a.max > b.max ? a.max : b.max, a.cnt + b.cnt};
  }
};

// ---------------------------------------------------------------------------
// Decoupled look-back with one-word tile descriptors: status and value are
// packed into one TxnWord, so publishing and reading a tile are each a single
// transaction and need no fences.
// ---------------------------------------------------------------------------

// Padding entries before the descriptors: the first window of tile t reads
// tiles t - 1 ... t - WARP, as low as -WARP.
const uint32_t LEXER_TILE_PADDING = WARP;

enum LexerTileStatus : uint32_t {
  LEXER_TILE_OOB       = 0,  // padding (before the first tile)
  LEXER_TILE_INVALID   = 1,  // not yet published
  LEXER_TILE_PARTIAL   = 2,  // tile aggregate published
  LEXER_TILE_INCLUSIVE = 3,  // inclusive prefix published
};

template<typename T>
struct TxnWordTraits;

template<> struct TxnWordTraits<uint8_t> {
  using TxnWord    = uint16_t;
  using StatusWord = uint8_t;
  __device__ __forceinline__ static TxnWord pack(StatusWord status, uint8_t value) {
    return uint16_t((uint32_t(value) << 8) | uint32_t(status));
  }
  __device__ __forceinline__ static StatusWord unpack_status(TxnWord w) { return StatusWord(w & 0xffu); }
  __device__ __forceinline__ static uint8_t unpack_value(TxnWord w) { return uint8_t(w >> 8); }
};
template<> struct TxnWordTraits<uint16_t> {
  using TxnWord    = uint32_t;
  using StatusWord = uint16_t;
  __device__ __forceinline__ static TxnWord pack(StatusWord status, uint16_t value) {
    return (uint32_t(value) << 16) | uint32_t(status);
  }
  __device__ __forceinline__ static StatusWord unpack_status(TxnWord w) { return StatusWord(w & 0xffffu); }
  __device__ __forceinline__ static uint16_t unpack_value(TxnWord w) { return uint16_t(w >> 16); }
};
template<> struct TxnWordTraits<uint32_t> {
  using TxnWord    = unsigned long long;
  using StatusWord = uint32_t;
  __device__ __forceinline__ static TxnWord pack(StatusWord status, uint32_t value) {
    return (TxnWord(value) << 32) | TxnWord(status);
  }
  __device__ __forceinline__ static StatusWord unpack_status(TxnWord w) { return StatusWord(w & 0xffffffffull); }
  __device__ __forceinline__ static uint32_t unpack_value(TxnWord w) { return uint32_t(w >> 32); }
};
// MaxAdd: status in bits 1-0, max in bits 32-2, cnt in bits 63-33 (inputs
// and token counts < 2^31, checked in the LexerCtx constructor).
template<> struct TxnWordTraits<MaxAdd> {
  using TxnWord    = unsigned long long;
  using StatusWord = uint32_t;
  __device__ __forceinline__ static TxnWord pack(StatusWord status, MaxAdd v) {
    return TxnWord(status) | (TxnWord(v.max) << 2) | (TxnWord(v.cnt) << 33);
  }
  __device__ __forceinline__ static StatusWord unpack_status(TxnWord w) { return StatusWord(w & 3ull); }
  __device__ __forceinline__ static MaxAdd unpack_value(TxnWord w) {
    return MaxAdd{uint32_t((w >> 2) & 0x7fffffffull), uint32_t(w >> 33)};
  }
};

// Relaxed GPU-scope stores: status and value share one word, so a reader
// needs no ordering beyond that word itself.
#if __CUDA_ARCH__ >= 700
__device__ __forceinline__ void lexer_store_relaxed(uint16_t* ptr, uint16_t val) {
  asm volatile("st.relaxed.gpu.u16 [%0], %1;" :: "l"(ptr), "h"(val) : "memory");
}
__device__ __forceinline__ void lexer_store_relaxed(uint32_t* ptr, uint32_t val) {
  asm volatile("st.relaxed.gpu.u32 [%0], %1;" :: "l"(ptr), "r"(val) : "memory");
}
__device__ __forceinline__ void lexer_store_relaxed(unsigned long long* ptr, unsigned long long val) {
  asm volatile("st.relaxed.gpu.u64 [%0], %1;" :: "l"(ptr), "l"(val) : "memory");
}
#define LEXER_SLEEP(ns) __nanosleep(ns)
#else
template<typename W>
__device__ __forceinline__ void lexer_store_relaxed(W* ptr, W val) {
  __threadfence();
  *const_cast<volatile W*>(ptr) = val;
}
#define LEXER_SLEEP(ns) __threadfence_block()
#endif

// Per-tile descriptors, (num_tiles + LEXER_TILE_PADDING) TxnWords.
template<typename T>
struct ScanTileState {
  using StatusWord = typename TxnWordTraits<T>::StatusWord;
  using TxnWord    = typename TxnWordTraits<T>::TxnWord;

  TxnWord* d_tile_descriptors;

  __host__ static size_t AllocationSize(uint32_t num_tiles) {
    return (num_tiles + LEXER_TILE_PADDING) * sizeof(TxnWord);
  }

  // One thread per descriptor.
  __device__ void InitializeStatus(uint32_t num_tiles) {
    uint32_t idx = blockIdx.x * blockDim.x + threadIdx.x;
    if (idx < num_tiles)
      d_tile_descriptors[LEXER_TILE_PADDING + idx] =
          TxnWordTraits<T>::pack(StatusWord(LEXER_TILE_INVALID), T());
    if (blockIdx.x == 0 && threadIdx.x < LEXER_TILE_PADDING)
      d_tile_descriptors[threadIdx.x] = TxnWordTraits<T>::pack(StatusWord(LEXER_TILE_OOB), T());
  }

  __device__ __forceinline__ void SetPartial(int tile_idx, T value) {
    lexer_store_relaxed(d_tile_descriptors + LEXER_TILE_PADDING + tile_idx,
                        TxnWordTraits<T>::pack(StatusWord(LEXER_TILE_PARTIAL), value));
  }
  __device__ __forceinline__ void SetInclusive(int tile_idx, T value) {
    lexer_store_relaxed(d_tile_descriptors + LEXER_TILE_PADDING + tile_idx,
                        TxnWordTraits<T>::pack(StatusWord(LEXER_TILE_INCLUSIVE), value));
  }

  // Spins (warp-wide) until the tile is published.
  __device__ __forceinline__ void WaitForValid(int tile_idx, StatusWord& status, T& value,
                                               uint32_t initial_delay_ns) {
    if (initial_delay_ns > 0)
      LEXER_SLEEP(initial_delay_ns);
    const volatile TxnWord* p = d_tile_descriptors + LEXER_TILE_PADDING + tile_idx;
    TxnWord word = *p;
    while (__any_sync(0xffffffff,
           TxnWordTraits<T>::unpack_status(word) == StatusWord(LEXER_TILE_INVALID))) {
      LEXER_SLEEP(350);
      word = *p;
    }
    status = TxnWordTraits<T>::unpack_status(word);
    value  = TxnWordTraits<T>::unpack_value(word);
  }
};

template<typename T>
__global__ void lexerInitTiles(ScanTileState<T> tiles, uint32_t num_tiles) {
  tiles.InitializeStatus(num_tiles);
}

// Prefix callback for cub::BlockScan (called by the first warp): lane i reads
// tile tile_idx - i - 1; each window is combined with a tail-segmented
// reduction, and the window slides back until an inclusive prefix (or the
// padding before the first tile) is found.  The padding contributes `seed`,
// the value before the first tile.
template<typename T, typename ScanOpT, uint32_t FIRST_DELAY_NS = 450>
struct TilePrefixCallbackOp {
  using StatusWord  = typename ScanTileState<T>::StatusWord;
  using WarpReduceT = cub::WarpReduce<T, WARP>;

  struct TempStorage {
    typename WarpReduceT::TempStorage warp_reduce;
  };

  ScanTileState<T>& tile_state;
  TempStorage&      temp_storage;
  ScanOpT           scan_op;
  int               tile_idx;
  T                 seed;

  __device__ __forceinline__
  TilePrefixCallbackOp(ScanTileState<T>& tile_state, TempStorage& temp_storage,
                       ScanOpT scan_op, int tile_idx, T seed)
      : tile_state(tile_state), temp_storage(temp_storage), scan_op(scan_op),
        tile_idx(tile_idx), seed(seed) {}

  __device__ __forceinline__ T
  ProcessWindow(int predecessor_idx, StatusWord& predecessor_status, uint32_t delay_ns) {
    T value;
    tile_state.WaitForValid(predecessor_idx, predecessor_status, value, delay_ns);
    const int is_oob    = predecessor_status == StatusWord(LEXER_TILE_OOB);
    const int tail_flag = (predecessor_status == StatusWord(LEXER_TILE_INCLUSIVE)) | is_oob;
    const T   eff_value = is_oob ? seed : value;
    // Lane 0 is the nearest predecessor: combine with the operator flipped.
    auto flipped_op = [&](T a, T b) { return scan_op(b, a); };
    return WarpReduceT(temp_storage.warp_reduce).TailSegmentedReduce(eff_value, tail_flag, flipped_op);
  }

  // Called by BlockScan with the block aggregate; returns the exclusive prefix.
  __device__ __forceinline__ T operator()(T block_aggregate) {
    if (threadIdx.x == 0)
      tile_state.SetPartial(tile_idx, block_aggregate);

    int predecessor_idx = tile_idx - (int)threadIdx.x - 1;
    StatusWord predecessor_status;
    T exclusive_prefix = ProcessWindow(predecessor_idx, predecessor_status, FIRST_DELAY_NS);
    while (__all_sync(0xffffffff, predecessor_status != StatusWord(LEXER_TILE_INCLUSIVE)
                               && predecessor_status != StatusWord(LEXER_TILE_OOB))) {
      predecessor_idx -= WARP;
      T window_agg = ProcessWindow(predecessor_idx, predecessor_status, 350);
      exclusive_prefix = scan_op(window_agg, exclusive_prefix);
    }

    T ep;
    if constexpr (sizeof(T) <= sizeof(uint32_t))
      ep = (T)__shfl_sync(0xffffffff, (uint32_t)exclusive_prefix, 0);
    else
      ep = cub::ShuffleIndex<WARP>(exclusive_prefix, 0, 0xffffffff);
    if (threadIdx.x == 0)
      tile_state.SetInclusive(tile_idx, scan_op(ep, block_aggregate));
    return ep;
  }
};

template<uint32_t BLOCK_SIZE>
using LexerBlockScanState = cub::BlockScan<state_t, BLOCK_SIZE, cub::BLOCK_SCAN_WARP_SCANS>;
template<uint32_t BLOCK_SIZE>
using LexerBlockScanMA    = cub::BlockScan<MaxAdd, BLOCK_SIZE, cub::BLOCK_SCAN_WARP_SCANS>;
using LexerPrefixOpState  = TilePrefixCallbackOp<state_t, LexerCompose>;
using LexerPrefixOpMA     = TilePrefixCallbackOp<MaxAdd, MaxAddOp>;

// ---------------------------------------------------------------------------
// Tile shape selection (used by cli.cu).  The template parameters mirror the
// former per-arch tuning interface; the lexer has one tuned shape, so the
// "table" always proposes LEXER_BLOCK_SIZE x LEXER_CHUNK and
// max_items_per_thread() clamps it to the shared memory budget.
// ---------------------------------------------------------------------------
template<uint32_t BLOCK_SIZE>
constexpr size_t lexer_shmem_bytes(uint32_t items_per_thread) {
  return (size_t)BLOCK_SIZE * items_per_thread       // tile
       + 256                                         // byte classes
       + sizeof(typename LexerBlockScanState<BLOCK_SIZE>::TempStorage)
       + sizeof(typename LexerPrefixOpState::TempStorage)
       + sizeof(typename LexerBlockScanMA<BLOCK_SIZE>::TempStorage)
       + sizeof(typename LexerPrefixOpMA::TempStorage);
}

// Largest supported ITEMS_PER_THREAD (96, 64, 32) whose per-block shared
// memory fits in SHARED_MEMORY * USABLE_PCT / 100 bytes.
template<typename I, typename state_t_, typename J, typename length_t_, typename terminal_t_,
         uint32_t BLOCK_SIZE, uint32_t SHARED_MEMORY,
         uint32_t HARD_CAP = 1024, uint32_t USABLE_PCT = 90>
constexpr uint32_t max_items_per_thread() {
  const size_t usable = (size_t)SHARED_MEMORY * USABLE_PCT / 100u;
  for (uint32_t ipt = LEXER_CHUNK; ipt >= 32; ipt -= 32)
    if (ipt <= HARD_CAP && lexer_shmem_bytes<BLOCK_SIZE>(ipt) <= usable)
      return ipt;
  return 32;
}

template<int SM_ARCH, typename state_t_, typename J>
constexpr uint32_t arch_ipt() {
  return LEXER_CHUNK;
}

template<int SM_ARCH, typename state_t_, typename J>
constexpr uint32_t arch_block_size() {
  return LEXER_BLOCK_SIZE;
}

template<typename I, typename J>
struct LexerCtx {

private:
  J offset = 0;
  volatile state_t* d_new_last_state;
  volatile state_t* d_old_last_state;
  I* d_new_size;
  volatile J* d_new_last_start;
  volatile J* d_old_last_start;
  volatile uint32_t* d_len_overflow;  // set to 1 by kernel on length_t overflow

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

  // Descriptors of all tiles: invalid (not yet published), padding OOB.
  void initTiles() {
    const uint32_t blocks = (num_tiles + 255) / 256;
    lexerInitTiles<<<blocks, 256>>>(state_tiles, num_tiles);
    lexerInitTiles<<<blocks, 256>>>(maxadd_tiles, num_tiles);
  }

  void setIdentity(volatile state_t* p) {
    const state_t iden = IDENTITY;
    gpuAssert(cudaMemcpy((void*)p, &iden, sizeof(state_t), cudaMemcpyHostToDevice));
  }

  void updateOffset() {
    offset += CHUNK_SIZE;
  }

public:
  const I CHUNK_SIZE;
  uint32_t num_tiles;
  // Device tables (read by the kernel): compose for the scans and
  // look-backs; byte classes and the step table for the per-byte steps.
  state_t* d_compose;
  uint8_t* d_byte_class;
  state_t* d_step;
  uint32_t num_classes;
  ScanTileState<state_t> state_tiles;
  ScanTileState<MaxAdd>  maxadd_tiles;

  LexerCtx(const I chunk_size,
           const I block_size,
           const I items_per_thread) : CHUNK_SIZE(chunk_size) {
    // Token starts and counts are 31-bit fields of the MaxAdd descriptors.
    if ((uint64_t)chunk_size >= (1ull << 31) - 1) {
      fprintf(stderr, "error: lexer chunk of %llu bytes exceeds 2^31 - 2\n",
              (unsigned long long)chunk_size);
      exit(1);
    }
    num_tiles = numBlocks(chunk_size, block_size, items_per_thread);

    gpuAssert(cudaMalloc(&d_compose, sizeof(h_compose)));
    gpuAssert(cudaMemcpy(d_compose, h_compose, sizeof(h_compose), cudaMemcpyHostToDevice));

    // Byte classes: bytes with the same to_state endofunction.  Step table:
    // step[s * num_classes + c] = compose(s, class c's endofunction).
    std::vector<uint8_t> byte_class(256);
    std::vector<uint32_t> class_endo;
    for (uint32_t b = 0; b < 256; b++) {
      const uint32_t e = get_index_cpu(h_to_state[b]);
      uint32_t c = 0;
      while (c < class_endo.size() && class_endo[c] != e) c++;
      if (c == class_endo.size()) class_endo.push_back(e);
      byte_class[b] = (uint8_t)c;
    }
    num_classes = (uint32_t)class_endo.size();
    std::vector<state_t> step((size_t)NUM_STATES * num_classes);
    for (uint32_t s = 0; s < NUM_STATES; s++)
      for (uint32_t c = 0; c < num_classes; c++)
        step[(size_t)s * num_classes + c] = h_compose[(size_t)s * NUM_STATES + class_endo[c]];
    gpuAssert(cudaMalloc(&d_byte_class, 256));
    gpuAssert(cudaMemcpy(d_byte_class, byte_class.data(), 256, cudaMemcpyHostToDevice));
    gpuAssert(cudaMalloc(&d_step, step.size() * sizeof(state_t)));
    gpuAssert(cudaMemcpy(d_step, step.data(), step.size() * sizeof(state_t), cudaMemcpyHostToDevice));

    gpuAssert(cudaMalloc(&state_tiles.d_tile_descriptors, ScanTileState<state_t>::AllocationSize(num_tiles)));
    gpuAssert(cudaMalloc(&maxadd_tiles.d_tile_descriptors, ScanTileState<MaxAdd>::AllocationSize(num_tiles)));

    gpuAssert(cudaMalloc((void**)&d_new_size, sizeof(I)));
    gpuAssert(cudaMalloc((void**)&d_new_last_state, sizeof(state_t)));
    gpuAssert(cudaMalloc((void**)&d_old_last_state, sizeof(state_t)));
    gpuAssert(cudaMalloc((void**)&d_new_last_start, sizeof(J)));
    gpuAssert(cudaMalloc((void**)&d_old_last_start, sizeof(J)));
    gpuAssert(cudaMalloc((void**)&d_len_overflow, sizeof(uint32_t)));
    reset();
  }

  void reset() {
    offset = 0;
    cudaMemset((void*)d_new_size, 0, sizeof(I));
    setIdentity(d_new_last_state);
    setIdentity(d_old_last_state);
    cudaMemset((void*)d_new_last_start, 0, sizeof(J));
    cudaMemset((void*)d_old_last_start, 0, sizeof(J));
    cudaMemset((void*)d_len_overflow, 0, sizeof(uint32_t));
    initTiles();
  }

  void cleanUp() {
    if (d_compose) cudaFree(d_compose);
    if (d_byte_class) cudaFree(d_byte_class);
    if (d_step) cudaFree(d_step);
    if (state_tiles.d_tile_descriptors) cudaFree(state_tiles.d_tile_descriptors);
    if (maxadd_tiles.d_tile_descriptors) cudaFree(maxadd_tiles.d_tile_descriptors);
    if (d_new_last_start) cudaFree((void*)d_new_last_start);
    if (d_old_last_start) cudaFree((void*)d_old_last_start);
    if (d_new_size) cudaFree((void*)d_new_size);
    if (d_new_last_state) cudaFree((void*)d_new_last_state);
    if (d_old_last_state) cudaFree((void*)d_old_last_state);
    if (d_len_overflow) cudaFree((void*)d_len_overflow);
  }

  __device__ __host__ __forceinline__
  J addOffset(I i) const {
    return (J)i + offset;
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

  __device__ __forceinline__
  void signalLengthOverflow() const {
    if (*d_len_overflow == 0u)
      atomicOr((uint32_t*)d_len_overflow, 1u);
  }

  bool isOverflow() const {
    uint32_t overflow = 0;
    gpuAssert(cudaMemcpy(&overflow, (const void*) d_len_overflow, sizeof(uint32_t), cudaMemcpyDeviceToHost));
    return overflow != 0;
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
    initTiles();
    swapLastStart();
    swapLastState();
    updateOffset();
  }
};

// Position of the k-th (from 0) set bit of m.
__device__ __forceinline__ uint32_t lexer_select_bit(uint32_t m, uint32_t k) {
  uint32_t pos = 0, c;
  c = __popc(m & 0xffffu); if (k >= c) { k -= c; m >>= 16; pos += 16; }
  c = __popc(m & 0xffu);   if (k >= c) { k -= c; m >>= 8;  pos += 8;  }
  c = __popc(m & 0xfu);    if (k >= c) { k -= c; m >>= 4;  pos += 4;  }
  c = __popc(m & 0x3u);    if (k >= c) { k -= c; m >>= 2;  pos += 2;  }
  c = m & 0x1u;            if (k >= c) {                   pos += 1;  }
  return pos;
}

// Highest set bit below position pos of the 96-bit mask (m0, m1, m2), -1 if none.
__device__ __forceinline__ int lexer_last_below(uint32_t m0, uint32_t m1, uint32_t m2, uint32_t pos) {
  const uint32_t b0 = pos >= 32 ? m0 : m0 & ((1u << pos) - 1);
  const uint32_t b1 = pos >= 64 ? m1 : pos < 32 ? 0u : m1 & ((1u << (pos - 32)) - 1);
  const uint32_t b2 = pos < 64 ? 0u : m2 & ((1u << (pos - 64)) - 1);
  return b2 ? 95 - __clz(b2) : b1 ? 63 - __clz(b1) : b0 ? 31 - __clz(b0) : -1;
}

template<typename I, typename J, I BLOCK_SIZE, I ITEMS_PER_THREAD>
__global__ ALPACC_LEXER_BOUNDS(BLOCK_SIZE) void
lexer(LexerCtx<I, J> ctx, uint8_t* d_string, terminal_t* d_terminals, J* d_starts, length_t* d_lengths, const I size, const bool is_last_chunk) {
  constexpr I CHUNK = ITEMS_PER_THREAD;
  static_assert(CHUNK % 32 == 0 && CHUNK <= 96,
                "ITEMS_PER_THREAD: 32, 64 or 96 (multiple of 32 for the swizzle, at most 96 for the bit masks)");
  static_assert(BLOCK_SIZE % WARP == 0 && BLOCK_SIZE >= 256 / 16, "BLOCK_SIZE: whole warps, at least 16 threads");
  static_assert(sizeof(I) == 4, "tile descriptors hold 31-bit positions and counts");
  static_assert(sizeof(state_t) <= 4, "look-back descriptors pack state_t with a status into one word");
  static_assert((TERMINAL_MASK >> TERMINAL_OFFSET) <= 0xff, "terminals are stored as bytes in the tile");
  constexpr I VECS       = CHUNK / 16;
  constexpr I WARP_BYTES = WARP * CHUNK;
  constexpr I TILE       = BLOCK_SIZE * CHUNK;

  __shared__ __align__(16) uint8_t bytes[TILE];   // input bytes, then terminals
  __shared__ typename LexerBlockScanState<BLOCK_SIZE>::TempStorage state_scan;
  __shared__ typename LexerPrefixOpState::TempStorage  state_prefix;
  __shared__ typename LexerBlockScanMA<BLOCK_SIZE>::TempStorage    ma_scan;
  __shared__ typename LexerPrefixOpMA::TempStorage     ma_prefix;
  __shared__ __align__(16) uint8_t byte_class[256];

  if (threadIdx.x < 256 / 16)
    reinterpret_cast<uint4*>(byte_class)[threadIdx.x] =
        reinterpret_cast<const uint4*>(ctx.d_byte_class)[threadIdx.x];

  const LexerCompose compose{ctx.d_compose};
  const state_t* __restrict__ d_step = ctx.d_step;
  const uint32_t num_classes = ctx.num_classes;
  // One byte step from state s.
  auto step = [&](state_t s, uint32_t byte) -> state_t {
    return __ldg(&d_step[(uint32_t)get_index(s) * num_classes + byte_class[byte]]);
  };

  const I tile      = blockIdx.x;
  const I warp      = threadIdx.x / WARP;
  const I lane      = threadIdx.x % WARP;
  const I tile_offs = tile * TILE;
  const bool full   = tile_offs + TILE <= size;
  const I my_offs   = tile_offs + threadIdx.x * CHUNK;          // first byte of my chunk
  const I valid     = my_offs < size ? min(size - my_offs, CHUNK) : 0;
  uint8_t* my_bytes = bytes + threadIdx.x * CHUNK;
  // Swizzle: vector k of thread t's chunk is stored at slot k ^ ((t >> 2) & 1), so
  // the 8 lanes of each 16-byte access phase (chunk stride 96 bytes) hit
  // different bank groups; byte offsets within a chunk flip bit 4.
  const I sw = (threadIdx.x >> 2) & 1;

  // Load the tile: coalesced 16-byte vectors over each warp's segment.
  if (full && (reinterpret_cast<uintptr_t>(d_string) & 15) == 0) {
    const uint4* src = reinterpret_cast<const uint4*>(d_string + tile_offs + warp * WARP_BYTES);
    uint4*       dst = reinterpret_cast<uint4*>(bytes + warp * WARP_BYTES);
#pragma unroll
    for (I k = 0; k < VECS; k++) {
      const I v = lane + k * WARP;   // vector of the warp's segment
      dst[v ^ (((v / VECS) >> 2) & 1)] = src[v];
    }
  } else {
    for (I i = 0; i < CHUNK; i++)
      my_bytes[i ^ (sw << 4)] = i < valid ? d_string[my_offs + i] : 0;
  }
  __syncthreads();   // byte classes and all chunks (the next thread's first byte) loaded

  // Byte after my chunk: next thread's first byte, or the next tile's.
  const I next_gid  = my_offs + CHUNK;
  const bool has_nb = next_gid < size && valid == CHUNK;
  const uint8_t nb  = !has_nb ? 0
                    : threadIdx.x + 1 < BLOCK_SIZE
                      ? my_bytes[CHUNK + ((((threadIdx.x + 1) >> 2) & 1) << 4)]
                    : d_string[next_gid];
  __syncthreads();   // next bytes read before pass B overwrites the chunks

  // Pass A: per-thread state reduction.
  uint4* my = reinterpret_cast<uint4*>(my_bytes);
  state_t agg = IDENTITY;
#pragma unroll
  for (I k = 0; k < VECS; k++) {
    const uint4 v = my[k ^ sw];
#pragma unroll
    for (I b = 0; b < 16; b++) {
      const uint32_t word = b < 4 ? v.x : b < 8 ? v.y : b < 12 ? v.z : v.w;
      const uint32_t byte = (word >> (8 * (b % 4))) & 0xffu;
      if (full || 16 * k + b < valid)
        agg = step(agg, byte);
    }
  }

  // Incoming state of each thread; the state before the first tile is the
  // last state of the previous chunk (IDENTITY for a fresh context).
  state_t prefix;
  {
    LexerPrefixOpState state_op(ctx.state_tiles, state_prefix, compose, (int)tile, ctx.getLastState());
    LexerBlockScanState<BLOCK_SIZE>(state_scan).ExclusiveScan(agg, prefix, compose, state_op);
  }

  // Pass B: rescan from the incoming state.  Bit i of (m0, m1, m2): byte i
  // ends a token (the state after it produces); of (n0, n1, n2): byte i's
  // token is kept (terminal != IGNORE_TOKEN).  Terminals are written in place.
  uint32_t m0 = 0, m1 = 0, m2 = 0;
#ifdef IGNORE_TOKEN
  uint32_t n0 = 0, n1 = 0, n2 = 0;
#else
  const uint32_t n0 = ~0u, n1 = ~0u, n2 = ~0u;
#endif
  // Byte 0 of the chunk starts a token (the previous chunk's last token
  // ended at the chunk boundary); only tracked for the chunk's first byte,
  // other boundaries are the preceding thread's last token end.
  bool start0 = false;
  state_t last = prefix;   // state after my last valid byte
  {
    state_t st = prefix;
#pragma unroll
    for (I k = 0; k < VECS; k++) {
      const uint4 v = my[k ^ sw];
      uint32_t tw[4] = {0, 0, 0, 0};
#pragma unroll
      for (I b = 0; b < 16; b++) {
        const I i = 16 * k + b;
        const uint32_t word = b < 4 ? v.x : b < 8 ? v.y : b < 12 ? v.z : v.w;
        const uint32_t byte = (word >> (8 * (b % 4))) & 0xffu;
        if (full || i < valid) {
          st = step(st, byte);
          if (i == 0) {
            start0 = my_offs == 0 && is_produce(st);
          } else if (is_produce(st)) {   // byte i - 1 ends a token
            if (i - 1 < 32)      m0 |= 1u << (i - 1);
            else if (i - 1 < 64) m1 |= 1u << (i - 1 - 32);
            else                 m2 |= 1u << (i - 1 - 64);
          }
          const uint32_t t = (uint32_t)get_terminal(st);
#ifdef IGNORE_TOKEN
          if (t != (uint32_t)IGNORE_TOKEN) {
            if (i < 32)      n0 |= 1u << i;
            else if (i < 64) n1 |= 1u << (i - 32);
            else             n2 |= 1u << (i - 64);
          }
#endif
          tw[b / 4] |= t << (8 * (b % 4));
          last = st;
        }
      }
      my[k ^ sw] = make_uint4(tw[0], tw[1], tw[2], tw[3]);
    }
  }
  if (valid > 0) {
    // My last byte ends a token if the state after the next byte produces;
    // the input's last byte ends one in the last chunk.
    const I li = valid - 1;
    if (has_nb ? is_produce(step(last, nb)) : is_last_chunk) {
      if (li < 32)      m0 |= 1u << li;
      else if (li < 64) m1 |= 1u << (li - 32);
      else              m2 |= 1u << (li - 64);
    }
  }
  const uint32_t e0 = m0 & n0, e1 = m1 & n1, e2 = m2 & n2;   // emitted tokens
  const I count = __popc(e0) + __popc(e1) + __popc(e2);

  // Output slots and token starts.
  MaxAdd pfx;
  {
    const int last_end = m2 ? 95 - __clz(m2) : m1 ? 63 - __clz(m1) : m0 ? 31 - __clz(m0) : -1;
    const MaxAdd ma{last_end >= 0 ? uint32_t(my_offs + last_end + 2) : start0 ? 1u : 0u, count};
    LexerPrefixOpMA ma_op(ctx.maxadd_tiles, ma_prefix, MaxAddOp(), (int)tile, MaxAdd{0u, 0u});
    LexerBlockScanMA<BLOCK_SIZE>(ma_scan).ExclusiveScan(ma, pfx, MaxAddOp(), ma_op);
  }
  const I offs = pfx.cnt;
  const uint32_t max_in = start0 ? 1u : pfx.max;   // my first token's start + 1 (0: earlier chunk)

  if (valid > 0 && my_offs + valid == size) {   // owner of the last input byte
    ctx.setNewSize(offs + count);
    ctx.setLastState(last);
    // start of the token holding the last byte
    const int prev = lexer_last_below(m0, m1, m2, valid - 1);
    ctx.setLastStart(prev >= 0  ? ctx.addOffset(my_offs + prev + 1)
                   : max_in > 0 ? ctx.addOffset(max_in - 1)
                                : ctx.getLastStart());
  }

  // Pass C: warp-cooperative emission.
  I incl = count;
#pragma unroll
  for (I d = 1; d < WARP; d <<= 1) {
    I y = __shfl_up_sync(0xffffffff, incl, d);
    if (lane >= d) incl += y;
  }
  const I warp_total = __shfl_sync(0xffffffff, incl, WARP - 1);
  const I warp_base  = __shfl_sync(0xffffffff, offs, 0);
  const I warp_offs  = tile_offs + warp * WARP_BYTES;
  const uint8_t* warp_terminals = bytes + warp * WARP_BYTES;
  __syncwarp();   // terminals of all lanes written
  for (I j = 0; j < warp_total; j += WARP) {
    const I r = j + lane;
    // owner lane: smallest o with incl_o > r
    I o = 0;
#pragma unroll
    for (I d = WARP / 2; d >= 1; d >>= 1) {
      I v = __shfl_sync(0xffffffff, incl, o + d - 1);
      if (v <= r) o += d;
    }
    const I o_incl  = __shfl_sync(0xffffffff, incl, o);
    const I o_count = __shfl_sync(0xffffffff, count, o);
    I k = r - (o_incl - o_count);   // rank within the owner's tokens
    const uint32_t oe0 = __shfl_sync(0xffffffff, e0, o);
    const uint32_t oe1 = __shfl_sync(0xffffffff, e1, o);
    const uint32_t oe2 = __shfl_sync(0xffffffff, e2, o);
    // Word holding the k-th set bit, then the bit within it.
    const I c0 = __popc(oe0), c01 = c0 + __popc(oe1);
    const uint32_t m = k < c0 ? oe0 : k < c01 ? oe1 : oe2;
    const I base     = k < c0 ? 0   : k < c01 ? 32  : 64;
    k               -= k < c0 ? 0   : k < c01 ? c0  : c01;
    const I pos = base + lexer_select_bit(m, k);
    // start: after the owner's previous token end (any terminal) below pos,
    // else the owner's incoming token start
    const uint32_t om0   = __shfl_sync(0xffffffff, m0, o);
    const uint32_t om1   = __shfl_sync(0xffffffff, m1, o);
    const uint32_t om2   = __shfl_sync(0xffffffff, m2, o);
    const uint32_t o_max = __shfl_sync(0xffffffff, max_in, o);
    const int prev = lexer_last_below(om0, om1, om2, pos);
    if (r < warp_total) {
      const I chunk = warp_offs + o * CHUNK;
      const J tok_start = prev >= 0  ? ctx.addOffset(chunk + prev + 1)
                        : o_max > 0  ? ctx.addOffset(o_max - 1)
                                     : ctx.getLastStart();
      const J tok_len = ctx.addOffset(chunk + pos + 1) - tok_start;
      if ((length_t)(tok_len) != tok_len) ctx.signalLengthOverflow();
      d_terminals[warp_base + r] =
          (terminal_t)warp_terminals[(o * CHUNK + pos) ^ (((o >> 2) & 1) << 4)];
      d_starts[warp_base + r]  = tok_start;
      d_lengths[warp_base + r] = (length_t)tok_len;
    }
  }
}
