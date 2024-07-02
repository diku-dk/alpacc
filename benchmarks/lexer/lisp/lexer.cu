using Token = char;
using State = unsigned short;
using Index = unsigned int;
using Char = unsigned char;
using Size = unsigned long;

const Size NUM_STATES = 12;
const Size NUM_TRANS = 256;
const Token IGNORE_TOKEN = 0;
const State ENDO_MASK = 15;
const State ENDO_OFFSET = 0;
const State TERMINAL_MASK = 112;
const State TERMINAL_OFFSET = 4;
const State ACCEPT_MASK = 128;
const State ACCEPT_OFFSET = 7;
const State PRODUCE_MASK = 256;
const State PRODUCE_OFFSET = 8;

State h_to_state[NUM_TRANS] =
        {75, 75, 75, 75, 75, 75, 75, 75, 75, 128, 128, 75, 75, 128,
         75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75,
         75, 75, 75, 128, 75, 75, 75, 75, 75, 75, 75, 161, 178, 75,
         75, 75, 75, 75, 75, 147, 147, 147, 147, 147, 147, 147, 147,
         147, 147, 75, 75, 75, 75, 75, 75, 75, 147, 147, 147, 147,
         147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147,
         147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 75, 75,
         75, 75, 75, 75, 147, 147, 147, 147, 147, 147, 147, 147, 147,
         147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147,
         147, 147, 147, 147, 147, 75, 75, 75, 75, 75, 75, 75, 75, 75,
         75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75,
         75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75,
         75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75,
         75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75,
         75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75,
         75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75,
         75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75,
         75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75,
         75, 75, 75, 75};

State h_compose[NUM_STATES * NUM_STATES] =
    {132, 392, 392, 392, 132, 392, 392, 392, 132, 392, 128, 75,
     421, 421, 421, 421, 421, 421, 421, 421, 421, 421, 161, 75,
     438, 438, 438, 438, 438, 438, 438, 438, 438, 438, 178, 75,
     407, 407, 407, 153, 407, 407, 407, 153, 407, 153, 147, 75,
     132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 75,
     421, 421, 421, 421, 421, 421, 421, 421, 421, 421, 421, 75,
     438, 438, 438, 438, 438, 438, 438, 438, 438, 438, 438, 75,
     407, 407, 407, 407, 407, 407, 407, 407, 407, 407, 407, 75,
     392, 392, 392, 392, 392, 392, 392, 392, 392, 392, 392, 75,
     153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 75,
     128, 161, 178, 147, 132, 421, 438, 407, 392, 153, 74, 75,
     75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75};

#include <sys/time.h>
#include <cub/cub.cuh>

int timeval_subtract(struct timeval *result, struct timeval *t2, struct timeval *t1) {
    unsigned int resolution = 1000000;
    long int diff = (t2->tv_usec + resolution * t2->tv_sec) - (t1->tv_usec + resolution * t1->tv_sec);
    result->tv_sec = diff / resolution;
    result->tv_usec = diff % resolution;
    return (diff < 0);
}

void compute_descriptors(timeval* measurements, Size size) {   
    double sample_mean = 0.0;
    double sample_variance = 0.0;
    double d_size = (double) size;
    double diff;
    timeval t_diff;
    
    for (Size i = 0; i < size; i++) {
        t_diff = measurements[i];
        diff = t_diff.tv_sec * 1e6 + t_diff.tv_usec;
        sample_mean += diff / d_size;
        sample_variance += (diff * diff) / d_size;
    }

    double sample_std = sqrt(sample_variance);
    double bound = (0.95 * sample_std) / sqrt(d_size - 1);

    printf("Average time: %.0lfμs", sample_mean);
    printf(" (95%% CI: [%.0lfμs, %.0lfμs])\n", sample_mean - bound, sample_mean + bound);
}

__device__ __forceinline__ State get_index(State state) {
    return (state & ENDO_MASK) >> ENDO_OFFSET;
}

__device__ __forceinline__ Token get_terminal(State state) {
    return (state & TERMINAL_MASK) >> TERMINAL_OFFSET;
}

bool is_accept(State state) {
    return (state & ACCEPT_MASK) >> ACCEPT_OFFSET;
}

__device__ __forceinline__ bool is_produce(State state) {
    return (state & PRODUCE_MASK) >> PRODUCE_OFFSET;
}

struct LexerCtx {
    State* d_to_state;
    Size to_state_mem_size;
    State* d_compose;
};

typedef struct __attribute__((__packed__)) {
    Index start, end;
    Token token;
} TokenSpan;

void init_ctx(LexerCtx* ctx) {
    cudaMalloc(&ctx->d_to_state, sizeof(h_to_state));
    cudaMemcpy(ctx->d_to_state, h_to_state, sizeof(h_to_state),
               cudaMemcpyHostToDevice);
    cudaMalloc(&ctx->d_compose, sizeof(h_compose));
    cudaMemcpy(ctx->d_compose, h_compose, sizeof(h_compose),
               cudaMemcpyHostToDevice);
}

void cleanup_ctx(LexerCtx* ctx) {
    if (ctx->d_to_state) cudaFree(ctx->d_to_state);
    if (ctx->d_compose) cudaFree(ctx->d_compose);
}

__global__ void to_padf_kernel(State* d_to_state,
                               Char* d_in,
                               State* d_out,
                               Index* indices,
                               Size size) {
    const Index gid = blockIdx.x * blockDim.x + threadIdx.x;

    if (gid < size) {
        d_out[gid] = d_to_state[d_in[gid]];
        indices[gid] = gid;
    }
}

void to_padf(State* d_to_state,
             Char* d_in,
             State* d_out,
             Index* indices,
             Size size) {
    Size block_size = 256;
    Size grid_size = (size + block_size - 1) / block_size;
    to_padf_kernel<<<grid_size, block_size>>>(d_to_state,
                                              d_in,
                                              d_out,
                                              indices,
                                              size);
}

struct Compose {
    State* d_compose;

    __host__ __forceinline__
    explicit Compose(State* d_compose) : d_compose(d_compose) {}

    __device__ __forceinline__
    State operator()(const State &a, const State &b) const {
        return d_compose[get_index(b) * NUM_STATES + get_index(a)];
    }
};

struct IsProducing {
    State* d_state;
    Size size;

    __host__ __forceinline__
    explicit IsProducing(State* d_state, Size size) : d_state(d_state), size(size) {}
  
    __device__ __forceinline__
    bool operator()(const Index &i) const {
        Index j = (i + 1) % size;
        return i == size - 1 || is_produce(d_state[j]);
    }
};

__global__ void to_token_kernel(State* d_state,
                                Index* d_in,
                                TokenSpan* d_out,
                                Size size) {
    const Index gid = blockIdx.x * blockDim.x + threadIdx.x;
    if (gid < size) {
        Index i = d_in[gid];
        Index start = 0;
        if (gid != 0) {
            start = 1 + d_in[gid - 1];
        }
        Index end = i + 1;
        d_out[gid].start = start;
        d_out[gid].end = end;
        d_out[gid].token = get_terminal(d_state[i]);
    }
}

void to_token(State* d_state,
              Index* d_in,
              TokenSpan* d_out,
              Size size) {
    Size block_size = 256;
    Size grid_size = (size + block_size - 1) / block_size;
    to_token_kernel<<<grid_size, block_size>>>(d_state, d_in, d_out, size);
}

struct IsIgnore {
    __device__ __forceinline__
    bool operator()(const TokenSpan &t) const {
      return t.token != IGNORE_TOKEN;
    }
};

void lex(LexerCtx* ctx,
         Char* d_str,
         State* d_temp,
         State* d_state,
         Index* d_indices_in,
         Index* d_indices_out,
         TokenSpan** tokens,
         Size* num_tokens,
         Size size) {
    assert(*tokens == NULL);
    assert(size != 0);

    Compose compose(ctx->d_compose);

    void* d_temp_storage = NULL;
    Size temp_storage_bytes = 0;

    Size* d_num_tokens = NULL;
    cudaMalloc((void**) &d_num_tokens, sizeof(Size));
    
    to_padf(ctx->d_to_state, d_str, d_temp, d_indices_in, size);
    
    cub::DeviceScan::InclusiveScan(d_temp_storage, temp_storage_bytes,
                                   d_temp, d_state, compose, size);
    cudaMalloc(&d_temp_storage, temp_storage_bytes);
    cub::DeviceScan::InclusiveScan(d_temp_storage, temp_storage_bytes,
                                   d_temp, d_state, compose, size);
    cudaDeviceSynchronize();
    
    cudaFree(d_temp_storage);
    d_temp_storage = NULL;
    temp_storage_bytes = 0;
    
    IsProducing is_producing(d_state, size);
    cub::DevicePartition::If(d_temp_storage, temp_storage_bytes,
                             d_indices_in, d_indices_out,
                             d_num_tokens, size, is_producing);
    cudaMalloc(&d_temp_storage, temp_storage_bytes);
    cub::DevicePartition::If(d_temp_storage, temp_storage_bytes,
                             d_indices_in, d_indices_out,
                             d_num_tokens, size, is_producing);
    cudaDeviceSynchronize();
    
    cudaFree(d_temp_storage);
    d_temp_storage = NULL;
    temp_storage_bytes = 0;
    
    Size num_tokens_temp = 0;
    cudaMemcpy(&num_tokens_temp, d_num_tokens, sizeof(Size),
               cudaMemcpyDeviceToHost);

    bool is_valid = true;
    if (size != 0) {
        State last_state = 0;
        cudaMemcpy(&last_state, &d_state[size - 1], sizeof(State),
                   cudaMemcpyDeviceToHost);
        is_valid = is_accept(last_state);
        assert(is_valid);
    }
    
    if (is_valid) {
        cudaMalloc((void**) tokens, 2 * num_tokens_temp * sizeof(TokenSpan));
        TokenSpan* temp_tokens = &((*tokens)[num_tokens_temp]);
        
        to_token(d_state, d_indices_out, temp_tokens, num_tokens_temp);
        cudaDeviceSynchronize();

        IsIgnore is_ignore;

        
        cub::DevicePartition::If(d_temp_storage, temp_storage_bytes,
                                 temp_tokens, *tokens,
                                 d_num_tokens, num_tokens_temp, is_ignore);
        cudaMalloc(&d_temp_storage, temp_storage_bytes);
        cub::DevicePartition::If(d_temp_storage, temp_storage_bytes,
                                 temp_tokens, *tokens,
                                 d_num_tokens, num_tokens_temp, is_ignore);
        cudaDeviceSynchronize();
        // cudaFree(temp_tokens);
        cudaFree(d_temp_storage);
        cudaMemcpy(num_tokens, d_num_tokens, sizeof(Size),
                   cudaMemcpyDeviceToHost);
    }
    
    cudaFree(d_num_tokens);
}

const Token EMPTY_TOKEN = 127;

__global__ void to_config_kernel(State* d_to_state,
                                 Char* d_in,
                                 State* d_out,
                                 Index* indices,
                                 Size size) {
    const Index gid = blockIdx.x * blockDim.x + threadIdx.x;

    if (gid < size) {
        d_out[gid] = d_to_state[d_in[gid]];
        indices[gid] = gid;
    }
}

void to_config(State* d_to_state,
              Char* d_in,
              State* d_out,
              Index* indices,
              Size size) {
    Size block_size = 256;
    Size grid_size = (size + block_size - 1) / block_size;
    to_config_kernel<<<grid_size, block_size>>>(d_to_state,
                                                d_in,
                                                d_out,
                                                indices,
                                                size);
}

Char* read_file(const char* filename, Size* size) {
    FILE* file = fopen(filename, "rb");
    if (file == NULL) {
        fprintf(stderr, "Error opening file %s\n", filename);
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    *size = ftell(file);
    fseek(file, 0, SEEK_SET);

    Char* buffer = (Char*) malloc(*size);
    if (buffer == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        fclose(file);
        return NULL;
    }

    Size bytes = fread(buffer, 1, *size, file);
    if (bytes != *size) {
        fprintf(stderr, "Error reading file %s\n", filename);
        free(buffer);
        fclose(file);
        return NULL;
    }

    fclose(file);
    return buffer;
}

int main(int argc, char** argv) {
    assert(argc==2);
    Size size = 0;
    Char* str;
    str = read_file(argv[1], &size);

    LexerCtx ctx;
    init_ctx(&ctx);
    Char* d_in;
    State* d_temp;
    State* d_state;
    Index* d_indices_in;
    Index* d_indices_out;
    cudaMalloc((void**) &d_indices_in, sizeof(Index) * size);
    cudaMalloc((void**) &d_indices_out, sizeof(Index) * size);
    cudaMalloc((void**) &d_in, sizeof(Char) * size);
    cudaMalloc((void**) &d_temp, sizeof(State) * size);
    cudaMalloc((void**) &d_state, sizeof(State) * size);
    cudaMemcpy(d_in, str, sizeof(Char) * size,
               cudaMemcpyHostToDevice);

    // Warmup
    for (Size i = 0; i < 100; ++i) {
        Size num_tokens = 0;
        TokenSpan* d_tokens = NULL;
        lex(&ctx, d_in, d_temp, d_state, d_indices_in, d_indices_out,
            &d_tokens, &num_tokens, size);
        assert(d_tokens != NULL);
        cudaFree(d_tokens);
    }
    
    
    const Size runs = 10;
    timeval times[runs];
    timeval prev, curr, t_diff;

    for (Size i = 0; i < runs; ++i) {
        Size num_tokens = 0;
        TokenSpan* d_tokens = NULL;
        gettimeofday(&prev, NULL);
        lex(&ctx, d_in, d_temp, d_state, d_indices_in, d_indices_out,
            &d_tokens, &num_tokens, size);
        gettimeofday(&curr, NULL);
        assert(d_tokens != NULL);
        cudaFree(d_tokens);
        timeval_subtract(&t_diff, &curr, &prev);
        times[i] = t_diff;
    }

    compute_descriptors(times, runs);

    Size num_tokens = 0;
    TokenSpan* d_tokens = NULL;
    lex(&ctx, d_in, d_temp, d_state, d_indices_in, d_indices_out,
        &d_tokens, &num_tokens, size);
    TokenSpan* h_tokens = (TokenSpan*) malloc(sizeof(TokenSpan) * num_tokens);
    cudaMemcpy(h_tokens, d_tokens, sizeof(TokenSpan) * num_tokens,
               cudaMemcpyDeviceToHost);
    printf("#token: %li\n", num_tokens);
    /*
    for (Size i = 0; i < num_tokens; i++) { 
        printf("token=%i; span=(%i,%i)\n", h_tokens[i].token,
               h_tokens[i].start, h_tokens[i].end);
    }
    */
    free(h_tokens);
    cudaFree(d_tokens);

    free(str);
    cudaFree(d_in);
    cudaFree(d_temp);
    cudaFree(d_state);
    cudaFree(d_indices_in);
    cudaFree(d_indices_out);
    cleanup_ctx(&ctx);
    return 0;
}

