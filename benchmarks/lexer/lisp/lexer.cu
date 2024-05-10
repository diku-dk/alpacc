#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <cuda_runtime.h>
#include <stdint.h>
#include <time.h>
#include <assert.h>
#include <stdlib.h>
#include <limits.h>
#include <sys/time.h>
#include <stdint.h>
#include <iostream>
#include <random>
#include <utility>
#include <cub/cub.cuh>

int timeval_subtract(struct timeval *result, struct timeval *t2, struct timeval *t1) {
    unsigned int resolution = 1000000;
    long int diff = (t2->tv_usec + resolution * t2->tv_sec) - (t1->tv_usec + resolution * t1->tv_sec);
    result->tv_sec = diff / resolution;
    result->tv_usec = diff % resolution;
    return (diff < 0);
}

void compute_descriptors(timeval* measurements, size_t size) {   
    double sample_mean = 0.0;
    double sample_variance = 0.0;
    double d_size = (double) size;
    double diff;
    timeval t_diff;
    
    for (size_t i = 0; i < size; i++) {
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

const size_t NUM_ENDOS = 12;
const size_t NUM_TRANS = 256;
const uint8_t IGNORE_TOKEN = 0;
const uint16_t ENDO_MASK = 15;
const uint16_t ENDO_OFFSET = 0;
const uint16_t TERMINAL_MASK = 112;
const uint16_t TERMINAL_OFFSET = 4;
const uint16_t ACCEPT_MASK = 128;
const uint16_t ACCEPT_OFFSET = 7;
const uint16_t PRODUCE_MASK = 256;
const uint16_t PRODUCE_OFFSET = 8;

__device__ __forceinline__ uint16_t get_index(uint16_t endo) {
    return (endo & ENDO_MASK) >> ENDO_OFFSET;
}

__device__ __forceinline__ uint8_t get_terminal(uint16_t endo) {
    return (endo & TERMINAL_MASK) >> TERMINAL_OFFSET;
}

bool is_accept(uint16_t endo) {
    return (endo & ACCEPT_MASK) >> ACCEPT_OFFSET;
}

__device__ __forceinline__ bool is_produce(uint16_t endo) {
    return (endo & PRODUCE_MASK) >> PRODUCE_OFFSET;
}

uint16_t h_to_endo[NUM_TRANS] =
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

uint16_t h_compose[NUM_ENDOS * NUM_ENDOS] =
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

struct LexerCtx {
    uint16_t* d_to_endo;
    size_t to_endo_mem_size;
    uint16_t* d_compose;
};

typedef struct __attribute__((__packed__)) {
    uint32_t start, end;
    uint8_t token;
} token;

void init_ctx(LexerCtx* ctx) {
    cudaMalloc(&ctx->d_to_endo, sizeof(h_to_endo));
    cudaMemcpy(ctx->d_to_endo, h_to_endo, sizeof(h_to_endo),
               cudaMemcpyHostToDevice);
    cudaMalloc(&ctx->d_compose, sizeof(h_compose));
    cudaMemcpy(ctx->d_compose, h_compose, sizeof(h_compose),
               cudaMemcpyHostToDevice);
}

void cleanup_ctx(LexerCtx* ctx) {
    if (ctx->d_to_endo) cudaFree(ctx->d_to_endo);
    if (ctx->d_compose) cudaFree(ctx->d_compose);
}

__global__ void to_padf_kernel(uint16_t* d_to_endo,
                               uint8_t* d_in,
                               uint16_t* d_out,
                               uint32_t* indices,
                               size_t size) {
    const uint32_t gid = blockIdx.x * blockDim.x + threadIdx.x;
    __syncthreads();

    if (gid < size) {
        d_out[gid] = d_to_endo[d_in[gid]];
        indices[gid] = gid;
    }
}

void to_padf(uint16_t* d_to_endo,
             uint8_t* d_in,
             uint16_t* d_out,
             uint32_t* indices,
             size_t size) {
    size_t block_size = 256;
    size_t grid_size = (size + block_size - 1) / block_size;
    to_padf_kernel<<<grid_size, block_size>>>(d_to_endo,
                                              d_in,
                                              d_out,
                                              indices,
                                              size);
}

struct Compose {
    uint16_t* d_compose;

    __host__ __forceinline__
    explicit Compose(uint16_t* d_compose) : d_compose(d_compose) {}

    __device__ __forceinline__
    uint16_t operator()(const uint16_t &a, const uint16_t &b) const {
        return d_compose[get_index(b) * NUM_ENDOS + get_index(a)];
    }
};

struct IsProducing {
    uint16_t* d_endo;
    size_t size;

    __host__ __forceinline__
    explicit IsProducing(uint16_t* d_endo, size_t size) : d_endo(d_endo), size(size) {}
  
    __device__ __forceinline__
    bool operator()(const uint32_t &i) const {
        uint32_t j = (i + 1) % size;
        return i == size - 1 || is_produce(d_endo[j]);
    }
};

__global__ void to_token_kernel(uint16_t* d_endo,
                                uint32_t* d_in,
                                token* d_out,
                                size_t size) {
    const uint32_t gid = blockIdx.x * blockDim.x + threadIdx.x;
    if (gid < size) {
        uint32_t i = d_in[gid];
        uint32_t start = 0;
        if (gid != 0) {
            start = 1 + d_in[gid - 1];
        }
        uint32_t end = i + 1;
        d_out[gid].start = start;
        d_out[gid].end = end;
        d_out[gid].token = get_terminal(d_endo[i]);
    }
}

void to_token(uint16_t* d_endo,
              uint32_t* d_in,
              token* d_out,
              size_t size) {
    size_t block_size = 256;
    size_t grid_size = (size + block_size - 1) / block_size;
    to_token_kernel<<<grid_size, block_size>>>(d_endo, d_in, d_out, size);
}

struct IsIgnore {
    __device__ __forceinline__
    bool operator()(const token &t) const {
      return t.token != IGNORE_TOKEN;
    }
};

void lex(LexerCtx* ctx,
         uint8_t* d_str,
         uint16_t* d_temp,
         uint16_t* d_endo,
         uint32_t* d_indices_in,
         uint32_t* d_indices_out,
         token** tokens,
         size_t* num_tokens,
         size_t size) {
    timeval prev, curr, t_diff;
    double diff;
    assert(*tokens == NULL);
    assert(size != 0);

    Compose compose(ctx->d_compose);

    void* d_temp_storage = NULL;
    size_t temp_storage_bytes = 0;

    size_t* d_num_tokens = NULL;
    cudaMalloc((void**) &d_num_tokens, sizeof(size_t));
    
    to_padf(ctx->d_to_endo, d_str, d_temp, d_indices_in, size);
    /*
    gettimeofday(&prev, NULL);
    cudaDeviceSynchronize();
    gettimeofday(&curr, NULL);
    timeval_subtract(&t_diff, &curr, &prev);
    diff = t_diff.tv_sec * 1e6 + t_diff.tv_usec;
    printf("time=%f\n", diff);
    */
    
    cub::DeviceScan::InclusiveScan(d_temp_storage, temp_storage_bytes,
                                   d_temp, d_endo, compose, size);
    cudaMalloc(&d_temp_storage, temp_storage_bytes);
    cub::DeviceScan::InclusiveScan(d_temp_storage, temp_storage_bytes,
                                   d_temp, d_endo, compose, size);
    cudaDeviceSynchronize();
    
    cudaFree(d_temp_storage);
    d_temp_storage = NULL;
    temp_storage_bytes = 0;
    
    IsProducing is_producing(d_endo, size);
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
    
    size_t num_tokens_temp = 0;
    cudaMemcpy(&num_tokens_temp, d_num_tokens, sizeof(size_t),
               cudaMemcpyDeviceToHost);

    bool is_valid = true;
    if (size != 0) {
        uint16_t last_endo = 0;
        cudaMemcpy(&last_endo, &d_endo[size - 1], sizeof(uint16_t),
                   cudaMemcpyDeviceToHost);
        is_valid = is_accept(last_endo);
        assert(is_valid);
    }
    
    if (is_valid) {
        cudaMalloc((void**) tokens, 2 * num_tokens_temp * sizeof(token));
        token* temp_tokens = &((*tokens)[num_tokens_temp]);
        
        to_token(d_endo, d_indices_out, temp_tokens, num_tokens_temp);
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
        cudaMemcpy(num_tokens, d_num_tokens, sizeof(size_t),
                   cudaMemcpyDeviceToHost);
    }
    
    cudaFree(d_num_tokens);
}

uint8_t* read_file(const char* filename, size_t* size) {
    FILE* file = fopen(filename, "rb");
    if (file == NULL) {
        fprintf(stderr, "Error opening file %s\n", filename);
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    *size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = (uint8_t*) malloc(*size);
    if (buffer == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        fclose(file);
        return NULL;
    }

    size_t bytes = fread(buffer, 1, *size, file);
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
    size_t size = 0;
    uint8_t* str;
    str = read_file(argv[1], &size);

    LexerCtx ctx;
    init_ctx(&ctx);
    uint8_t* d_in;
    uint16_t* d_temp;
    uint16_t* d_endo;
    uint32_t* d_indices_in;
    uint32_t* d_indices_out;
    cudaMalloc((void**) &d_indices_in, sizeof(uint32_t) * size);
    cudaMalloc((void**) &d_indices_out, sizeof(uint32_t) * size);
    cudaMalloc((void**) &d_in, sizeof(uint8_t) * size);
    cudaMalloc((void**) &d_temp, sizeof(uint16_t) * size);
    cudaMalloc((void**) &d_endo, sizeof(uint16_t) * size);
    cudaMemcpy(d_in, str, sizeof(uint8_t) * size,
               cudaMemcpyHostToDevice);

    // Warmup
    for (size_t i = 0; i < 100; ++i) {
        size_t num_tokens = 0;
        token* d_tokens = NULL;
        lex(&ctx, d_in, d_temp, d_endo, d_indices_in, d_indices_out,
            &d_tokens, &num_tokens, size);
        assert(d_tokens != NULL);
        cudaFree(d_tokens);
    }
    
    
    const size_t runs = 10;
    timeval times[runs];
    timeval prev, curr, t_diff;

    for (size_t i = 0; i < runs; ++i) {
        size_t num_tokens = 0;
        token* d_tokens = NULL;
        gettimeofday(&prev, NULL);
        lex(&ctx, d_in, d_temp, d_endo, d_indices_in, d_indices_out,
            &d_tokens, &num_tokens, size);
        gettimeofday(&curr, NULL);
        assert(d_tokens != NULL);
        cudaFree(d_tokens);
        timeval_subtract(&t_diff, &curr, &prev);
        times[i] = t_diff;
    }


    compute_descriptors(times, runs);

    size_t num_tokens = 0;
    token* d_tokens = NULL;
    lex(&ctx, d_in, d_temp, d_endo, d_indices_in, d_indices_out,
        &d_tokens, &num_tokens, size);
    token* h_tokens = (token*) malloc(sizeof(token) * num_tokens);
    cudaMemcpy(h_tokens, d_tokens, sizeof(token) * num_tokens,
               cudaMemcpyDeviceToHost);
    printf("#token: %li\n", num_tokens);
    /*
    for (size_t i = 0; i < num_tokens; i++) { 
        printf("token=%i; span=(%i,%i)\n", h_tokens[i].token,
               h_tokens[i].start, h_tokens[i].end);
    }
    */
    free(h_tokens);
    cudaFree(d_tokens);

    free(str);
    cudaFree(d_in);
    cudaFree(d_temp);
    cudaFree(d_endo);
    cudaFree(d_indices_in);
    cudaFree(d_indices_out);
    cleanup_ctx(&ctx);
    return 0;
}

