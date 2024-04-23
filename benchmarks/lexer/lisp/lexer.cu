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

template<typename T>
void randomArray(T* array, size_t size, T min, T max) {
    std::default_random_engine generator {static_cast<long unsigned int>(time(0))};
    std::uniform_int_distribution<T> distribution(min, max);
    for (size_t i = 0; i < size; i++) {
        array[i] = distribution(generator);
    }
}

const size_t NUM_STATES = 6;
const size_t NUM_ENDOS = 6;
const size_t NUM_TRANS = 256;

uint8_t h_to_endo[NUM_TRANS] =
        {5, 5, 5, 5, 5, 5, 5, 5, 5, 0, 0, 5, 5, 0, 5, 5,
         5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
         0, 5, 5, 5, 5, 5, 5, 5, 1, 2, 5, 5, 5, 5, 5, 5,
         3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 5, 5, 5, 5, 5, 5,
         5, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
         3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 5, 5, 5, 5, 5,
         5, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
         3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 5, 5, 5, 5, 5,
         5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
         5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
         5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
         5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
         5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
         5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
         5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
         5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5};

uint8_t h_to_state[NUM_ENDOS] = {2, 3, 4, 1, 0, 5};

uint8_t h_to_token[NUM_STATES] = {4, 1, 0, 2, 3, 4};

uint8_t h_compose[NUM_ENDOS * NUM_ENDOS] =
  {0, 0, 0, 0, 0, 5,
   1, 1, 1, 1, 1, 5,
   2, 2, 2, 2, 2, 5,
   3, 3, 3, 3, 3, 5,
   0, 1, 2, 3, 4, 5,
   5, 5, 5, 5, 5, 5};

bool h_is_accepting[NUM_STATES] =
  {false, true, true, true, true, false};

uint8_t states[206] =
  {1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
   2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
   2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
   3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
   3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
   4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
   4,4,4,4,4,4,4,4};

uint8_t symbols[206] =
  {9,10,13,32,40,41,40,41,48,49,50,51,52,53,54,55,56,57,65,66,67,68,
   69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,
   97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,
   114,115,116,117,118,119,120,121,122,9,10,13,32,40,41,48,49,50,51,
   52,53,54,55,56,57,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
   81,82,83,84,85,86,87,88,89,90,97,98,99,100,101,102,103,104,105,106,
   107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,9,
   10,13,32,40,41,48,49,50,51,52,53,54,55,56,57,65,66,67,68,69,70,71,
   72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,97,98,99,
   100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,
   116,117,118,119,120,121,122};

bool h_is_producing[NUM_STATES][NUM_TRANS];

void init_is_producing() {
    for (size_t i = 0; i < NUM_STATES; ++i) {
        for (size_t j = 0; j < NUM_TRANS; ++j) {
            h_is_producing[i][j] = false;
        }
    }
  
    for (size_t i = 0; i < sizeof(states); ++i) {
        h_is_producing[states[i]][symbols[i]] = true;;
    }
}

struct LexerCtx {
    uint8_t* d_to_endo;
    uint8_t* d_to_state;
    uint8_t* d_to_token;
    uint8_t* d_compose;
    bool* d_is_producing;
};

typedef struct __attribute__((__packed__)) {
    uint32_t start, end;
    uint8_t token;
} token;

void init_ctx(LexerCtx* ctx) {
    init_is_producing();
    cudaMalloc(&ctx->d_to_endo, sizeof(h_to_endo));
    cudaMemcpy(ctx->d_to_endo, h_to_endo, sizeof(h_to_endo),
               cudaMemcpyHostToDevice);
    cudaMalloc(&ctx->d_to_state, sizeof(h_to_state));
    cudaMemcpy(ctx->d_to_state, h_to_state, sizeof(h_to_state),
               cudaMemcpyHostToDevice);
    cudaMalloc(&ctx->d_to_token, sizeof(h_to_token));
    cudaMemcpy(ctx->d_to_token, h_to_token,
               sizeof(h_to_token), cudaMemcpyHostToDevice);
    cudaMalloc(&ctx->d_compose, sizeof(h_compose));
    cudaMemcpy(ctx->d_compose, h_compose, sizeof(h_compose),
               cudaMemcpyHostToDevice);
    cudaMalloc(&ctx->d_is_producing, sizeof(h_is_producing));
    cudaMemcpy(ctx->d_is_producing, h_is_producing,
               sizeof(h_is_producing), cudaMemcpyHostToDevice);
}

void cleanup_ctx(LexerCtx* ctx) {
    if (ctx->d_to_endo) cudaFree(ctx->d_to_endo);
    if (ctx->d_to_state) cudaFree(ctx->d_to_state);
    if (ctx->d_to_token) cudaFree(ctx->d_to_token);
    if (ctx->d_compose) cudaFree(ctx->d_compose);
    if (ctx->d_is_producing) cudaFree(ctx->d_is_producing);
}

__global__ void to_padf_kernel(uint8_t* d_to_endo,
                               uint8_t* d_in,
                               uint8_t* d_out,
                               uint32_t* indices,
                               size_t size) {
    const uint32_t gid = blockIdx.x * blockDim.x + threadIdx.x;
    if (gid < size) {
        d_out[gid] = d_to_endo[d_in[gid]];
        indices[gid] = gid;
    }
}

void to_padf(uint8_t* d_to_endo,
             uint8_t* d_in,
             uint8_t* d_out,
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
    uint8_t* d_compose;

    __host__ __forceinline__
    explicit  Compose(uint8_t* d_compose) : d_compose(d_compose) {}

    __device__ __forceinline__
    uint8_t operator()(const uint8_t &a, const uint8_t &b) const {
        return d_compose[b * NUM_ENDOS + a];
    }
};

struct IsProducing {
    uint8_t* d_str;
    uint8_t* d_endo;
    bool* d_is_producing;
    uint8_t* d_to_state;
    size_t size;

    __host__ __forceinline__
    explicit IsProducing(uint8_t* d_str,
                         uint8_t* d_endo,
                         bool* d_is_producing,
                         uint8_t* d_to_state,
                         size_t size) : d_str(d_str),
                                        d_endo(d_endo),
                                        d_is_producing(d_is_producing),
                                        d_to_state(d_to_state),
                                        size(size) {}
  
    __device__ __forceinline__
    bool operator()(const uint32_t &i) const {
      uint32_t j = (i + 1) % size;
      uint32_t idx = NUM_TRANS * d_to_state[d_endo[i]] + d_str[j];
      bool is_producing = d_is_producing[idx];
      return i == size - 1 || is_producing;
    }
};

__global__ void to_token_kernel(uint8_t* d_to_state,
                                uint8_t* d_to_token,
                                uint8_t* d_endo,
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
        uint8_t token = d_to_token[d_to_state[d_endo[i]]];
        d_out[gid].start = start;
        d_out[gid].end = end;
        d_out[gid].token = token;
    }
}

void to_token(uint8_t* d_to_state,
              uint8_t* d_to_token,
              uint8_t* d_endo,
              uint32_t* d_in,
              token* d_out,
              size_t size) {
    size_t block_size = 256;
    size_t grid_size = (size + block_size - 1) / block_size;
    to_token_kernel<<<grid_size, block_size>>>(d_to_state,
                                               d_to_token,
                                               d_endo,
                                               d_in,
                                               d_out,
                                               size);
}

struct IsIgnore {
    __device__ __forceinline__
    bool operator()(const token &t) const {
      return t.token != 0;
    }
};

void lex(LexerCtx* ctx,
         uint8_t* d_str,
         uint8_t* d_temp,
         uint8_t* d_endo,
         uint32_t* d_indices_in,
         uint32_t* d_indices_out,
         token** tokens,
         size_t* num_tokens,
         size_t size) {
    assert(*tokens == NULL);
    Compose compose(ctx->d_compose);

    void* d_temp_storage = NULL;
    size_t temp_storage_bytes = 0;

    size_t* d_num_tokens = NULL;
    cudaMalloc((void**) &d_num_tokens, sizeof(size_t));
    
    to_padf(ctx->d_to_endo, d_str, d_temp, d_indices_in, size);
    cudaDeviceSynchronize();
    
    cub::DeviceScan::InclusiveScan(d_temp_storage, temp_storage_bytes,
                                   d_temp, d_endo, compose, size);
    cudaMalloc(&d_temp_storage, temp_storage_bytes);
    cub::DeviceScan::InclusiveScan(d_temp_storage, temp_storage_bytes,
                                   d_temp, d_endo, compose, size);
    cudaDeviceSynchronize();
    
    cudaFree(d_temp_storage);
    d_temp_storage = NULL;
    temp_storage_bytes = 0;
    
    IsProducing is_producing(d_str, d_endo, ctx->d_is_producing,
                             ctx->d_to_state, size);
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
        uint8_t last_endo = 0;
        cudaMemcpy(&last_endo, &d_endo[size - 1], sizeof(uint8_t),
                   cudaMemcpyDeviceToHost);
        is_valid = h_is_accepting[h_to_state[last_endo]];
    }
    
    if (is_valid) {
        cudaMalloc((void**) tokens, 2 * num_tokens_temp * sizeof(token));
        token* temp_tokens = &((*tokens)[num_tokens_temp]);
        
        to_token(ctx->d_to_state, ctx->d_to_token, d_endo,
                 d_indices_out, temp_tokens, num_tokens_temp);
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
    uint8_t* str;
    size_t size = 0;
    str = read_file(argv[1], &size);
    
    LexerCtx ctx;
    init_ctx(&ctx);
    uint8_t* d_in;
    uint8_t* d_temp;
    uint8_t* d_endo;
    uint32_t* d_indices_in;
    uint32_t* d_indices_out;
    cudaMalloc((void**) &d_indices_in, sizeof(uint32_t) * size);
    cudaMalloc((void**) &d_indices_out, sizeof(uint32_t) * size);
    cudaMalloc((void**) &d_in, sizeof(uint8_t) * size);
    cudaMalloc((void**) &d_temp, sizeof(uint8_t) * size);
    cudaMalloc((void**) &d_endo, sizeof(uint8_t) * size);
    cudaMemcpy(d_in, str, sizeof(uint8_t) * size, cudaMemcpyHostToDevice);

    size_t num_tokens = 0;
    token* d_tokens = NULL;
    lex(&ctx, d_in, d_temp, d_endo, d_indices_in, d_indices_out,
        &d_tokens, &num_tokens, size);
    assert(d_tokens != NULL);
    token* h_tokens = (token*) malloc(sizeof(token) * num_tokens);
    cudaMemcpy(h_tokens, d_tokens, sizeof(token) * num_tokens,
               cudaMemcpyDeviceToHost);


    // Warmup
    for (size_t i = 0; i < 50; ++i) {
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
    printf("#token: %li\n", num_tokens);

    compute_descriptors(times, runs);


    /*
    for (size_t i = 0; i < num_tokens; i++) { 
        printf("token=%i; span=(%i,%i)\n", h_tokens[i].token,
               h_tokens[i].start, h_tokens[i].end);
    }
    */

    free(str);
    free(h_tokens);
    cudaFree(d_tokens);
    cudaFree(d_in);
    cudaFree(d_temp);
    cudaFree(d_endo);
    cudaFree(d_indices_in);
    cudaFree(d_indices_out);
    cleanup_ctx(&ctx);
    return 0;
}

