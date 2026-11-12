// Test main for cuda/pse.cu, appended after cuda/common.cu + cuda/scan.cu +
// cuda/pse.cu by tests/testpse.sh (emulating the generator concatenation).
//
// Checks both PSE semantics (strict <, and <= via INCL=true) against a CPU
// stack reference across input patterns and sizes, including the ±1 depth
// walks the LLP parser will feed the kernel for bracket matching.
#include <stdlib.h>
#include <string.h>

const int N = 4 * 1024 * 1024;

static int *h_in, *h_ref, *h_got;
static int *d_in, *d_out;
static SPTScratch<int, 128, 4> s_lt;
static SPTScratch<int, 128, 4> s_le;
static bool all_ok = true;

// CPU reference: nearest j < i with (le ? in[j] <= in[i] : in[j] < in[i]),
// else -1.  Classic monotonic stack, O(n).
static void cpu_pse(const int* in, int n, bool le, int* out) {
    int* stk = (int*) malloc((size_t)n * sizeof(int));
    int top = -1;
    for (int i = 0; i < n; i++) {
        while (top >= 0 &&
               !(le ? (in[stk[top]] <= in[i]) : (in[stk[top]] < in[i])))
            top--;
        out[i] = (top >= 0) ? stk[top] : -1;
        stk[++top] = i;
    }
    free(stk);
}

static void check(const char* label, int n, bool le) {
    gpuAssert(cudaMemcpy(d_in, h_in, (size_t)n * sizeof(int), cudaMemcpyHostToDevice));
    gpuAssert(cudaMemset(d_out, 0xAB, (size_t)n * sizeof(int)));
    if (le) runSPT<int, 128, 4, true >(d_in, d_out, n, s_le);
    else    runSPT<int, 128, 4, false>(d_in, d_out, n, s_lt);
    gpuAssert(cudaDeviceSynchronize());
    gpuAssert(cudaMemcpy(h_got, d_out, (size_t)n * sizeof(int), cudaMemcpyDeviceToHost));
    cpu_pse(h_in, n, le, h_ref);
    int bad = -1;
    for (int i = 0; i < n; i++)
        if (h_ref[i] != h_got[i]) { bad = i; break; }
    if (bad >= 0)
        printf("  [%-4s %-10s n=%9d] MISMATCH at %d: ref=%d got=%d (val=%d)\n",
               le ? "<=" : "<", label, n, bad, h_ref[bad], h_got[bad], h_in[bad]);
    else
        printf("  [%-4s %-10s n=%9d] PASS\n", le ? "<=" : "<", label, n);
    all_ok &= (bad < 0);
}

static void fill_and_check(const char* label) {
    // N, odd size with partial last block, sizes around one block (B=512), 1.
    const int sizes[6] = { N, 1000003, 513, 512, 511, 1 };
    for (int i = 0; i < 6; i++) {
        check(label, sizes[i], true);
        check(label, sizes[i], false);
    }
}

int main() {
    cudaDeviceProp prop;
    gpuAssert(cudaGetDeviceProperties(&prop, 0));
    printf("Device: %s  (SM %d.%d, %d SMs)\n\n", prop.name, prop.major, prop.minor,
           prop.multiProcessorCount);

    h_in  = (int*) malloc((size_t)N * sizeof(int));
    h_ref = (int*) malloc((size_t)N * sizeof(int));
    h_got = (int*) malloc((size_t)N * sizeof(int));
    gpuAssert(cudaMalloc(&d_in,  N * sizeof(int)));
    gpuAssert(cudaMalloc(&d_out, N * sizeof(int)));

    s_lt = allocSPTScratch<int, 128, 4, false>(N);
    s_le = allocSPTScratch<int, 128, 4, true>(N);
    printf("num_phys: strict=%d, incl=%d\n\n", s_lt.num_phys, s_le.num_phys);

    printf("=== Correctness (vs CPU stack reference) ===\n");

    srand(1234);
    for (int i = 0; i < N; i++) h_in[i] = rand();
    fill_and_check("random");

    srand(99);
    for (int i = 0; i < N; i++) h_in[i] = rand() % 16;
    fill_and_check("dup16");

    for (int i = 0; i < N; i++) h_in[i] = 42;
    fill_and_check("all-equal");

    for (int i = 0; i < N; i++) h_in[i] = N - i;
    fill_and_check("descending");

    for (int i = 0; i < N; i++) h_in[i] = i;
    fill_and_check("ascending");

    // INT_MAX stress: INF-valued queries exercise the lane<W ballot guard
    // in Phase 3 (under <=, lt(INF, INT_MAX) is true).
    srand(5);
    for (int i = 0; i < N; i++) h_in[i] = (rand() % 4 == 0) ? 2147483647 : rand();
    fill_and_check("intmax");

    // Depth-like input (what the parser feeds PSE): small +/-1 walk.
    srand(77);
    {
        int d = 0;
        for (int i = 0; i < N; i++) {
            d += (rand() % 2) ? 1 : -1;
            if (d < 0) d = 0;
            h_in[i] = d;
        }
    }
    fill_and_check("depths");

    cudaFree(d_in);
    cudaFree(d_out);
    freeSPTScratch<int, 128, 4>(s_lt);
    freeSPTScratch<int, 128, 4>(s_le);
    free(h_in); free(h_ref); free(h_got);

    printf("\n%s\n", all_ok ? "ALL PASS" : "FAILED");
    return all_ok ? 0 : 1;
}
