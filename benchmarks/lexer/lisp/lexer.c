#include <ctype.h>
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include<sys/time.h>

// Read the rest of an open file into a NUL-terminated string; returns
// NULL on error.
static void* fslurp_file(FILE *f, size_t *size) {
  long start = ftell(f);
  fseek(f, 0, SEEK_END);
  long src_size = ftell(f)-start;
  fseek(f, start, SEEK_SET);
  unsigned char *s = (unsigned char*) malloc((size_t)src_size + 1);
  if (fread(s, 1, (size_t)src_size, f) != (size_t)src_size) {
    free(s);
    s = NULL;
  } else {
    s[src_size] = '\0';
  }

  if (size) {
    *size = (size_t)src_size;
  }

  return s;
}

static void* slurp_file(const char *filename, size_t *size) {
  FILE *f = fopen(filename, "rb"); // To avoid Windows messing with linebreaks.
  if (f == NULL) return NULL;
  unsigned char *s = fslurp_file(f, size);
  fclose(f);
  return s;
}

int main(int argc, char** argv) {
  assert(argc==2);
  size_t n;
  unsigned char* buf = slurp_file(argv[1], &n);
  assert(buf != NULL);

  size_t i = 0, count = 0;

  while (isspace(buf[i]) && i < n) {
    i++;
  }
  struct timeval start, end;

  gettimeofday(&start, NULL);
  while (i < n) {
    int c = buf[i++];
    if (c == ')') {
      count++;
    }
    else if (c == '(') {
      count++;
    }
    else if (isspace(c)) {
      continue;
    } else if (isalnum(c)) {
      while (isalnum(buf[i]) && i < n) {
        i++;
      }
      count++;
    } else if (c == EOF) {
      break;
    } else {
      fprintf(stderr, "Bad character: %d\n", buf[i]);
      return 1;
    }
  }
  gettimeofday(&end, NULL);
  printf("#token: %d\n", (int)count);
  long elapsed = ((end.tv_sec - start.tv_sec) * 1000000) + (end.tv_usec - start.tv_usec);
  printf("Time elapsed: %ldμs\n", elapsed);
  return 0;
}
