#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

void random_letter() {
  int i = rand() % 60;
  if (i < 25) {
    putchar('a' + i);
  } else if (i < 50) {
    putchar('A' + i - 25);
  } else {
    putchar('0' + i - 50);
  }
}

void random_whitespace() {
  const char whitespace[4] = {'\t', '\n', ' ', '\r'};
  putchar(whitespace[rand()%sizeof(whitespace)]);
}

int random_token() {
  int i = rand() % 4;
  switch (i) {
  case 0:
    {
      int count = 3 + rand() % 17;
      for (int j = 0; j < count; j++) { random_letter(); }
      return count;
    }
    break;
  case 1:
    {
      int count = 3 + rand() % 6;
      for (int j = 0; j < count; j++) { random_whitespace(); }
      return count;
    }
  case 2:
    putchar('(');
    break;
  case 3:
    putchar(')');
    break;
  }
  return 0;
}

int main(int argc, char *argv[]) {
  assert(argc == 2);
  int max_size = 0;
  sscanf(argv[1], "%d", &max_size);
  assert(0 <= max_size);
  int curr_size = 0;
  while (curr_size < max_size) {
    curr_size += random_token();
  }
}
