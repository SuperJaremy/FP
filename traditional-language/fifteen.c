#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

uint64_t count_paths(size_t matrix_side) {
  size_t i;
  int side_dots = matrix_side + 1;
  int last_dot = side_dots * side_dots - 1;
  uint64_t *paths = malloc(sizeof(uint64_t) * (last_dot + 1));
  for (i = 0; i < last_dot + 1; i++) {
    if (i == 0 || i < side_dots || i % side_dots == 0) {
      paths[i] = 1;
    } else {
      paths[i] = paths[i - 1] + paths[i - side_dots];
    }
  }
  return paths[last_dot];
}

int main() {
  size_t matrix_side = 20;
  printf("%" PRIu64 "\n", count_paths(matrix_side));
  return 0;
}
