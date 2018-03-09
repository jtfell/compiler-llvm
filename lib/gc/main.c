#include "./lib.h"

int main(int argc, char *argv[]) {
  gc_pool_init();

  bdescr * blockA = alloc_group(5);
  print_state();
  bdescr * blockB = alloc_group(11);
  print_state();
  free_group(blockA);
  print_state();
  free_group(blockB);
  print_state();
}
