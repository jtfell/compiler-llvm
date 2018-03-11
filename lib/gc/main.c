#include "./lib.h"

void do_alloc() {
  array * arr1 = alloc_array(5);

  int intArr[] = {1, 2, 3, 4, 5};
  set_array_data(arr1, (int *) intArr);
  print_array_data(arr1);

  print_state();
}

int main(int argc, char *argv[]) {
  printf("Test pool.c\n");
  gc_pool_init();

  bdescr * blockA = alloc_group(5);
  print_state();
  bdescr * blockB = alloc_group(11);
  print_state();
  free_group(blockA);
  print_state();
  free_group(blockB);
  print_state();

  printf("Test lib.c\n");

  gc_init();

  do_alloc();

  gc_run();
}

