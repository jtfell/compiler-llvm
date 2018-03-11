#include <stdio.h>
#include <unistd.h>
#include <execinfo.h>

#include "./lib.h"

#define BLOCK_SIZE 1024
#define INTS_PER_BLOCK (BLOCK_SIZE / sizeof(int))

void * arrayMap;

void print_stack() {
  void *array[10];
  size_t size;

  size = backtrace(array, 10);
  char ** strings = backtrace_symbols(array, size);

  printf("Obtained %zd stack frames.\n", size);

  for (size_t i = 0; i < size; i++) {
    printf("%s - ", strings[i]);
    printf("%d\n", array[i]);
  }

  free(strings);
}

void gc_init() {
  arrayMap = sbrk(0);
  sbrk(50 * sizeof(array));

  gc_pool_init();
  print_stack();
}

void gc_run() {
  printf("stack pointer: %d\n", __builtin_frame_address(0));
  print_stack();
}

array * alloc_array(int length) {
 
  // Calculate how many blocks are required for this array
  size_t blksRequired = ((length * sizeof(int)) / BLOCK_SIZE) + 1;

  // Allocate the memory through the low-level API
  bdescr * group = alloc_group(blksRequired);

  // Find the first non-allocated array in the global arrayMap
  array * arrP;
  for (void * p = arrayMap; p < arrayMap + (50 * sizeof(array)); p += sizeof(array)) {
    arrP = (array *) p;

    if (arrP->data == NULL) {
      arrP->data = group;
      arrP->length = length;
      break;
    }

    printf("Out of memory!!!\n");
  }

  return arrP;
}

void print_array_data(array * arr) {
  bdescr * currentBlk = arr->data;

  for (int i = 0; i < arr->length; i++) {

    int indexInBlk = i % INTS_PER_BLOCK;
    
    int * intBuffer = (int *) currentBlk->start;

    // When at the end of a block, follow the link and start using the next point
    // printf("%d, %d\n", indexInBlk, INTS_PER_BLOCK);
    if (indexInBlk == INTS_PER_BLOCK - 1) {
      printf("%d: %d @ (%d)\n", i, intBuffer[i], &(intBuffer[i]));

      currentBlk = currentBlk->link;
      if (currentBlk == NULL) {
        printf("Allocated invalid blk!!!\n");
      }
    }
  }
}

void set_array_data(array * arr, int * elems) {
  bdescr * currentBlk = arr->data;

  for (int i = 0; i < arr->length; i++) {

    int indexInBlk = i % INTS_PER_BLOCK;
    
    int * intBuffer = (int *) currentBlk->start;
    intBuffer[i] = elems[i];

    // When at the end of a block, follow the link and start using the next point
    if (indexInBlk == INTS_PER_BLOCK - 1) {
      currentBlk = currentBlk->link;
      if (currentBlk == NULL) {
        printf("Allocated invalid blk!!!\n");
      }
    }
  }

}
