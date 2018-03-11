#include <stdio.h>
#include <unistd.h>

#include "./pool.h"

typedef struct array_ {

    // The length of the array
    int               length;

    // Pointer to the underlying memory
    struct bdescr_ *     data;
} array;

/*
 * Top level API
 */
void gc_init();
void gc_run();

array * alloc_array(int length);
void print_array_data(array * arr);
void set_array_data(array * arr, int * elems);
