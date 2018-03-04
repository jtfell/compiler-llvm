#include <stdio.h>
#include <unistd.h>

/*
 * GARBAGE COLLECTOR
 */
typedef enum { false, true } bool;
typedef struct bdescr_ {

    // The memory location of the block
    void *               start;

    // The next block in the group (can be null)
    struct bdescr_ *     link;

    // Indicate if this block has been allocated
    bool                 allocated;
} bdescr;

/*
 * Top level API
 */
bdescr * alloc_group(int n);

void free_group(bdescr *p);
void print_state();
void print_block(bdescr *blk);

void gc_pool_init();
