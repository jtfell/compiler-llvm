#include <stdio.h>
#include <unistd.h>

#include "./lib.h"

/*
 * GARBAGE COLLECTOR
 */
#define BLOCK_SIZE 1024

// Direct pointers to memory locations
void * mappingStartLocation;
void * mappingCurrentLocation;
void * allocStartLocation;
void * allocCurrentLocation;
void * allocEndLocation;

/*
 * Top level API
 */
bdescr * alloc_group(int n) {
  bdescr * blk;

  // Keep a reference to the first block we're allocating
  bdescr * firstBlk = (bdescr *) mappingCurrentLocation;
  bdescr * prevBlk = firstBlk;

  for (int i = 0; i < n; i++) {

    // Create a mapping entry
    blk = (bdescr *) mappingCurrentLocation;
    mappingCurrentLocation += sizeof(bdescr);

    blk->allocated = true;

    // Point to the next free block of unallocated space
    blk->start = allocCurrentLocation;
    allocCurrentLocation += BLOCK_SIZE;

    // If not the first block in the group, link the previous one to this block
    if (mappingCurrentLocation != prevBlk) {
      prevBlk->link = blk;
    } else {
      prevBlk->link = NULL;
    }

    prevBlk = blk;
  }

  return firstBlk;
}

void free_group(bdescr *p) {
}

void print_state() {
  printf("=======================================================\n");
  printf("Allocation state:\n");
  printf("       start: %d\n", (int) allocStartLocation);
  printf("         end: %d\n", (int) allocEndLocation);
  
  printf("mappingStart: %d\n", (int) mappingStartLocation);
  printf(" mappingCurr: %d\n", (int) mappingCurrentLocation);

  printf("allocated blocks:\n");
  for (void * i = mappingStartLocation; i < mappingCurrentLocation; i += sizeof(bdescr)) {
    print_block(i);
  }
  printf("=======================================================\n\n");
}

void print_block(bdescr * blk) {
  printf(
      "start: %d, allocated: %d, link: %d\n",
      (int) blk->start,
      blk->allocated,
      (int) blk->link
  );
}

void gc_pool_init() {

  // Allocate space for management struct
  mappingStartLocation = sbrk(0);
  sbrk(50 * sizeof(bdescr));

  // Get the start address
  allocStartLocation = sbrk(0);

  // Allocate a chunk of memory
  sbrk(50 * BLOCK_SIZE);

  // Get the final address
  allocEndLocation = sbrk(0);

  printf(
      "allocated %d memory (%d -> %d)\n ", 
      (int) (allocEndLocation - allocStartLocation),
      (int) allocStartLocation,
      (int) allocEndLocation
  );

  mappingCurrentLocation = mappingStartLocation;
  allocCurrentLocation = allocStartLocation;
}
