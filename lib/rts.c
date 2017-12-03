#include <stdio.h>

int print_int(int a) {
  // Simulated failure case
  if (a > 10) {
    return 1;
  }

  // Use the IO function provided by the platform
  printf("%d", a);
  return 0;
}
