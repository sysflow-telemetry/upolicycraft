#include <stdlib.h> 
#include <stdio.h>

int main(int argc, char *argv[]) {
  void *p;
  void *q = p;
  uids_log("Input");
  uids_debug(p);
  int res = posix_memalign(&p, 32, 128);
  if (res != 0) {
     puts("Invalid error!\n");
  }
  if (q == p) {
     puts("The value of is unchanged!\n");
  }
  uids_log("Output");
  uids_debug(p);
}
