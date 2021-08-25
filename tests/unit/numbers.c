#include <stdlib.h>
#include <stdio.h>

#include <assert.h>

int main(int argc, char *argv[]) {
  int x = atoi("10");

  if (x != 10) {
    puts("atoi broken!");
  }

  long s = strtol("80", NULL, 10);

  if (s != 80) {
    puts("strtol broken!");
  }

   puts("All systems go!\n");
}
