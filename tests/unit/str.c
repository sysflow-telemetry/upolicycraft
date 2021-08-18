#include <string.h>
#include <stdio.h>

int main(int argc, char *argv[]) {
  char buf[16];
  bzero(buf, 16);
  strcpy(buf, argv[1]);
  char *s = strstr(argv[1], "$document_root");
  if (s) {
    puts("Test failed!");
  } else {
    puts("Test passed!");
  }

  s = strstr(argv[2], "$document_root");
  if (s == &argv[2][1]) {
    puts("Test passed!");
  } else {
    puts("Test failed!");
  }

}
