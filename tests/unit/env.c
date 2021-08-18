#include <stdlib.h>
#include <stdio.h>

extern char **environ;

int main(int argc, char *argv[]) { 
  if (environ == NULL) {
     puts("Could not detect environ!\n");
  }
  puts(getenv("PATH"));
  puts(getenv("PWD"));
  if (getenv("NONEXISTENT")) {
    puts("Obtained a pointer to a non-existent environment variable.\n");
  } else {
    puts("All tests passed!");
  }
}
