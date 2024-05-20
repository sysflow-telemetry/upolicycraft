#include <pwd.h>
#include <stddef.h>
#include <stdio.h>

int main(int argc, char *argv[]) {
  printf("offsetof(struct passwd, pw_dir): %lu\n", offsetof(struct passwd, pw_dir));
}
