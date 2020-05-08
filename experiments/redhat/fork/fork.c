#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
  char *argv1[] = {argv[2], NULL};

  pid_t pid;

  if ((pid = fork()) == 0) {
    // In the child.
    execv(argv[1], argv1);
  } else {
    // In the parent
    sleep(1);
    printf("Done waiting!\n");
  }

}
