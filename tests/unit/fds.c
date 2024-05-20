#include <errno.h>
#include <stdio.h>
#include <stddef.h>
/* According to POSIX.1-2001, POSIX.1-2008 */
#include <sys/select.h>

/* According to earlier standards */
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <dirent.h>

extern uids_log(char *);
extern uids_debug(void *);

void test_fstat64() {
    FILE *fp = fopen("/root/test.log", "r");
    uids_log(fp);
    uids_debug(fp);
    struct stat statbuf;

    fstat64(fileno(fp), &statbuf);

    uids_debug(statbuf.st_size);

    printf("Size: %d %d\n", statbuf.st_size, sizeof(statbuf.st_size));
    printf("Mode: %x\n", statbuf.st_mode & 0100000);

    // 0100000
    // 0040000
    uids_log("Checking mode");

    uids_debug(statbuf.st_mode);

    if (!S_ISREG(statbuf.st_mode)) {
      puts("Could not confirm file is a regular file!");
      return 1;
    }

    if (statbuf.st_uid != 0) {
      puts("User id is not set properly!");
      return 1;
    }

    printf("Uid: %x\n", offsetof(struct stat, st_uid));
    printf("NLink: %x\n", offsetof(struct stat, st_nlink));
}

void test_stat64() {
    struct stat statbuf;

    stat64("/root/test.log", &statbuf);

    uids_debug(statbuf.st_size);

    printf("Size: %d %d\n", statbuf.st_size, sizeof(statbuf.st_size));
    printf("Mode %x\n", statbuf.st_mode & 0100000);
    // 0100000
    // 0040000
    uids_log("Checking mode");

    uids_debug(statbuf.st_mode);

    if (!S_ISREG(statbuf.st_mode)) {
      puts("Could not confirm file is a regular file!");
      return 1;
    }

    if (statbuf.st_uid != 0) {
      puts("User id is not set properly!");
      return 1;
    }
}

int main(int argc, char *argv[]) {
  fd_set fds;
  int sockets[2];
  
  FD_ZERO(&fds);
  FD_SET(2, &fds);

  test_fstat64();
  test_stat64();
 
  puts("Tests passed!");

  printf("O_CREAT = %x\n", O_CREAT);

  printf("ENOENT = %x\n", ENOENT);

  printf("d_name: %x\n", offsetof(struct dirent, d_name));
        //socketpair(AF_UNIX, SOCK_STREAM, 0, sockets);

        //close(sockets[0]);
        //close(sockets[1]);
}
