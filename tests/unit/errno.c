#include <errno.h>
#include <stdio.h>

extern uids_log(char *);
extern uids_debug(void *);

extern int * __errno_location();

int main(int argc, char *argv[]) {
    int *errnoloc = __errno_location();
    uids_debug(errnoloc);
    int status = errno;
    if (status == 0) {
       puts("Test passed!\n"); 
    }

}
