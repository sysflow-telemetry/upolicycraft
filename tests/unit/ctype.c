#include <ctype.h>
#include <stdio.h>
#include <stdint.h>

// extern unsigned short **__ctype_b_loc();

extern void uids_log(char *s);
extern void uids_debug(void *p);

void check_ctype() {
    puts("Check ctype properties!");

    if (isalnum(0x61)) {
       puts("Success!");
    } else {
       puts("Failure!");
    }

    if (isalnum(0x20)) {
       puts("Failure!");
    } else {
       puts("Success!");
    }

    if (isalpha(0x20)) {
       puts("isalpha(' ') Failure!");
    } else {
       puts("Success!");
    }

    if (isdigit(0x20)) {
       puts("isdigit(' ') Failure!");
    } else {
       puts("Success!");
    }

    if (isspace(0x20)) {
       puts("Success!");
    } else {
       puts("Failure!");
    }

    if (isprint(0x1e)) {
      puts("Failure!");
    } else {
      puts("Success!");
    }

    if (isprint(0x61)) {
      puts("Success!");
    } else {
      puts("Failure!");
    }

    if (tolower(0x41) == 0x61) {
      puts("Success!");
    } else {
      puts("Failure! tolower(A) != a");
    }

}

int main(int argc, char *argv[]) {
    /**
    if (isalpha(0x28)) {
        puts("Success!");
    } else {
        puts("Failure!");
    } */
    unsigned short int **p = __ctype_b_loc();
    unsigned short int *q = *p;
    uids_log("__ctype_b_loc");
    uids_debug(p);
    uids_debug(q);
    uids_debug(&q[0x61]);
    uids_debug(q[0x61]);

    /*
    printf("0x%x\n", q[0x61]);

    if (isalpha(0x47)) {
       puts("Success!");
    } else {
       puts("Failure!");
       exit(1);
    }

    if (isalpha(0x60)) {
       puts("Failure!");
       exit(1);
    } else {
       puts("Success!");
    }

    if (isalpha(0x61)) {
       puts("Success!");
    } else {
       puts("Failure!");
    } */

    /*
    check_ctype();

    if (toupper('y') == 'Y') {
       puts("Success!");
    } else {
       puts("Failure!");
    }
    */

    /**
    int32_t **tl = __ctype_tolower_loc();
    uids_log("Checking ctypetolower_loc:");
    uids_debug(&tl[0x41]);
    uids_debug(tl[0x41]);
    */
}
