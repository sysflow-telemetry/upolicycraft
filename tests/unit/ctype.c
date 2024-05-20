#include <ctype.h>
#include <stdio.h>
#include <stdint.h>

// extern unsigned short **__ctype_b_loc();

extern void uids_log(char *s);
extern void uids_debug(void *p);

void check_ctype() {
    puts("Check ctype properties!");

    puts("Check isalnum");
    if (isalnum(0x61)) {
       puts("Success!");
    } else {
       puts("Failure isalnum(0x61)!");
    }

    puts("Check bad isalnum");
    if (isalnum(0x20)) {
       puts("Failure isalnum(0x20)!");
    } else {
       puts("Success!");
    }

    if (isalpha(0x20)) {
       puts("Failure isalpha(' ')!");
    } else {
       puts("Success!");
    }

    if (isdigit(0x20)) {
       puts("Failure isdigit(' ')!");
    } else {
       puts("Success!");
    }

    if (isspace(0x20)) {
       puts("Success!");
    } else {
       puts("Failure isspace(' ')!");
    }

    if (isprint(0x1e)) {
      puts("Failure (RS)!");
    } else {
      puts("Success!");
    }

    if (isprint(0x61)) {
      puts("Success!");
    } else {
      puts("Failure isprint('a')!");
    }

    if (tolower(0x41) == 0x61) {
      puts("Success!");
    } else {
      puts("Failure tolower(A) != a!");
    }

    uids_log("After tolower!");

    return;

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

    uids_debug(&q[0x20]);
    uids_debug(q[0x20]);

    printf("0x%x\n", q[0x61]);

    if (isalpha(0x47)) {
      puts("Success!");
    } else {
      puts("Failure isalpha('G')!");
    }

    if (isalpha(0x60)) {
      puts("Failure is alpha('`')!");
    } else {
      puts("Success!");
    }

    if (isalpha(0x61)) {
      puts("Success!");
    } else {
      puts("Failure is alpha('a')!");
    }

    if (toupper('y') == 'Y') {
       puts("Success!");
    } else {
       puts("Failure!");
    }

    check_ctype();

    /*
    int32_t **tl = __ctype_tolower_loc();
    uids_log("Checking ctypetolower_loc:");
    uids_debug(&tl[0x41]);
    uids_debug(tl[0x41]); */
}
