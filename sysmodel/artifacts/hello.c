#include <stdio.h>
#include <stdlib.h>

#include "mylibc.h"

void hello_world() {
    myputs("Hello World!\n");
}

int main(int argc, char *argv[]) {
    hello_world();
    myexit(1);
}
