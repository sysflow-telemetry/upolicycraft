#include <stdio.h>

int main(int argc, char *argv[]) {
    char buf[256];
    char input[256];
    char input1[256];
    fgets(input, 256, stdin);
    fgets(input1, 256, stdin);

    uids_log(input);
    uids_log(input1);

    snprintf(buf, 256, "%s%s", input, input1);
    puts(buf);
}
