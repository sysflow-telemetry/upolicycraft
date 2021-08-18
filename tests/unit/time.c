#include <stdbool.h>
#include <stdio.h>
#include <time.h>

//  int tm_min;    /* Minutes (0-59) */
//  int tm_hour;   /* Hours (0-23) */
//  int tm_mday;   /* Day of the month (1-31) */
//  int tm_mon;    /* Month (0-11) */
//  int tm_year;   /* Year - 1900 */
//  int tm_wday;   /* Day of the week (0-6, Sunday = 0) */
//  int tm_yday;   /* Day in the year (0-365, 1 Jan = 0) */
//  int tm_isdst;  /* Daylight saving time */

int main(int argc, char *argv[]) {
    struct tm res;
    bool fail = false;    

    printf("Sizeof struct tm = %d\n", sizeof(res));

    localtime_r(0, &res);
    if (res.tm_sec != 0) {
        puts("Could not reset second!\n");
        fail = true;
    }

    if (res.tm_min != 0) {
        puts("Could not reset minute!\n");
        fail = true;
    }

    if (res.tm_hour != 0) {
        puts("Could not reset hour!\n");
        fail = true;
    }

    if (res.tm_mday != 0) {
        puts("Could not reset day!\n");
        fail = true;
    }

    if (res.tm_mon != 0) {
        puts("Could not reset mon!\n");
        fail = true;
    }

    if (res.tm_year != 0) {
        puts("Could not reset year!\n");
        fail = true;
    }

    if (res.tm_wday != 0) {
        puts("Could not reset week day!\n");
        fail = true;
    }

    if (res.tm_yday != 0) {
        puts("Could not reset yday!\n");
        fail = true;
    }

    if (res.tm_isdst != 0) {
        puts("Could not reset daylight savings!\n");
        fail = true;
    }

    if (!fail) {
        puts("All tests passed!\n");
    }
 
}
