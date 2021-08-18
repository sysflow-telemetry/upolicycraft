#include <stdlib.h>
#include <stdio.h>

#include <assert.h>

void itoa1(char *buf, int n);

int main(int argc, char *argv[]) {
   int x = atoi("10");
   assert(x == 10);

   long s = strtol("80", NULL, 10);
   assert(s == 80);

   /**

   char buf[16] = "net00";

   // itoa1(buf+3, 10);

   printf("%s\n", buf);

   if (buf[3] != '1' && buf[4] != '0') {
      puts("ERROR!");
   } */

   puts("All systems go!\n");
}
