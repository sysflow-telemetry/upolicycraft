#!/usr/bin/env bash

# A helpful utility for finding the offset of a specific field 
# in a struct.

STRUCT=$1
FIELD=$2

if [ "$#" -ne 2 ]; then
  echo "usage: $0 struct field"
  exit 1
fi

cat << HEREDOC > offset.c
#include <pwd.h>
#include <stddef.h>
#include <stdio.h>

int main(int argc, char *argv[]) {
  printf("offsetof($STRUCT, $FIELD): %lu\n", offsetof($STRUCT, $FIELD));
}
HEREDOC

gcc -o offset offset.c
./offset
