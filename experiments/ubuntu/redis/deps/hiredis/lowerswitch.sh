#!/usr/bin/env bash

printf "Optimizing %s\n" $1

/usr/lib/llvm-3.8/bin/opt -lowerswitch < $1 > /tmp/output.o
llc-3.8 /tmp/output.o -o /tmp/output.s
as /tmp/output.s -o $1
