#!/usr/bin/env bash

CFLAGS="-fno-stack-protector -emit-llvm" CC=clang make

./optimize.sh

sed -i 's/clang/gcc/' Makefile
sed -i 's/now/now -L \/root\/uids -l:uids.so/' Makefile

make

