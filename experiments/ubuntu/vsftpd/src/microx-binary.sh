#!/usr/bin/env bash

sed -i 's/gcc/clang/' Makefile

make

./optimize.sh

sed -i 's/clang/gcc/' Makefile
sed -i 's/now/now -L \/root\/uids -l:uids.so/' Makefile

make

