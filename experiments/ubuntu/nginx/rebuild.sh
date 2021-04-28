#!/usr/bin/env bash

./uids-config.sh

sed -i 's/CC =\tgcc/CC = clang/' objs/Makefile

make

./optimize.sh

sed -i 's/CC = clang/CC = gcc/' objs/Makefile
sed -i 's/lz/lz -L /root/uids -l:uids.so ' objs/Makefile

make

