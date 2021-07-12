#!/usr/bin/env bash

CC=clang CFLAGS="-fno-stack-protector -emit-llvm" make cron

./optimize.sh

make cron
