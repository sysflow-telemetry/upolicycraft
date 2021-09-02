#!/usr/bin/env bash

# -mllvm -lowerswitch

export CC="gcc"
export CFLAGS="-fno-stack-protector -emit-llvm"

./configure --without-http_rewrite_module --without-http_gzip_module
