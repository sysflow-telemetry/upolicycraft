nullhttpd
=========

A fork of nullhttpd to evaluate uIDS.

How to build with coverage tracking.

    LDFLAGS=-fprofile-instr-generate -fcoverage-mapping
    CFLAGS=-fprofile-instr-generate -fcoverage-mapping
    CC=clang


