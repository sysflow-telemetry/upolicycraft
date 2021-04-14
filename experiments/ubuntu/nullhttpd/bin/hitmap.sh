#!/usr/bin/env bash

llvm-cov-3.8 show ./nullhttpd -instr-profile=nullhttpd.profdata 
