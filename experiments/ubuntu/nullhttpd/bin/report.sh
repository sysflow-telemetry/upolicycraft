#!/usr/bin/env bash

# Do I need to delete nullhttpd.profdata?

llvm-profdata-3.8 merge nullhttpd.profraw -o nullhttpd.profdata
llvm-cov-3.8 report ./nullhttpd -instr-profile=nullhttpd.profdata

