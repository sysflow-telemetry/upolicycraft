#!/usr/bin/env bash

PROGRAM=$1

llvm-profdata-3.8 merge ${PROGRAM}.profraw -o ${PROGRAM}.profdata
llvm-cov-3.8 report ./vsftpd -instr-profile=${PROGRAM}.profdata

