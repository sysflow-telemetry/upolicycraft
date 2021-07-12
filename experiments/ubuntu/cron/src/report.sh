#!/usr/bin/env bash

VERB=$1
PROGRAM=$2

llvm-profdata-3.8 merge ${PROGRAM}.profraw -o ${PROGRAM}.profdata

# report
# show

llvm-cov-3.8 ${VERB} ./vsftpd -instr-profile=${PROGRAM}.profdata
