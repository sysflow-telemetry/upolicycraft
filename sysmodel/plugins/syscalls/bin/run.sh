#!/usr/bin/env bash

if [ "$#" -ne 2 ]; then
  echo "usage: $0 binary entrypoint"
  exit 1
fi

PROGRAM=$1
ENTRYPOINT=$2

# Limit-max-length the total number of basic blocks Primus will run

bap $PROGRAM --run \
       --run-entry-points="${ENTRYPOINT}" \
       --primus-limit-max-length=8192 \
       --verbose \
       --primus-promiscuous-mode \
       --primus-greedy-scheduler \
       --primus-syscall-identify
