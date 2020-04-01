#!/usr/bin/env bash

if [ "$#" -ne 2 ]; then
  echo "usage: $0 binary entrypoint"
  exit 1
fi

PROGRAM=$1
ENTRYPOINT=$2

# Limit-max-length the total number of basic blocks Primus will run

bap $PROGRAM -prun \
       --run-entry-points="${ENTRYPOINT}" \
       --run-argv="helloworld,/bin/sh,/bin/bash" \
       --primus-limit-max-length=2048 \
       --primus-promiscuous-mode \
       --primus-greedy-scheduler \
       --primus-uids-model \
       --report-progress
