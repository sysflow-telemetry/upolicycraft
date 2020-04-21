#!/usr/bin/env bash

if [ "$#" -ne 3 ]; then
  echo "usage: $0 binary entrypoint argv"
  exit 1
fi

PROGRAM=$1
ENTRYPOINT=$2
ARGV=$3

# Limit-max-length the total number of basic blocks Primus will run

# Useful options for tracing the execution of the Primus machine.
# --primus-print-obs=exception,pc-changed,jumping,call,call-return,machine-switch,machine-fork,lisp-message,incident,incident-location \
# --primus-print-output=primus.log \
# --cache-size=1024 \

bap $PROGRAM -prun \
       --run-entry-points="${ENTRYPOINT}" \
       --run-argv=${ARGV} \
       --primus-limit-max-length=2048 \
       --primus-promiscuous-mode \
       --primus-greedy-scheduler \
       --primus-uids-model \
       --report-progress
