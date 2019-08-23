#!/usr/bin/env bash

# deprecated: for reference

# --report-progress

bap $1 --run \
       --run-entry-points="myputs" \
       --primus-limit-max-length=8192 \
       --primus-syscall-observations all \
       --dump=bir:result.out
