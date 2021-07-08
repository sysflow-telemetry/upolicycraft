#!/usr/bin/env bash

# Run from the uIDS sub-directory.

./bin/uIDS /uIDS/tests/cron/cron _start '/bin/cron' /bin/bash ./cron.sh 1000000000 -r -v -fs "/uIDS/tests/cron/fs"
