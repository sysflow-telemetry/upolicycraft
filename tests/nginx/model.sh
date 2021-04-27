#!/usr/bin/env bash

# Run from the uIDS sub-directory

./bin/uIDS /uIDS/tests/nginx/nginx _start '/bin/nginx' /bin/bash ./nginx.sh 10000000 -r -v
