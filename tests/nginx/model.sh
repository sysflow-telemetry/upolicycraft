#!/usr/bin/env bash

# Run from the uIDS sub-directory

./bin/uIDS /uIDS/tests/nginx/nginx _start '/bin/nginx' /bin/bash ./nginx.sh 1000000000 -r -v -fs "/uIDS/tests/nginx/fs" -redirects "srv:/tmp/server.log" -t /uIDS/tests/nginx/testcases/
