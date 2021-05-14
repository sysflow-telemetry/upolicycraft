#!/usr/bin/env bash

# Run from the uIDS sub-directory

./bin/uIDS /uIDS/tests/nginx/nginx _start '/bin/nginx' /bin/bash ./nginx.sh 100000000  -fs "/usr/local/nginx/logs/nginx.pid:/uIDS/tests/nginx/fs/usr/local/nginx/logs/nginx.pid,/usr/local/nginx/logs/access.log:/uIDS/tests/nginx/fs/usr/local/nginx/logs/access.log,/usr/local/nginx/conf/mime.types:/uIDS/tests/nginx/fs/usr/local/nginx/conf/mime.types,/usr/local/nginx/conf/nginx.conf:/uIDS/tests/nginx/fs/usr/local/nginx/conf/nginx.conf,/usr/local/nginx/logs/error.log:/uIDS/tests/nginx/fs/usr/local/nginx/logs/error.log,srv:/tmp/srv.log" -r -v
