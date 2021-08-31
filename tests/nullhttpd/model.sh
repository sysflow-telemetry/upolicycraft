#!/usr/bin/env bash

# Run from the uIDS sub-directory

touch /tmp/srv.log

./entrypoint/uIDS /uIDS/tests/nullhttpd/nullhttpd _start '/bin/nullhttpd,-h,127.0.0.1,-p,80,-b,/var/www/html,-i,/var/www/html/index.html,-r,/var/www/html/404.html,-d,/var/www/nullhttpd.pid,-u,www-data,-g,www-data' /bin/bash ./nullhttpd.sh 10000000 -redirects "srv:/tmp/srv.log" -fs "/uIDS/tests/nullhttpd/fs" -t /uIDS/tests/nullhttpd/testcases/ -r
