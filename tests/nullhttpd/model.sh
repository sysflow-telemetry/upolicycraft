#!/usr/bin/env bash

# Run from the uIDS sub-directory

./bin/uIDS /uIDS/tests/nullhttpd/nullhttpd _start '/bin/nullhttpd,-h,127.0.0.1,-p,80,-b,/var/www/html,-i,/var/www/html/index.html,-r,/var/www/html/404.html,-d,/var/www/nullhttpd.pid,-u,www-data,-g,www-data' /bin/bash ./nullhttpd.sh 10000000 -fs "/var/www/html/index.html:/uIDS/tests/nullhttpd/fs/html/index.html,/var/www/html//index.html:/uIDS/tests/nullhttpd/fs/html/index.html,/var/www/html/index.html:/uIDS/tests/nullhttpd/fs/html/index.html,/var/www/html//giphy.gif:/uIDS/tests/nullhttpd/fs/html/giphy.gif,/var/www/html//docs/old/test.html:/uIDS/tests/nullhttpd/fs/html/docs/old/test.html,/var/www/html/404.html:/uIDS/tests/nullhttpd/fs/html/404.html,/var/www/nullhttpd.pid:/uIDS/tests/nullhttpd/fs/nullhttpd.pid,srv:/tmp/srv.log" -t $PWD/testcases/ -r 
