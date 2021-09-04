#!/usr/bin/env bash

./bin/uIDS /uIDS/tests/redis/redis-server _start '/bin/redis' /bin/bash ./redis.sh 1000000000 -r -v -redirects "srv:/tmp/server.log" -fs "/uIDS/tests/redis/fs" -t /uIDS/tests/redis/testcases/ 
