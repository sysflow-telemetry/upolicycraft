#!/usr/bin/env bash

# Run from the uIDS sub-directory.

# /etc/vsftpd.conf:/uIDS/tests/vsftpd/fs/etc/vsftpd.conf,/dev/null:/uIDS/tests/vsftpd/fs/dev/null

./bin/uIDS /uIDS/tests/vsftpd/vsftpd _start '/bin/vsftpd' /bin/bash ./vsftpd.sh 1000000000 -r -v -redirects "srv:/tmp/server.log" -fs "/uIDS/tests/vsftpd/fs" -t /uIDS/tests/vsftpd/testcases/
