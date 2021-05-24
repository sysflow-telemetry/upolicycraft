#!/usr/bin/env bash

# Run from the uIDS sub-directory.

./bin/uIDS /uIDS/tests/vsftpd/vsftpd _start '/bin/vsftpd' /bin/bash ./vsftpd.sh 1000000000 -r -v -fs "srv:/tmp/server.log,/etc/vsftpd.conf:/uIDS/tests/vsftpd/fs/etc/vsftpd.conf"
