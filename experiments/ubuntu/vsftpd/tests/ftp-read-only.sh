#!/usr/bin/env bash

ftp -niv $1 21 <<EOT
user anonymous
rhelp
site
chmod
pwd
ls -la
ascii
get README.md
size README.md
binary
cd uploads
ascii
nlist
cd extra
passive
get giphy.gif
cdup
pwd
get .dangerous
get .private
get .private
quit
EOT

