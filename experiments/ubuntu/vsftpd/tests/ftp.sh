#!/usr/bin/env bash 

ftp -niv 127.0.0.1 21 <<EOT
user anonymous
rhelp
site
chmod
pwd
ls -la
ascii
get README.md
append README.md
README.md
binary
cd uploads
ls
mkdir extra
cd extra
sunique
send giphy.gif
sunique
mkdir extra1
cd extra1
mkdir extra2
cd extra2
passive
send lorem.txt
rename lorem.txt lorem1.txt
append lorem1.txt
lorem.txt
nlist
cdup
cdup
cdup
ls -R
cd extra
cd extra1
cd extra2
delete lorem1.txt
cdup
rmdir extra2
cdup
rmdir extra1
size giphy.gif
delete giphy.gif
cdup
rmdir extra
get secret-code.c
get .private
get .dangerous
restart
status
site
CHMOD
quit
EOT
