root@af7d7347d779:~/vsftpd-3.0.3# ftp -n 127.0.0.1 21
Connected to 127.0.0.1.
220 (vsFTPd 3.0.3)
ftp> user anonymous
331 Please specify the password.
Password:
230 Login successful.
Remote system type is UNIX.
Using binary mode to transfer files.
ftp> ?
Commands may be abbreviated.  Commands are:

!               dir             mdelete         qc              site
$               disconnect      mdir            sendport        size
account         exit            mget            put             status
append          form            mkdir           pwd             struct
ascii           get             mls             quit            system
bell            glob            mode            quote           sunique
binary          hash            modtime         recv            tenex
bye             help            mput            reget           tick
case            idle            newer           rstatus         trace
cd              image           nmap            rhelp           type
cdup            ipany           nlist           rename          user
chmod           ipv4            ntrans          reset           umask
close           ipv6            open            restart         verbose
cr              lcd             prompt          rmdir           ?
delete          ls              passive         runique
debug           macdef          proxy           send
ftp> account
Account:
502 ACCT not implemented.
ftp> ascii
200 Switching to ASCII mode.
ftp> bell
Bell mode on.
ftp> bell
Bell mode off.
ftp> binary
200 Switching to Binary mode.
ftp> binary
200 Switching to Binary mode.
ftp> case
Case mapping on.
ftp> case
Case mapping off.
ftp> cd uploads
250 Directory successfully changed.
ftp> ls
200 PORT command successful. Consider using PASV.
150 Here comes the directory listing.
drwx------    2 1000     1000         4096 Jun 07 05:53 extra
-rw-rw-rw-    1 1000     1000            0 May 25 19:38 something1.txt
-rw-------    1 1000     1000        67472 May 25 23:05 vsftpd.profraw
-rw-------    1 1000     1000        67656 May 26 02:27 vsftpd1.profraw
226 Directory send OK.
ftp> get something1.txt
local: something1.txt remote: something1.txt
200 PORT command successful. Consider using PASV.
150 Opening BINARY mode data connection for something1.txt (0 bytes).
226 Transfer complete.
ftp> ls
200 PORT command successful. Consider using PASV.
150 Here comes the directory listing.
drwx------    2 1000     1000         4096 Jun 07 05:53 extra
-rw-rw-rw-    1 1000     1000            0 May 25 19:38 something1.txt
-rw-------    1 1000     1000        67472 May 25 23:05 vsftpd.profraw
-rw-------    1 1000     1000        67656 May 26 02:27 vsftpd1.profraw
226 Directory send OK.
ftp> ls
200 PORT command successful. Consider using PASV.
150 Here comes the directory listing.
drwx------    2 1000     1000         4096 Jun 07 05:53 extra
-rw-rw-rw-    1 1000     1000            0 May 25 19:38 something1.txt
-rw-------    1 1000     1000        67472 May 25 23:05 vsftpd.profraw
-rw-------    1 1000     1000        67656 May 26 02:27 vsftpd1.profraw
226 Directory send OK.
ftp> get vsftpd.profraw
local: vsftpd.profraw remote: vsftpd.profraw
200 PORT command successful. Consider using PASV.
550 Failed to open file.
ftp> ls
200 PORT command successful. Consider using PASV.
150 Here comes the directory listing.
drwx------    2 1000     1000         4096 Jun 07 05:53 extra
-rw-rw-rw-    1 1000     1000            0 May 25 19:38 something1.txt
-rw-------    1 1000     1000        67472 May 25 23:05 vsftpd.profraw
-rw-------    1 1000     1000        67656 May 26 02:27 vsftpd1.profraw
226 Directory send OK.
ftp> ls
200 PORT command successful. Consider using PASV.
150 Here comes the directory listing.
drwx------    2 1000     1000         4096 Jun 07 05:53 extra
-rw-rw-rw-    1 1000     1000            0 May 25 19:38 something1.txt
-rw-------    1 1000     1000        67472 May 25 23:05 vsftpd.profraw
-rw-------    1 1000     1000        67656 May 26 02:27 vsftpd1.profraw
226 Directory send OK.
ftp> ls
200 PORT command successful. Consider using PASV.
150 Here comes the directory listing.
drwx------    2 1000     1000         4096 Jun 07 05:53 extra
-rw-rw-rw-    1 1000     1000            0 May 25 19:38 something1.txt
-rw-------    1 1000     1000        67472 May 25 23:05 vsftpd.profraw
-rw-------    1 1000     1000        67656 May 26 02:27 vsftpd1.profraw
226 Directory send OK.
ftp> pasv
?Invalid command
ftp> passive
Passive mode on.
ftp> ls
227 Entering Passive Mode (127,0,0,1,232,199).
150 Here comes the directory listing.
drwx------    2 1000     1000         4096 Jun 07 05:53 extra
-rw-rw-rw-    1 1000     1000            0 May 25 19:38 something1.txt
-rw-------    1 1000     1000        67472 May 25 23:05 vsftpd.profraw
-rw-------    1 1000     1000        67656 May 26 02:27 vsftpd1.profraw
226 Directory send OK.
ftp> get vsftpd.profraw
local: vsftpd.profraw remote: vsftpd.profraw
227 Entering Passive Mode (127,0,0,1,56,91).
550 Failed to open file.
ftp> ls
227 Entering Passive Mode (127,0,0,1,185,68).
150 Here comes the directory listing.
drwx------    2 1000     1000         4096 Jun 07 05:53 extra
-rw-rw-rw-    1 1000     1000            0 May 25 19:38 something1.txt
-rw-------    1 1000     1000        67472 May 25 23:05 vsftpd.profraw
-rw-------    1 1000     1000        67656 May 26 02:27 vsftpd1.profraw
226 Directory send OK.
ftp> cd extra
250 Directory successfully changed.
ftp> ls
227 Entering Passive Mode (127,0,0,1,227,37).
150 Here comes the directory listing.
226 Transfer done (but failed to open directory).
ftp> cd ..
250 Directory successfully changed.
ftp> ls
227 Entering Passive Mode (127,0,0,1,82,216).
150 Here comes the directory listing.
drwx------    2 1000     1000         4096 Jun 07 05:53 extra
-rw-rw-rw-    1 1000     1000            0 May 25 19:38 something1.txt
-rw-------    1 1000     1000        67472 May 25 23:05 vsftpd.profraw
-rw-------    1 1000     1000        67656 May 26 02:27 vsftpd1.profraw
226 Directory send OK.
ftp> ls
227 Entering Passive Mode (127,0,0,1,207,197).
150 Here comes the directory listing.
drwx------    2 1000     1000         4096 Jun 07 05:53 extra
-rw-rw-rw-    1 1000     1000            0 May 25 19:38 something1.txt
-rw-------    1 1000     1000        67472 May 25 23:05 vsftpd.profraw
-rw-------    1 1000     1000        67656 May 26 02:27 vsftpd1.profraw
226 Directory send OK.
ftp> ls
227 Entering Passive Mode (127,0,0,1,216,117).
150 Here comes the directory listing.
drwx------    2 1000     1000         4096 Jun 07 05:53 extra
-rw-rw-rw-    1 1000     1000            0 May 25 19:38 something1.txt
-rw-------    1 1000     1000        67472 May 25 23:05 vsftpd.profraw
-rw-------    1 1000     1000        67656 May 26 02:27 vsftpd1.profraw
226 Directory send OK.
ftp> send secbuf.c
local: secbuf.c remote: secbuf.c
227 Entering Passive Mode (127,0,0,1,214,192).
150 Ok to send data.
226 Transfer complete.
2370 bytes sent in 0.00 secs (32.2887 MB/s)
ftp> ls
227 Entering Passive Mode (127,0,0,1,154,11).
150 Here comes the directory listing.
drwx------    2 1000     1000         4096 Jun 07 05:53 extra
-rw-------    1 1000     1000         2370 Jun 07 14:01 secbuf.c
-rw-rw-rw-    1 1000     1000            0 May 25 19:38 something1.txt
-rw-------    1 1000     1000        67472 May 25 23:05 vsftpd.profraw
-rw-------    1 1000     1000        67656 May 26 02:27 vsftpd1.profraw
226 Directory send OK.
ftp> ls
227 Entering Passive Mode (127,0,0,1,37,58).
150 Here comes the directory listing.
drwx------    2 1000     1000         4096 Jun 07 05:53 extra
-rw-------    1 1000     1000         2370 Jun 07 14:01 secbuf.c
-rw-rw-rw-    1 1000     1000            0 May 25 19:38 something1.txt
-rw-------    1 1000     1000        67472 May 25 23:05 vsftpd.profraw
-rw-------    1 1000     1000        67656 May 26 02:27 vsftpd1.profraw
226 Directory send OK.
ftp> ls
227 Entering Passive Mode (127,0,0,1,21,124).
150 Here comes the directory listing.
drwx------    2 1000     1000         4096 Jun 07 05:53 extra
-rw-------    1 1000     1000         2370 Jun 07 14:01 secbuf.c
-rw-rw-rw-    1 1000     1000            0 May 25 19:38 something1.txt
-rw-------    1 1000     1000        67472 May 25 23:05 vsftpd.profraw
-rw-------    1 1000     1000        67656 May 26 02:27 vsftpd1.profraw
226 Directory send OK.
ftp> ls -l extra
output to local-file: extra? y
227 Entering Passive Mode (127,0,0,1,234,65).
150 Here comes the directory listing.
226 Directory send OK.
ftp> ls
227 Entering Passive Mode (127,0,0,1,222,232).
150 Here comes the directory listing.
drwx------    2 1000     1000         4096 Jun 07 05:53 extra
-rw-------    1 1000     1000         2370 Jun 07 14:01 secbuf.c
-rw-rw-rw-    1 1000     1000            0 May 25 19:38 something1.txt
-rw-------    1 1000     1000        67472 May 25 23:05 vsftpd.profraw
-rw-------    1 1000     1000        67656 May 26 02:27 vsftpd1.profraw
226 Directory send OK.
ftp> delete secbuf.c
250 Delete operation successful.
ftp> ls
227 Entering Passive Mode (127,0,0,1,212,201).
150 Here comes the directory listing.
drwx------    2 1000     1000         4096 Jun 07 05:53 extra
-rw-rw-rw-    1 1000     1000            0 May 25 19:38 something1.txt
-rw-------    1 1000     1000        67472 May 25 23:05 vsftpd.profraw
-rw-------    1 1000     1000        67656 May 26 02:27 vsftpd1.profraw
226 Directory send OK.
ftp> ls
227 Entering Passive Mode (127,0,0,1,143,150).
150 Here comes the directory listing.
drwx------    2 1000     1000         4096 Jun 07 05:53 extra
-rw-rw-rw-    1 1000     1000            0 May 25 19:38 something1.txt
-rw-------    1 1000     1000        67472 May 25 23:05 vsftpd.profraw
-rw-------    1 1000     1000        67656 May 26 02:27 vsftpd1.profraw
226 Directory send OK.
ftp> send secbuf.c
local: secbuf.c remote: secbuf.c
227 Entering Passive Mode (127,0,0,1,215,95).
150 Ok to send data.
226 Transfer complete.
2370 bytes sent in 0.00 secs (35.8763 MB/s)
ftp> ls
227 Entering Passive Mode (127,0,0,1,63,191).
150 Here comes the directory listing.
drwx------    2 1000     1000         4096 Jun 07 05:53 extra
-rw-------    1 1000     1000         2370 Jun 07 14:02 secbuf.c
-rw-rw-rw-    1 1000     1000            0 May 25 19:38 something1.txt
-rw-------    1 1000     1000        67472 May 25 23:05 vsftpd.profraw
-rw-------    1 1000     1000        67656 May 26 02:27 vsftpd1.profraw
226 Directory send OK.
ftp> ls
227 Entering Passive Mode (127,0,0,1,59,38).
150 Here comes the directory listing.
drwx------    2 1000     1000         4096 Jun 07 05:53 extra
-rw-------    1 1000     1000         2370 Jun 07 14:02 secbuf.c
-rw-rw-rw-    1 1000     1000            0 May 25 19:38 something1.txt
-rw-------    1 1000     1000        67472 May 25 23:05 vsftpd.profraw
-rw-------    1 1000     1000        67656 May 26 02:27 vsftpd1.profraw
226 Directory send OK.
ftp> ?
Commands may be abbreviated.  Commands are:

!               dir             mdelete         qc              site
$               disconnect      mdir            sendport        size
account         exit            mget            put             status
append          form            mkdir           pwd             struct
ascii           get             mls             quit            system
bell            glob            mode            quote           sunique
binary          hash            modtime         recv            tenex
bye             help            mput            reget           tick
case            idle            newer           rstatus         trace
cd              image           nmap            rhelp           type
cdup            ipany           nlist           rename          user
chmod           ipv4            ntrans          reset           umask
close           ipv6            open            restart         verbose
cr              lcd             prompt          rmdir           ?
delete          ls              passive         runique
debug           macdef          proxy           send
ftp> ls
227 Entering Passive Mode (127,0,0,1,159,44).
150 Here comes the directory listing.
drwx------    2 1000     1000         4096 Jun 07 05:53 extra
-rw-------    1 1000     1000         2370 Jun 07 14:02 secbuf.c
-rw-rw-rw-    1 1000     1000            0 May 25 19:38 something1.txt
-rw-------    1 1000     1000        67472 May 25 23:05 vsftpd.profraw
-rw-------    1 1000     1000        67656 May 26 02:27 vsftpd1.profraw
226 Directory send OK.
ftp> cdup
250 Directory successfully changed.
ftp> ls
227 Entering Passive Mode (127,0,0,1,210,174).
150 Here comes the directory listing.
-rw-r--r--    1 1000     1000        40224 Jun 07 13:46 README.md
-rw-r--r--    1 0        0              15 Jun 07 05:40 anewfile.txt
drwxrwxrwx    3 1000     1000         4096 Jun 07 14:02 uploads
226 Directory send OK.
ftp> ls
227 Entering Passive Mode (127,0,0,1,160,178).
150 Here comes the directory listing.
-rw-r--r--    1 1000     1000        40224 Jun 07 13:46 README.md
-rw-r--r--    1 0        0              15 Jun 07 05:40 anewfile.txt
drwxrwxrwx    3 1000     1000         4096 Jun 07 14:02 uploads
226 Directory send OK.
ftp> ?
Commands may be abbreviated.  Commands are:

!               dir             mdelete         qc              site
$               disconnect      mdir            sendport        size
account         exit            mget            put             status
append          form            mkdir           pwd             struct
ascii           get             mls             quit            system
bell            glob            mode            quote           sunique
binary          hash            modtime         recv            tenex
bye             help            mput            reget           tick
case            idle            newer           rstatus         trace
cd              image           nmap            rhelp           type
cdup            ipany           nlist           rename          user
chmod           ipv4            ntrans          reset           umask
close           ipv6            open            restart         verbose
cr              lcd             prompt          rmdir           ?
delete          ls              passive         runique
debug           macdef          proxy           send
ftp> chmod 777 README
550 Permission denied.
ftp> chmod 777 README.md
550 Permission denied.
ftp> debug
Debugging on (debug=1).
ftp> debug
Debugging off (debug=0).
ftp> debug
Debugging on (debug=1).
ftp> ls
ftp: setsockopt (ignored): Permission denied
---> PASV
227 Entering Passive Mode (127,0,0,1,197,14).
---> LIST
150 Here comes the directory listing.
-rw-r--r--    1 1000     1000        40224 Jun 07 13:46 README.md
-rw-r--r--    1 0        0              15 Jun 07 05:40 anewfile.txt
drwxrwxrwx    3 1000     1000         4096 Jun 07 14:02 uploads
226 Directory send OK.
ftp> debug
Debugging off (debug=0).
ftp> ls
227 Entering Passive Mode (127,0,0,1,76,66).
150 Here comes the directory listing.
-rw-r--r--    1 1000     1000        40224 Jun 07 13:46 README.md
-rw-r--r--    1 0        0              15 Jun 07 05:40 anewfile.txt
drwxrwxrwx    3 1000     1000         4096 Jun 07 14:02 uploads
226 Directory send OK.
ftp> ?
Commands may be abbreviated.  Commands are:

!               dir             mdelete         qc              site
$               disconnect      mdir            sendport        size
account         exit            mget            put             status
append          form            mkdir           pwd             struct
ascii           get             mls             quit            system
bell            glob            mode            quote           sunique
binary          hash            modtime         recv            tenex
bye             help            mput            reget           tick
case            idle            newer           rstatus         trace
cd              image           nmap            rhelp           type
cdup            ipany           nlist           rename          user
chmod           ipv4            ntrans          reset           umask
close           ipv6            open            restart         verbose
cr              lcd             prompt          rmdir           ?
delete          ls              passive         runique
debug           macdef          proxy           send
ftp> ls
227 Entering Passive Mode (127,0,0,1,217,231).
150 Here comes the directory listing.
-rw-r--r--    1 1000     1000        40224 Jun 07 13:46 README.md
-rw-r--r--    1 0        0              15 Jun 07 05:40 anewfile.txt
drwxrwxrwx    3 1000     1000         4096 Jun 07 14:02 uploads
226 Directory send OK.
ftp> ?
Commands may be abbreviated.  Commands are:

!               dir             mdelete         qc              site
$               disconnect      mdir            sendport        size
account         exit            mget            put             status
append          form            mkdir           pwd             struct
ascii           get             mls             quit            system
bell            glob            mode            quote           sunique
binary          hash            modtime         recv            tenex
bye             help            mput            reget           tick
case            idle            newer           rstatus         trace
cd              image           nmap            rhelp           type
cdup            ipany           nlist           rename          user
chmod           ipv4            ntrans          reset           umask
close           ipv6            open            restart         verbose
cr              lcd             prompt          rmdir           ?
delete          ls              passive         runique
debug           macdef          proxy           send
ftp> form
We only support non-print format, sorry.
ftp> glob
Globbing off.
ftp> glob
Globbing on.
ftp> hash
Hash mark printing on (1024 bytes/hash mark).
ftp> idle
550 Permission denied.
ftp> image
200 Switching to Binary mode.
ftp> lcd
Local directory now /root/vsftpd-3.0.3
ftp> newer
(remote-file) README.md
(local-file) README.md
local: README.md remote: README.md
227 Entering Passive Mode (127,0,0,1,175,40).
150 Opening BINARY mode data connection for README.md (40224 bytes).
#######################################
226 Transfer complete.
40224 bytes received in 0.00 secs (127.8686 MB/s)
ftp> ls
227 Entering Passive Mode (127,0,0,1,191,78).
150 Here comes the directory listing.
-rw-r--r--    1 1000     1000        40224 Jun 07 13:46 README.md
-rw-r--r--    1 0        0              15 Jun 07 05:40 anewfile.txt
drwxrwxrwx    3 1000     1000         4096 Jun 07 14:02 uploads
226 Directory send OK.
ftp> nmap
Nmap off.
ftp> nmap
Nmap off.
ftp> prompt
Interactive mode off.
ftp> prompt
Interactive mode on.
ftp> passive
Passive mode off.
ftp>
ftp>
ftp> qc
Quote control characters off.
ftp> qc
Quote control characters on.
ftp>
ftp>
ftp> put
(local-file) README.md
(remote-file) README.md
local: README.md remote: README.md
200 PORT command successful. Consider using PASV.
150 Ok to send data.
#######################################
226 Transfer complete.
40224 bytes sent in 0.00 secs (137.0021 MB/s)
ftp> ls
200 PORT command successful. Consider using PASV.
150 Here comes the directory listing.
-rw-r--r--    1 1000     1000        40224 Jun 07 14:05 README.md
-rw-r--r--    1 0        0              15 Jun 07 05:40 anewfile.txt
drwxrwxrwx    3 1000     1000         4096 Jun 07 14:02 uploads
226 Directory send OK.
ftp> ?
Commands may be abbreviated.  Commands are:

!               dir             mdelete         qc              site
$               disconnect      mdir            sendport        size
account         exit            mget            put             status
append          form            mkdir           pwd             struct
ascii           get             mls             quit            system
bell            glob            mode            quote           sunique
binary          hash            modtime         recv            tenex
bye             help            mput            reget           tick
case            idle            newer           rstatus         trace
cd              image           nmap            rhelp           type
cdup            ipany           nlist           rename          user
chmod           ipv4            ntrans          reset           umask
close           ipv6            open            restart         verbose
cr              lcd             prompt          rmdir           ?
delete          ls              passive         runique
debug           macdef          proxy           send
ftp>
ftp> pwd
257 "/" is the current directory
ftp> quote
(command line to send)
usage: quote line-to-send
ftp> rename
(from-name) README.md README1.md
350 Ready for RNTO.
550 Rename failed.
ftp> ls
200 PORT command successful. Consider using PASV.
150 Here comes the directory listing.
-rw-r--r--    1 1000     1000        40224 Jun 07 14:05 README.md
-rw-r--r--    1 0        0              15 Jun 07 05:40 anewfile.txt
drwxrwxrwx    3 1000     1000         4096 Jun 07 14:02 uploads
226 Directory send OK.
ftp> cd uploads
250 Directory successfully changed.
ftp> ls
200 PORT command successful. Consider using PASV.
150 Here comes the directory listing.
drwx------    2 1000     1000         4096 Jun 07 05:53 extra
-rw-------    1 1000     1000         2370 Jun 07 14:02 secbuf.c
-rw-rw-rw-    1 1000     1000            0 May 25 19:38 something1.txt
-rw-------    1 1000     1000        67472 May 25 23:05 vsftpd.profraw
-rw-------    1 1000     1000        67656 May 26 02:27 vsftpd1.profraw
226 Directory send OK.
ftp> rename
(from-name) vsftpd.profraw
(to-name) vsftpd1.profraw
350 Ready for RNTO.
250 Rename successful.
ftp> ls
200 PORT command successful. Consider using PASV.
150 Here comes the directory listing.
drwx------    2 1000     1000         4096 Jun 07 05:53 extra
-rw-------    1 1000     1000         2370 Jun 07 14:02 secbuf.c
-rw-rw-rw-    1 1000     1000            0 May 25 19:38 something1.txt
-rw-------    1 1000     1000        67472 May 25 23:05 vsftpd1.profraw
226 Directory send OK.
ftp> ?
Commands may be abbreviated.  Commands are:

!               dir             mdelete         qc              site
$               disconnect      mdir            sendport        size
account         exit            mget            put             status
append          form            mkdir           pwd             struct
ascii           get             mls             quit            system
bell            glob            mode            quote           sunique
binary          hash            modtime         recv            tenex
bye             help            mput            reget           tick
case            idle            newer           rstatus         trace
cd              image           nmap            rhelp           type
cdup            ipany           nlist           rename          user
chmod           ipv4            ntrans          reset           umask
close           ipv6            open            restart         verbose
cr              lcd             prompt          rmdir           ?
delete          ls              passive         runique
debug           macdef          proxy           send
ftp> site
(arguments to SITE command) foobar
550 Permission denied.
ftp> size
(filename) vsftpd1.profraw
213 67472
ftp> status
Connected to 127.0.0.1.
No proxy connection.
Connecting using address family: any.
Mode: stream; Type: binary; Form: non-print; Structure: file
Verbose: on; Bell: off; Prompting: on; Globbing: on
Store unique: off; Receive unique: off
Case: off; CR stripping: on
Quote control characters: on
Ntrans: off
Nmap: off
Hash mark printing: on; Use of PORT cmds: on
Tick counter printing: off
ftp> struct
We only support file structure, sorry.
ftp> system
215 UNIX Type: L8
ftp> sunique
Store unique on.
ftp> tenex
200 Switching to Binary mode.
ftp> tick
Hash mark printing off.
Tick counter printing on (10240 bytes/tick increment).
ftp> trace
Packet tracing on.
ftp> trace
Packet tracing off.
ftp> type
Using tenex mode to transfer files.
ftp> user
(username) foobar
530 Can't change from guest user.
Login failed.
ftp> umask
550 Permission denied.
ftp>
ftp>

