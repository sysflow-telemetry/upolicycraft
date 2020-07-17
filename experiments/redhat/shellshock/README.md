# uIDS ShellShock evaluation

Start the sysflowtelemetry/httpd:latest container.

    docker run --rm -it -p 8080:80 sysflowtelemetry/httpd:latest

Deface the web server by exploiting the shellshock bug.

    ./exploit/exploit-deface.sh localhost 8081

