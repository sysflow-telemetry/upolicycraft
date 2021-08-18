Demo
====

This server provides a simple program for demonstrating `uIDS`. The `nullhttpd`
directory provides better material for a demo using real software.
 
Running the echo server with core-dumps enabled and dropping into bash.

    docker run --rm -it --ulimit core=-1 -p 8080:8080 --entrypoint /bin/bash sysflowtelemetry/echo:latest

Running the echo server for a demo.

    docker run --rm -it -name echo -p 8080:8080 sysflowtelemetry/echo:latest

