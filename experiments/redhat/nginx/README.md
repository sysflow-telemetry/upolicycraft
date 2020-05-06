NGINX Dockerfile 
================

This Dockerfile builds the latest NGINX within a UBI7
environment that Primus can micro-execute.

You can build the container with

    ./build.sh

After building the container, you can start the web server with the following:

    docker run -it -p 80:80 sysflowtelemetry/nginx:latest

and then connect to the NGINX welcome page 

    http://localhost:80
