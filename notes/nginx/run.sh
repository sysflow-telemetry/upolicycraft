#!/usr/bin/env bash

docker run --net=host --rm -v $PWD/html:/usr/share/nginx/html:ro -d nginx
