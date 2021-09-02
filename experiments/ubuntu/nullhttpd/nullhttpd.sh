#!/usr/bin/env bash

/bin/nullhttpd -h 0.0.0.0 -p 80 -b /var/www/html -i /var/www/html/index.html -r /var/www/html/404.html -d /var/www/nullhttpd.pid -u www-data -g www-data 
