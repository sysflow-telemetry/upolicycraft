FROM ubuntu:16.04 as build

RUN apt update
RUN apt install build-essential

COPY src /nullhttpd

WORKDIR /nullhttpd

FROM ubuntu:16.04

COPY --from=build /nullhttpd/nullhttpd /bin/nullhttpd
COPY static /var/www/html

CMD ["/bin/nullhttpd", "-h", "127.0.0.1", "-p", "2051", "-b", "../html", "-i", "../html/index.html", "-r", "../404.html", "-d", "../nullhttpd.pid", "-u", "0", "-g", "0"]
