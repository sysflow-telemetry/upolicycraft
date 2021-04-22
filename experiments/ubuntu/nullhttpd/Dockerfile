FROM ubuntu:16.04 as build

RUN apt-get update
RUN apt-get -y install build-essential

COPY src /nullhttpd

WORKDIR /nullhttpd

RUN make

FROM ubuntu:16.04

COPY --from=build /nullhttpd/nullhttpd /bin/nullhttpd
COPY html /var/www/html
COPY nullhttpd.sh /nullhttpd.sh

CMD ["/nullhttpd.sh"]
