FROM ubuntu:18.04 as build

RUN apt-get update
RUN apt-get -y install build-essential

COPY src /nullhttpd

WORKDIR /nullhttpd

RUN make

FROM ubuntu:18.04

COPY --from=build /nullhttpd/nullhttpd /bin/nullhttpd
COPY html /var/www/html
RUN touch /var/www/nullhttpd.pid
RUN chmod 777 /var/www/nullhttpd.pid
COPY nullhttpd.sh /nullhttpd.sh

CMD ["/nullhttpd.sh"]
