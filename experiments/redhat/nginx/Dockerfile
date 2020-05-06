FROM registry.access.redhat.com/ubi7/ubi:7.8-255 as build

RUN yum -y install gcc make pcre-devel zlib-devel wget
RUN mkdir /nginx
WORKDIR /nginx
RUN wget http://nginx.org/download/nginx-1.17.10.tar.gz
RUN tar -xf nginx-1.17.10.tar.gz
WORKDIR /nginx/nginx-1.17.10
RUN ./configure
RUN make

FROM registry.access.redhat.com/ubi7/ubi:7.8-255

COPY --from=build /nginx/nginx-1.17.10/ /nginx/
COPY nginx.sh /nginx/
RUN mkdir /nginx/logs

ENTRYPOINT ["/nginx/nginx.sh"]
