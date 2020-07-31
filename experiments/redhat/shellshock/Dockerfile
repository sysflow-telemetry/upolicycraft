FROM registry.access.redhat.com/ubi7/ubi:7.8-255 as build

RUN yum -y install gcc make pcre-devel apr apr-devel apr-util apr-util-devel wget unzip
RUN wget https://apache.cs.utah.edu//httpd/httpd-2.4.43.tar.gz
RUN tar -xf httpd-2.4.43.tar.gz
RUN echo $PWD

WORKDIR /httpd-2.4.43

RUN ./configure --with-mpm=prefork --disable-shared
RUN make -j32
RUN make install

WORKDIR /

RUN wget https://github.com/bminor/bash/archive/bash-4.2.zip
RUN unzip bash-4.2.zip

WORKDIR /bash-bash-4.2

RUN ./configure
RUN make -j32
RUN make install

FROM registry.access.redhat.com/ubi7/ubi:7.8-255

RUN yum -y install pcre apr apr-util

RUN mkdir /httpd
COPY --from=build /usr/local/apache2 /usr/local/apache2
COPY --from=build /bash-bash-4.2/bash /bin/bash

ADD exploit/main.sh /httpd/main.sh
ADD exploit/httpd.conf /usr/local/apache2/conf/httpd.conf
ADD exploit/index.html /usr/local/apache2/htdocs/index.html
ADD exploit/vulnerable /usr/local/apache2/cgi-bin/vulnerable

RUN chown -R daemon:daemon /usr/local/apache2/htdocs/

EXPOSE 80

ENTRYPOINT ["/httpd/main.sh"]
