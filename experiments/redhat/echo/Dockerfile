FROM registry.access.redhat.com/ubi7/ubi:7.8-255 as build

RUN yum -y install gcc make
RUN mkdir /echo
WORKDIR /echo

ADD . /echo/

RUN make

FROM registry.access.redhat.com/ubi7/ubi:7.8-255

COPY --from=build /echo/echo /echo/echo
COPY echo.sh /echo/echo.sh
COPY echo.conf /echo/echo.conf

ENTRYPOINT ["/echo/echo.sh"]
