FROM ubuntu:18.04 as build

RUN apt update
RUN apt -y install gcc make

RUN mkdir /echo
WORKDIR /echo

ADD . /echo/

RUN make 

FROM ubuntu:18.04 

COPY --from=build /echo/echo /echo/echo
COPY echo.sh /echo/echo.sh
COPY echo.conf /echo/echo.conf

ENTRYPOINT ["/echo/echo.sh"]
