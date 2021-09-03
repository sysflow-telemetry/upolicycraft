FROM ubuntu:20.04 as build

RUN apt update
RUN apt -y install build-essential
RUN mkdir /cgc
WORKDIR /cgc

ADD . /cgc/

WORKDIR /cgc/
RUN make -f /cgc/cgc.mk /cgc/libcgc.so

WORKDIR /cgc/challenges/CADET_00055
RUN make
WORKDIR /cgc/challenges/CROMU_00058
RUN make
WORKDIR /cgc/challenges/CROMU_00047
RUN make

FROM ubuntu:20.04

RUN apt update
RUN apt -y install xinetd

RUN mkdir /cgc
COPY --from=build /cgc/challenges /cgc/challenges
COPY ./xinetd.d/cgc /etc/xinetd.d/cgc

ENTRYPOINT ["xinetd", "-dontfork"]
