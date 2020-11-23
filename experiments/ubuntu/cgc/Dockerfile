FROM ubuntu:16.04 as build

RUN apt -y install build-essential vim gdb python gcc-multilib python-pip xinetd 
RUN mkdir /cgc
WORKDIR /cgc

ADD . /cgc/

WORKDIR /cgc/challenges/CADET_00001
RUN make
WORKDIR /cgc/challenges/CROMU_00007
RUN make
WORKDIR /cgc/challenges/CROMU_00046
RUN make

FROM ubuntu:16.04 

RUN mkdir /cgc
COPY --from=build /cgc/challenges /cgc/challenges
COPY ./xinetd.d/cgc /etc/xinetd.d/cgc

ENTRYPOINT ["xinetd", "-dontfork"]
