FROM registry.access.redhat.com/ubi7/ubi:7.8-255 as build

RUN yum -y install gcc make 
RUN mkdir /cgc
WORKDIR /cgc

ADD . /cgc/

WORKDIR /cgc/challenges/CADET_00001

RUN make

FROM registry.access.redhat.com/ubi7/ubi:7.8-255

RUN mkdir /cgc
COPY --from=build /cgc/challenges /cgc/challenges
COPY cgc.sh /cgc/cgc.sh

ENTRYPOINT ["/cgc/cgc.sh"]
