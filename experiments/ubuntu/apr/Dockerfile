FROM registry.access.redhat.com/ubi7/ubi AS build

RUN yum -y update
RUN yum -y install gcc make apr apr-devel apr-util apr-util-devel

RUN mkdir /uIDS
COPY . /uIDS
WORKDIR /uIDS
RUN make

FROM registry.access.redhat.com/ubi7/ubi

RUN yum -y update
RUN yum -y install apr apr-util

RUN mkdir /uIDS
COPY --from=build /uIDS/apr /uIDS/apr
COPY entry.sh /uIDS/entry.sh

ENTRYPOINT ["/uIDS/entry.sh"]

