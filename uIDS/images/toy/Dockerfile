FROM registry.access.redhat.com/ubi7/ubi AS build 

RUN yum -y update 
RUN yum -y install gcc make

RUN mkdir /uIDS
COPY . /uIDS
WORKDIR /uIDS
RUN make fork-argv

FROM registry.access.redhat.com/ubi7/ubi 

RUN mkdir /uIDS
COPY --from=build /uIDS/fork-argv /uIDS/fork-argv
COPY entry.sh /uIDS/entry.sh

ENTRYPOINT ["/uIDS/entry.sh"]

