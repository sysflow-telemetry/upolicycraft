FROM binaryanalysisplatform/bap@sha256:59eb9b54d386439d969fcb257b3fc5d3132e9b1bf230001bddcb8fe78fda565c

USER root

ENV DEBIAN_FRONTEND=noninteractive
RUN ln -fs /usr/share/zoneinfo/America/New_York /etc/localtime

RUN echo "deb http://dk.archive.ubuntu.com/ubuntu/ xenial main" >> /etc/apt/sources.list
RUN echo "deb http://dk.archive.ubuntu.com/ubuntu/ xenial universe" >> /etc/apt/sources.list

RUN apt update
RUN apt -y --no-install-recommends install build-essential vim python python3-pip tzdata pkg-config libgmp-dev zlib1g-dev

COPY build /build

ENV PATH=/build/bin:$PATH

WORKDIR /debug

RUN make

ENV LD_LIBRARY_PATH=/debug/upc.so

USER opam

RUN opam config exec -- opam install core core_kernel shexp yojson camlp4

COPY --chown=opam bap-plugin/abi/posix.h /home/opam/.opam/4.09/share/bap/api/c/posix.h
COPY --chown=opam bap-plugin/systems/core.asd /home/opam/.opam/4.09/share/bap/primus/systems/core.asd
COPY --chown=opam bap-plugin/site-lisp /home/opam/.opam/4.09/share/bap/primus/site-lisp

COPY --chown=opam . /upc

WORKDIR /upc/bap-plugin

# Set up the environment

ENV CAML_LD_LIBRARY_PATH=/home/opam/.opam/4.09/lib/stublibs:/home/opam/.opam/4.09/lib/ocaml/stublibs:/home/opam/.opam/4.09/lib/ocaml
ENV OCAML_TOPLEVEL_PATH=/home/opam/.opam/4.09/lib/toplevel
ENV OPAMYES=1
ENV OPAM_SWITCH_PREFIX=/home/opam/.opam/4.09
ENV PATH=/home/opam/.opam/4.09/bin:/home/opam/.opam/4.09/bin:/uIDS/uIDS/bin/:/build/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
ENV PKG_CONFIG_PATH=/home/opam/.opam/4.09/lib/pkgconfig

RUN opam config exec -- oasis setup -setup-update dynamic
RUN opam config exec -- make
RUN opam config exec -- make install
RUN opam config exec -- ./bin/build-plugin.sh

WORKDIR /upc/bap-plugin/entrypoint

RUN opam config exec -- make

WORKDIR /upc/bap-plugin/

RUN cp /upc/bap-plugin/entrypoint/upc /upc/bap-plugin/bin/upc

ENV PATH="/upc/bap-plugin/bin/:${PATH}"

RUN touch /tmp/srv.log

ENTRYPOINT ["/upc/bap-plugin/bin/upc"]
