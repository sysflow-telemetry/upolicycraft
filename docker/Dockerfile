FROM binaryanalysisplatform/bap@sha256:f614e5295ef45a78670d1298343520a614bebe8792d7e195920b19c3277d0069

USER root

ENV DEBIAN_FRONTEND=noninteractive
RUN ln -fs /usr/share/zoneinfo/America/New_York /etc/localtime

RUN echo "deb http://dk.archive.ubuntu.com/ubuntu/ xenial main" >> /etc/apt/sources.list
RUN echo "deb http://dk.archive.ubuntu.com/ubuntu/ xenial universe" >> /etc/apt/sources.list

RUN apt update
RUN apt -y --no-install-recommends install build-essential vim python python3-pip tzdata pkg-config libgmp-dev zlib1g-dev

COPY debug /debug

WORKDIR /debug

RUN make

ENV LD_LIBRARY_PATH=/debug/upc.so

USER opam

RUN opam config exec -- opam install core core_kernel shexp yojson camlp4

COPY --chown=opam bap-plugin/abi/posix.h /home/opam/.opam/4.09/share/bap/api/c/posix.h
COPY --chown=opam bap-plugin/abi/core.asd /home/opam/.opam/4.09/share/bap/primus/systems/core.asd
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

# Build the BAP Plugin

RUN opam config exec -- oasis setup -setup-update dynamic
RUN opam config exec -- make
RUN opam config exec -- make install
RUN opam config exec -- ./bin/build-plugin.sh

# Build the entrypoint for invoking BAP + the plugin

WORKDIR /upc/bap-plugin/entrypoint

# Look up how to install dependencies via dune.
RUN opam install core_unix

RUN opam config exec -- make

WORKDIR /upc/bap-plugin/

USER root

RUN cp /upc/bap-plugin/entrypoint/_build/default/upc.exe  /upc/bap-plugin/bin/upc
RUN chown opam:opam /upc/bap-plugin/bin/upc

USER opam

ENV PATH="/upc/bap-plugin/bin/:${PATH}"

RUN touch /tmp/srv.log

ENTRYPOINT ["/upc/bap-plugin/bin/upc"]
