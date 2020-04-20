FROM docker.io/binaryanalysisplatform/bap:2.0.0 as bap

RUN opam install yojson camlp4
COPY --chown=opam . /uIDS

RUN sudo apk add jq python3

WORKDIR /uIDS/uIDS/plugins/model/

ENV CAML_LD_LIBRARY_PATH="/home/opam/.opam/4.07/lib/stublibs:/home/opam/.opam/4.07/lib/ocaml/stublibs:/home/opam/.opam/4.07/lib/ocaml"
ENV OCAML_TOPLEVEL_PATH="/home/opam/.opam/4.07/lib/toplevel"
ENV MANPATH=:/home/opam/.opam/4.07/man
ENV OPAM_SWITCH_PREFIX=/home/opam/.opam/4.07
ENV PATH=/home/opam/.opam/4.07/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

RUN oasis setup -setup-update dynamic
RUN make
RUN make install
RUN ./bin/build-plugin.sh

ENV PATH="/uIDS/uIDS/plugins/model/bin/:${PATH}"

ENTRYPOINT ["/uIDS/uIDS/plugins/model/bin/run.sh"]
