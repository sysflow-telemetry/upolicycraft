FROM docker.io/binaryanalysisplatform/bap:2.0.0 as bap

RUN opam install yojson camlp4
ENV PATH="/home/opam/.opam/4.07/bin:${PATH}"
COPY . /uIDS

USER root
RUN chown -R opam:opam /uIDS/
USER opam

WORKDIR /uIDS/uIDS/plugins/syscalls/
RUN oasis setup -setup-update dynamic
RUN make
RUN make install
RUN ./bin/build-plugin.sh

ENV PATH="/uIDS/uIDS/plugins/syscalls/bin/:${PATH}"

ENTRYPOINT ["/uIDS/uIDS/plugins/syscalls/bin/run.sh"]
