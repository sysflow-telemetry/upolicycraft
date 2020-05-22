FROM binaryanalysisplatform/bap@sha256:501f28bee8eaac8b63b7e72ce9218c52a93de850b1a7f2a91274bacd7e4f45a4

RUN opam install yojson camlp4
COPY --chown=opam . /uIDS

RUN sudo apt-get -y install jq python3

WORKDIR /uIDS/uIDS

RUN opam config exec -- oasis setup -setup-update dynamic
RUN opam config exec -- make
RUN opam config exec -- make install
RUN opam config exec -- ./bin/build-plugin.sh

ENV PATH="/uIDS/uIDS/bin/:${PATH}"

ENTRYPOINT ["opam", "config", "exec", "--", "/uIDS/uIDS/bin/run.sh"]
