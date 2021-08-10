FROM binaryanalysisplatform/bap@sha256:501f28bee8eaac8b63b7e72ce9218c52a93de850b1a7f2a91274bacd7e4f45a4 AS build

RUN opam install yojson camlp4
COPY --chown=opam . /uIDS

WORKDIR /uIDS/uIDS

RUN cp /uIDS/uIDS/support/posix.h /home/opam/.opam/4.09/share/bap-api/c/posix.h
RUN cp /uIDS/uIDS/support/ascii.lisp /home/opam/.opam/4.09/share/primus/site-lisp/ascii.lisp
RUN cp /uIDS/uIDS/support/atoi.lisp /home/opam/.opam/4.09/share/primus/site-lisp/atoi.lisp
RUN cp /uIDS/uIDS/support/stdlib.lisp /home/opam/.opam/4.09/share/primus/site-lisp/stdlib.lisp
RUN cp /uIDS/uIDS/support/string.lisp /home/opam/.opam/4.09/share/primus/site-lisp/string.lisp
RUN cp /uIDS/uIDS/support/simple-memory-allocator.lisp /home/opam/.opam/4.09/share/primus/site-lisp/simple-memory-allocator.lisp
RUN cp /uIDS/uIDS/support/stdio.lisp /home/opam/.opam/4.09/share/primus/site-lisp/stdio.lisp

RUN opam config exec -- oasis setup -setup-update dynamic
RUN opam config exec -- make
RUN opam config exec -- make install
RUN opam config exec -- ./bin/build-plugin.sh
RUN opam config exec -- opam install core core_kernel shexp

ENV PATH="/uIDS/uIDS/bin/:${PATH}"

ENTRYPOINT ["opam", "config", "exec", "--", "/uIDS/uIDS/bin/run"]

FROM build AS dev

RUN sudo apt-get -y update
RUN sudo apt-get -y install vim xinetd graphviz
RUN pip install defusedxml
RUN touch /tmp/srv.log

ENTRYPOINT ["/usr/bin/env", "bash"]
