sf-model
========

Generating SysFlow Models from Container Binaries

Tutorial:

    https://github.com/BinaryAnalysisPlatform/bap-tutorial



8/23/2019 Demo Steps:
=========================

Go to the `echo` directory.

    cd sysmodel/artifacts/echo

Build the `server` and `libc.so` examples.

    make server libc.so

Output the leaves of `server`'s call graph.

    bap ./server --pass cg

Micro-Execute a function in our fake libc.

    ../../plugins/syscalls/bin/run.sh ./libc.so branching

Build a model of `echo` using Call Graph search and Primus.
    (~20 minutes since it lifts glibc.)

    ../../../bin/model ./server

The file `model.json` shows its output.

Start SysFlow.

    cd ~/ibm/sf-telemetry-stack
    ./start_probe

Run the `server` process.

    ./server 8081

Run the `server` exploit.

    (Sometimes the process crashes before reaching a shell)

    python2 server-exploit.py

An example SysFlow is given in exploit.sf with the information relevant to the exploit
stored in `exploit.md`
