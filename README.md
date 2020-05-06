uIDS
====

An Intrusion Detection System (IDS) for micro-services.

Evaluation:

All Docker Images for the Evaluation are contained in:

    experiments/echo
    experiments/nginx

IDS Workflow:

The uids-util docker image handles disassembling and inspecting docker images, the following
commands allow you to inspect a container so that micro-execution can run the binary in a separate
container.

Unpack an image

    unpack -image image-name -path foo

Mount an image's filesystem

    mount-layers -image image-name -path foo

Collect the entrypoint:

    entrypoint -image image-name

Find the path to the entrypoint (on the host filesystem):

    path -image image-name -path path-inside-image

Soon, find a binary with bash -x

    binary -image image-name -path path-to-bash-script

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

    (Sometimes the process crashes before reaching a shell, it eventually works for me)

    python2 server-exploit.py

An example SysFlow is given in exploit.sf with the information relevant to the exploit
stored in `exploit.md`
