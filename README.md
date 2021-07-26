uIDS
====

An Intrusion Detection System (IDS) for micro-services.

Evaluation:

All Docker Images for the Evaluation are contained in:

    experiments/ubuntu/

uIDS Workflow
=============

The uids-util docker image handles disassembling and inspecting docker images, the following
commands allow you to inspect a container so that micro execution can run the binary in a separate
container.

Save an image:

    docker save image-name > images/image-name.tar

Unpack an image

    util unpack -image image-name

Mount an image's filesystem

    util mount-layers -image image-name

Collect the entrypoint:

    util entrypoint -image image-name

Find the path to the entrypoint (on the host filesystem):

    util path -image image-name -path path-inside-image

Soon, find a binary with bash -x

    util binary -image image-name -path path-to-bash-script

Dev Workflow
=============

    source bin/env

    dev

Eval Workflow
=============
    
    Generating input for a challenge:

    If present, the xml file checked in to the challenge's folder can be passed to the cb-replay-dump script which stores the
    entire "write" transcript to the /tmp/data file.

    If an "input.log" is present, you can create the model by running `cat input.log | nc localhost port-of-challenge`

Tracking Line Coverage 
======================

    https://clang.llvm.org/docs/SourceBasedCodeCoverage.html


BAP Subtleties
==============

Passing multiple arguments to the binary under analysis.

time run /uIDS/tests/nullhttpd _start '["nullhttpd", "-h"]' /bin/bash /cgc/cromu.sh greedy "%000000" 100000000

References
==========

Tutorial:

    https://github.com/BinaryAnalysisPlatform/bap-tutorial

Start SysFlow Collector

    ./bin/sf-collector

Run the `server` process.

    docker run -it sysflowtelemetry/echo

Run the `server` exploit.

    (Sometimes the process crashes before reaching a shell, it eventually works for me)

    python2 server-exploit.py

An example SysFlow is given in exploit.sf with the information relevant to the exploit
stored in `exploit.md`
