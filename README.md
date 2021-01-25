uIDS
====

An Intrusion Detection System (IDS) for micro-services.

Evaluation:

All Docker Images for the Evaluation are contained in:

    experiments/redhat/fork
    experiments/redhat/echo
    experiments/redhat/nginx

uIDS Workflow
=============

The uids-util docker image handles disassembling and inspecting docker images, the following
commands allow you to inspect a container so that micro-execution can run the binary in a separate
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

    If present, the xml file checked in to the challenge's folder can be passed to the cb-replay-model script.

    If an "input.log" is present, you can create the model by running `cat input.log | nc localhost port-of-challenge`

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
