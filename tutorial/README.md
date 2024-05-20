uPolicyCraft Tutorial
=====================

In this tutorial, we'll walk developers through protecting a web server with uPolicyCraft by doing the following:

    - Generating an effect graph for a simple echo server.
    - Using the Microservice-Aware Policy Monitor (MPM) to enforce the effect graph over the web server's container telemetry

Next, set up your shell to invoke the tutorial's commands:

    source bin/env.sh

Next, build the uPolicyCraft container image from the parent directory.

Construct an effect graph for the echo binary:

    model

View the corresponding effect graph:

    open output/output.pdf
    cat output/output.json

Enforce the effect graph over a benign trace:

    validate

Detect a backdoor shell:

    detect
