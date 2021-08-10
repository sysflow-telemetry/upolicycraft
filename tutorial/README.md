uIDS Tutorial 
=============

In this tutorial, we'll walk developers through protecting a web server with uIDS by doing the following:

    - Generating an effect graph for a simple web server 
    - Using the MIDS to enforce the effect graph over the web server's container telemetry 

First, set up your shell to invoke the tutorial's commands:

    source bin/env.sh

Next, build the uIDS container images:

    build 

Construct an effect graph for the nullhttpd library:

    model 

View the corresponding effect graph:

    open output/output.pdf
    cat output/output.json 

Enforce the effect graph over a benign trace:

    validate

Detect an attempt to disclose system files:

    detect
