uIDS
====

An Intrusion Detection System (IDS) for micro-services.

Tutorial
========

A working tutorial for running uIDS on a simple web server may be found in the
`tutorial` directory.

uIDS Workflow 
=============

uIDS is divided into three components, a build environment for producing a
suitable binary for modeling, a micro execution framework for producing effect
graphs for a container's binary, and the microservice-aware intrusion detection
system (MIDS) for enforcing these effect graphs over lightweight container
telemetry provided by SysFlow.

