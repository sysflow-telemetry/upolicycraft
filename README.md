<img align="center" src="https://github.com/sysflow-telemetry/upolicycraft/assets/928389/c053254f-b379-446a-a74a-c6569a18a5d7" width="80" height="80">

uPolicyCraft
============

A framework for generating and monitoring stateful security policies for microservices.

To cite our paper, please use the following:

    @inproceedings{uPolicyCraft:2024,
        title={Automated Synthesis of Effect Graph Policies for Microservice-Aware Stateful System Call Specialization},
        author={Blair, William and Araujo, Frederico and Taylor, Teryl, and Jang, Jiyong},
        booktitle={IEEE Symposium on Security and Privacy (Oakland)},
        month={5},
        year={2024},
        location={San Francisco, CA, USA}
    }

Note that this is not a supported product. It is intended as an experimental
approach to stateful system call specialization for microservices.

Tutorial
========

A working tutorial for running uPolicyCraft on a simple web server may be found in the
`tutorial` directory.

uPolicyCraft Workflow
=====================

uPolicyCraft is divided into two components:

1) A micro execution framework for producing effect graphs for a microservice' binary.

2) The microservice-aware policy monitor (MPM) for monitoring these effect
   graphs (i.e., a distributed effect graph) over lightweight container telemetry
   provided by SysFlow.

Build Environment
=================

## Building An Artifact

In general, the Binary Analysis Platform (BAP) is well-suited for lifting,
analyzing, and micro-executing x86 executables (in addition to other
architectures). In our work, we primarily focused on executables produced by
GCC on Ubuntu. As a result, some offsets may need to be regenerated to target
different application binary interfaces (ABIs) for other platforms.
Fortunately, scripts can automate this using the target toolchain. In the past,
we noticed some restrictions needed to be imposed on the analyzed executable in
order to successfully micro-execute them.

However, we observed that as BAP and Primus matured, these restrictions
gradually were unneeded (e.g., omitting stack protectors due to an incomplete
ABI for segment registers or compiling without jump tables). A good approach
would be to keep symbols to simplify debugging modeling and omit optimizations.
In principle, uPolicyCraft should produce effect graphs for stripped
executables, but the labels on each effect graph would correspond to addresses
within the executable.

## Measuring Effect Coverage

To view the quality of your test inputs, you can measure effect coverage by doing the following.

    - Use brute-force micro execution to detect effects in external libraries.
      This yields a mapping of ABI functions to effects (i.e., syscalls).

    - Model the executable with the effect mapping, and decide whether your effect coverage
      is suitable for your microservice' configuration.

Once your test suite has achieved suitable effect coverage, you can take the inputs
for your application, which consist of both the container image filesystem and
network inputs, and generate an effect graph that the MPM can monitor.

Generating Effect Graphs
========================

In order to produce an effect graph for a given binary, an analyst may place a
microservice binary at a location that the micro execution framework may access.
This will typically be a folder that contains a container entrypoint, a filesystem
that represents the container image, and a corpus of test cases that uPolicyCraft
can use to micro-execute the entrypoint.

In order to build the micro execution modeling framework, you can do the following:

    docker build -f docker/Dockerfile -t sysflowtelemetry/upolicycraft:latest .

Micro-executing a given entrypoint will naturally require the program to
utilize external library dependencies.  Instead of micro-executing these
dependencies directly, uPolicyCraft makes use of BAP's ability to abstract
external functions using either the Primus LISP interpreter or the OCaml runtime.
The current implementation of uPolicyCraft attempts to provide a reasonable
amount of the Standard C Library to support obtaining the effect graphs for
container entrypoints.

Should an analyst need to extend the given ABI with a specific function, they
have two options. They can either implement the function in Primus LISP or in
OCaml. The former is desirable if the function can be represented as a simple
transformation of the input or some machine state, such as performing some
simple computation over memory, or updating an attribute accessible within a
struct. The LISP dialect present in Primus (Primus LISP) can be thought of
as a functional interface for updating machine state. A Primus LISP program
can define loops, perform simple machine arithmetic, and alter arbitrary
memory within the Primus Machine. For example, the following LISP function
computes the nth fibonacci number and stores the result into a pointer
given as an argument to the function.

    (defun fibonacci (n result)
      (declare (visibility :public) (external "fibonacci"))
      (let ((i 0)
            (j 1))
           (while (> n 0)
              (let ((t i))
                (set i j)
                (set j (+ t j)))
              (decr n))
            (write-word int result i)))

To make the micro execution framework aware of the fibonacci function, so that
any call to fibonacci in a binary resolves to this LISP implementation, we need
to declare a prototype of the function with the framework's application binary
interface (ABI). To do so, we can add the following prototype to
`uPolicyCraft/abi/posix.h`.

    void fibonacci (int n, int *result);

Note that you can make Primus aware of additional ABI functions by simply declaring
their C prototypes within this header file. Upon building uPolicyCraft, this header
file will be copied into the relevant folder within the docker image.

Once the prototype is added, we can add the LISP function to
`uPolicyCraft/site-lisp/fib.lisp` so that the micro execution framework can
take advantage of the added functionality.  To prevent having to build an image
every time you adjust the ABI, you can mount your local `site-lisp` and `abi`
to the correct locations when running the micro execution framework, as in the
following:

    docker run -v $PWD/uPolicyCraft/site-lisp:/home/opam/.opam/4.09/share/bap/primus/site-lisp -v $PWD/uPolicyCraft/abi/posix.h:/home/opam/.opam/4.09/share/bap/api/c/posix.h -v $PWD/uPolicyCraft/abi/core.asd:/home/opam/.opam/4.09/share/bap/primus/systems/core.asd --entrypoint /bin/bash -it sysflowtelemetry/uPolicyCraft:latest

The `core.asd` file defines the different Primus systems micro execution can
utilize. Depending on the needs of your analysis, you can take advantage of the
more interesting plugins found within Primus, such as those that assign random
values to unmapped memory regions, individual registers, or external functions
(i.e., the "probabilistic address space" described in our paper).  For the
purpose of generating effect graphs, we usually just require a stubbed and
bounded micro execution framework. Micro-executing Go programs benefit from the
probabilistic address space in order to abstract language data-structures not
relevant to a given effect graph. Note that, relying too much on a
probabilistic address space may decline the quality of the effect graph, for
example by yielding random system call arguments.

Implementing complex data-structures and algorithms in this LISP environment
may be too cumbersome, at which point it can be convenient to utilize the
functionality present in the OCaml runtime to implement an ABI function. Primus
allows plugin authors to extend the functionality of the LISP interpreter by
adding individual functions that LISP programs may call. This is helpful for
providing the LISP interpreter visibility into the filesystem or for providing
abstractions for network connections or complex functionality. Each of these
functions is backed by a corresponding OCaml function invoked by the LISP
interpreter. For example, modeling a web server will likely require some
regular expression matching in order to select the location to route individual
requests given as test cases. In the following example, we embed a new function
into the Primus LISP interpreter that checks whether a given string matches a
regular expression pattern using the popular Re2 regular expression matching
library. Observe that the OCaml code must first lift the strings located in
memory into OCaml strings since Primus LISP, like C, has no notion of strings
as first class values. The following snippet defines a new function in the LISP
interpreter.

    def "upc-ocaml-regex-match"
        (tuple [a; b] @-> bool)
        (module RegexMatch)
          {|(upc-ocaml-regex-match PATTERN S) checks whether s matches a given pattern. |}

and the following module implements a naive form of regular expression
matching. Observe that the run function is executed each time a LISP program
invokes the `upc-ocaml-regex-match` function.

    module RegexMatch (Machine : Primus.Machine.S) = struct
      [@@@warning "-P"]

      let addr_width =
        Machine.arch >>| Arch.addr_size >>| Size.in_bits
      let value_of_int x = addr_width >>= fun w -> Value.of_word (Bitvector.of_int ~width:w x)

      let run [pattern;s] =
        let wpattern = Value.to_word pattern in
        let ws = Value.to_word s in
        string_of_addr wpattern
        >>= fun pattern' ->
        string_of_addr ws >>= fun s' ->
        let re = Re2.create_exn pattern' in
        Re2.matches re str
    end

Note that Primus' ability to introspect into the binary's machine architecture
allows one to implement reusable ABIs across different instruction set
architectures.  More info on how to use BAP can be found on the project's
[website][1].

Code that expands the functionality of the LISP interpreter may be embedded in
the upc.ml file. Likewise with the LISP ABI, you may find it helpful to map the
upc.ml file into the appropriate location in the micro execution image during
development. When working inside a running container, you can run the micro
execution framework by invoking the `entrypoint/upc` program.  This program
aims to provide a convenient wrapper around the `bap` utility.

Microservice-Aware Policy Monitor (MPM)
=======================================

Once an analyst obtains an effect graph for a given binary, an operator can
provide the graph to an MPM for detecting possible intrusions in running
containers. You can build the MPM with the following:

     docker build -f docker/Dockerfile.plugins -t sysflowtelemetry/mpm:latest .

and then demonstrate the MPM working on a benign and compromised trace produced
by a web server.

     ./bin/mpm ../sysflow-plugin/mpm/resources/pipelines/echo.json ../sysflow-plugin/mpm/resources/traces/echo/benign/
     ./bin/mpm ../sysflow-plugin/mpm/resources/pipelines/echo.json ../sysflow-plugin/mpm/resources/traces/echo/malicious/

The JSON models produced by the micro execution framework may be referred to by
the "pipeline" file given to the MPM. That is, the distributed effect graphs
for whole microservice architectures described in the paper are documented
within a SysFlow pipeline. During development, we often informally referred to
this demonstration of effect graphs' capabilities to detect policy violations
as uIDS (microIDS or a micro Intrusion Detection System). More documentation on
how to construct telemetry pipelines can be found in the [SysFlow project][2].

[1]: https://binaryanalysisplatform.github.io
[2]: https://sysflow.io

BAP Version
===========

Currently running on a preview of BAP 2.6

2.6.0-alpha+77d09fe

Acknowledgment
==============

This research was developed with funding from the Defense Advanced Research
Projects Agency (DARPA). The views, opinions and/or findings expressed are
those of the authors and should not be interpreted as representing the official
views or policies of the Department of Defense or the U.S. Government.
