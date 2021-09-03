uIDS
====

An Intrusion Detection System (IDS) for micro-services.

Tutorial
========

A working tutorial for running uIDS on a simple web server may be found in the
`tutorial` directory.

uIDS Workflow
=============

uIDS is divided into three components:

1) A build environment for producing a binary suitable for modeling.

2) A micro execution framework for producing effect graphs for a container's binary.

3) The microservice-aware intrusion detection system (MIDS) for enforcing these
   effect graphs over lightweight container telemetry provided by SysFlow.

Build Environment
=================

## Building An Artifact

By default, this image comes with a build environment that allows an analyst to
produce a binary for micro execution and measure the code coverage achieved by the
program's test suite.

It can be helpful to use the `uIDS` docker image to build a given source tree
in order to produce a binary that can be modelled. Suppose your project is
given in the `src` directory. You can mount the directory within the `uIDS`
container and build your project with the following.

    docker run -e CFLAGS="-fno-stack-protector -fcf-protection=none" -e LDFLAGS="-L /build/uids/ -l:uids.so" -u $(id -u) -it -v $PWD:/host -w /host --entrypoint make sysflowtelemetry/uids:latest

This command assumes that your project uses CFLAGS and LDFLAGS to customize the
build of your project in a Makefile. You can always extend or alter the default
uIDS image to embed these flags however it may work for your project.

## Measuring Code Coverage

To get a sense of the quality of your test inputs, you can compile your application
with LLVM instrumentation so that every time you run your application, a coverage report
will be generated for you on disk.

    docker run -e CC="clang" -e CFLAGS="-fprofile-instr-generate -fcoverage-mapping" -e LDFLAGS="-L /build/uids/ -l:uids.so -fprofile-instr-generate -fcoverage-mapping" -u $(id -u) -it -v $PWD:/host -w /host --entrypoint make sysflowtelemetry/uids:latest

By default, the docker image stores profiling data in the `/tmp` directory which you
can alter by changing the built in LLVM environment flags in the Dockerfile, or specifying
your own environment variables at runtime. After your program exits, you can generate a
report using the following commands in the image.

     llvm-profdata merge /path/to/output.profraw -o /path/to/output.profdata
     llvm-cov report ./binary -instr-profile=/path/to/output.profdata

Once your test suite has achieved suitable coverage, you can take the inputs
for your application which consist of both the container image filesystem and
network inputs and generate an effect graph to inform the MIDS.

Generating Effect Graphs
========================

In order to produce an effect graph for a given binary, an analyst may place a
binary produced by the provided build environment at a location that the micro
execution framework may access. This will typically be a folder that contains a
container entrypoint, a filesystem that represents the container image, and a
corpus of test cases that uIDS can use to produce to micro-execute the binary.

In order to build the micro execution framework, you can do the following:

    docker build -f docker/Dockerfile.experiment -t sysflowtelemetry/uids:latest .

If you would like a more stable but less recent build, you can check out the `v0.1.7`
tag and run the following:

    docker build -f docker/Dockerfile -t sysflowtelemetry/uids:latest .

Micro-executing a given entrypoint will naturally require the program to
utilize external library dependencies.  Instead of micro-executing these
dependencies directly, uIDS makes use of BAP's ability to abstract external
functions using either the Primus LISP interpreter or utilizing the OCaml
runtime. The current implementation of uIDS attempts to provide a reasonable
amount of the Standard C Library to support obtaining the effect graphs of a
container entrypoint. Should an analyst need to extend the given ABI with a
specific function, they have two options. They can either implement the
function in Primus LISP or in OCaml. The former is desirable if the function
can be represented as a simple transformation of the input or some machine
state, such as performing some simple computation over memory, or updating the
attribute given in a struct. The LISP dialect present in Primus can be thought
of as a functional interface for manipulating machine state, a program can
define loops, perform simple machine arithmetic, and alter arbitrary memory in
the Primus Machine.  For example, the following LISP function computes the nth
fibonacci number and stores the result into a pointer given as an argument.

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

To make the micro execution framework aware of the fibonacci function so that
any call to fibonacci in a binary resolves to the LISP implementation, we need
to include  prototype of the function with the framework's application binary
interface (ABI). To do so, we can add the following prototype to
`uIDS/abi/posix.h`.

    void fibonacci (int n, int *result);

Once the prototype is added, we can add the LISP function to
`uIDS/site-lisp/fib.lisp` so that the micro execution framework can take
advantage of the added functionality.  To prevent having to build an image
every time you adjust the ABI, you can mount your local `site-lisp` and `abi`
to the correct locations when running the micro execution framework, as in the
following:

    docker run -v $PWD/uIDS/site-lisp:/home/opam/.opam/4.09/share/bap/primus/site-lisp -v $PWD/uIDS/abi/posix.h:/home/opam/.opam/4.09/share/bap/api/c/posix.h -v $PWD/uIDS/abi/core.asd:/home/opam/.opam/4.09/share/bap/primus/systems/core.asd --entrypoint /bin/bash -it sysflowtelemetry/uids:latest

The `core.asd` file defines the different Primus systems micro execution can
run.  Depending on the needs of your analysis, you can take advantage of the
more interesting plugins found in Primus, such as those that assign random
values to unmapped memory regions, individual registers, or external functions.
For the purpose of generating effect graphs, we just require a stubbed micro
execution framework with a bounded execution time.

Implementing complex data-structures and algorithms in this environment may be
too cumbersome, at which point it can be convenient to utilize the
functionality present in the OCaml runtime to implement a function. Primus
allows plugin authors to extend the functionality of the LISP interpreter by
adding individual functions that LISP programs may call. This is helpful for
providing the LISP interpreter visibility into the filesystem or for providing
abstractions for network connections or complex functionality. Each of these
functions is backed by a corresponding OCaml function run by the LISP
interpreter. For example, modeling a web server will likely require some
regular expression matching in order to select the location to route individual
requests given as test cases. In the following example, we embed a new function
into the Primus LISP interpreter that checks whether a given string matches a
regular expression pattern using the popular Re2 regular expression matching
library. Observe that the OCaml code must first lift the strings given into
memory into OCaml strings since Primus LISP, like C, has no notion of strings
as first class values. The following snippet defines a new function in the LISP
interpreter.

    def "uids-ocaml-regex-match"
        (tuple [a; b] @-> bool)
        (module RegexMatch)
          {|(uids-ocaml-regex-match PATTERN S) checks whether s matches a given pattern. |}

and the following module implements a naive form of regular expression
matching. Observe that the run function is executed each time a LISP program
invokes the `uids-ocaml-regex-match` function.

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
allows one to implement one ABI that may be reused across different instruction
set architectures. More info on how to use BAP can be found on the project's [website][1].

Code that expands the functionality of the LISP interpreter may be embedded in
the uids.ml file. Likewise with the LISP ABI, you may find it helpful to map
the uids.ml file into the appropriate location in the micro execution image
during development. When working inside a running container, you can run the
micro execution framework by invoking the `entrypoint/uIDS` program, which aims
to provide a convenient wrapper around the `bap` utility.

Microservice-Aware Intrusion Detection System (MIDS)
====================================================

Once an analyst obtains an effect graph for a given binary, an operator can
provide the graph to a MIDS for detecting possible intrusions in running
containers. One can build the MIDS with the following:

     docker build -f docker/Dockerfile.plugins -t sysflowtelemetry/mids:latest .

and then demonstrate the MIDS working on a benign and compromised trace produced by a web server.

     ./bin/mids ../plugins/mids/resources/pipelines/nullhttpd.json ../plugins/mids/resources/traces/nullhttpd/benign/
     ./bin/mids ../plugins/mids/resources/pipelines/nullhttpd.json ../plugins/mids/resources/traces/nullhttpd/malicious/

The JSON models produced by the micro execution framework may be referred to by the "pipeline" file given to the MIDS, with some modfication needed depending on your container's operating environment and the filters used in your SysFlow
deployment. More documentation on how to construct telemetry pipelines can be found in the [SysFlow project][2].

[1]: http://binaryanalysisplatform.github.io
[2]: https://sysflow.io
