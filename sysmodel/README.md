sysmodel
========

Derive process behavior models from application binaries.

Perform a `bap` pass over a binary. For every dependency, fork and run a bap
pass on the dependency, and communicate the nodes that make the frontier to the
child bap passes.

For every child bap process, return its intermediate result and use
it to aggregate the final result.

For the process behavior model, return the graphlette given in the dependency's
relevant functions.

For estimating capabilities, return the system calls given in the dependency's
relevant functions.

IPC between `bap` instances can be accomplished using ocaml-zmq. (Use lwt)

Running Passes
==============

Traversing a call graph with BAP from an entrypoint:

    bap binary --pass cg --cg-entrypoint "@main"
