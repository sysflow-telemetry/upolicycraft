all: syscalls

.PHONY: syscalls

syscalls: syscalls.ml
	eval `opam config env`
	bapbuild syscalls.plugin
	bapbundle install syscalls.plugin
