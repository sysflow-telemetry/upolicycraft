all: leafnodes

.PHONY: leafnodes

leafnodes: leafnodes.ml
	eval `opam config env`
	bapbuild leafnodes.plugin
	bapbundle install leafnodes.plugin

