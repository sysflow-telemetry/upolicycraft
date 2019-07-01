all: cg

.PHONY: cg

cg: cg.ml
	eval `opam config env`
	bapbuild cg.plugin
	bapbundle install cg.plugin
