all: cg

.PHONY: cg

cg: cg.ml
	eval `opam config env`
	bapbuild -pkg yojson cg.plugin
	bapbundle install cg.plugin
