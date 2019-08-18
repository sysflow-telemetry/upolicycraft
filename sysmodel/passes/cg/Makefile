all: cg

.PHONY: cg

cg: cg.ml
	eval `opam config env`
	bapbuild -pkg "dot yojson" cg.plugin
	bapbundle install cg.plugin
