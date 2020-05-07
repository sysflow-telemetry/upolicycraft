all: leafnodes

.PHONY: leafnodes

leafnodes: leafnodes.ml
	eval `opam config env`
	bapbuild -pkg yojson leafnodes.plugin
	bapbundle install leafnodes.plugin

