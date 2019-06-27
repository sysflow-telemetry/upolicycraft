all: jmp

.PHONY: jmp

jmp: jmp.ml
	eval `opam config env`
	bapbuild jmp.plugin
	bapbundle install jmp.plugin

