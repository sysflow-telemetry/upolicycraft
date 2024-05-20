all: uPolicyCraft

uPolicyCraft: _build/default/upc.exe
	rm -f ../bin/upc
	cp $< ../bin/upc

_build/default/upc.exe: upc.ml
	dune build upc.exe

.phony: clean

clean:
	rm -rf _build
