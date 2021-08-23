all: uIDS

uIDS: _build/default/uIDS.exe
	cp _build/default/uIDS.exe uIDS

_build/default/uIDS.exe: entrypoint.ml
	dune build uIDS.exe

.phony: clean

clean:
	rm -rf _build uIDS
