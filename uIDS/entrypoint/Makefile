all: uIDS

uIDS: _build/default/entrypoint.exe
	cp $< uIDS

_build/default/entrypoint.exe: entrypoint.ml
	dune build entrypoint.exe

.phony: clean

clean:
	rm -rf _build uIDS
