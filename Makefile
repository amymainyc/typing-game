.PHONY: test check

build:
	dune build

code:
	-dune build
	code .

test:
	OCAMLRUNPARAM=b dune exec test/test.exe
	
game:
	OCAMLRUNPARAM=b dune exec bin/main.exe

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/test.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh