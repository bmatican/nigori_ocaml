.PHONY: all clean test oasis

OASIS_FILE=_oasis

all: oasis
	ocaml setup.ml -configure 
	ocaml setup.ml -build

test: oasis
	ocaml setup.ml -configure --enable-tests
	ocaml setup.ml -build
	ocaml setup.ml -test 

oasis: $(OASIS_FILE)
	oasis setup
	make clean

clean:
	ocamlbuild -clean
