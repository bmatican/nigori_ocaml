.PHONY: all clean

all: 
	ocaml setup.ml -configure 
	ocaml setup.ml -build

test: 
	ocaml setup.ml -configure --enable-tests
	ocaml setup.ml -build
	ocaml setup.ml -test 

clean:
	ocamlbuild -clean
