.PHONY: all clean test oasis server

OASIS_FILE=_oasis

all: oasis server
	ocaml setup.ml -configure
	ocaml setup.ml -build

test: oasis, server
	ocaml setup.ml -configure --enable-tests
	ocaml setup.ml -build
	ocaml setup.ml -test 

oasis: $(OASIS_FILE)
	oasis setup

server: 
	cd server/ && $(MAKE)

clean:
	ocamlbuild -clean
	cd server/ && $(MAKE) clean
