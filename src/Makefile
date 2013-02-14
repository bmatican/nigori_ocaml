.PHONY: all clean test oasis common

OASIS_FILE=_oasis
TESTS ?= --enable-tests
PREFIX ?= /usr/local

all: oasis common build ### test

oasis: $(OASIS_FILE)
	oasis setup

setup.bin: setup.ml
	ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	rm -f setup.cmx setup.cmi setup.o setup.cmo

setup.data: setup.bin
	./setup.bin -configure $(TESTS) --prefix $(PREFIX)

build: setup.data setup.bin
	./setup.bin -build

common: 
	cd common/ && $(MAKE)

test: oasis setup.bin build common
	./setup.bin -test

clean:
	ocamlbuild -clean
	rm -f setup.data setup.log setup.bin
	cd common/ && $(MAKE) clean
