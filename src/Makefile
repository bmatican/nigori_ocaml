.PHONY: all clean

all: crypto

crypto:
	ocamlc crypto.mli
	ocamlfind ocamlc -linkpkg -package cryptokit crypto.ml

clean:
	rm *.{cmi,cmo}

run:
	./a.out
