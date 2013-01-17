.PHONY: all clean

CRYPTO=crypto
TEST_CRYPTO=test_$(CRYPTO)

all: $(CRYPTO)

test: $(TEST_CRYPTO)
	./$(TEST_CRYPTO)

$(CRYPTO):
	ocamlc $@.mli
	ocamlfind ocamlc -linkpkg -package cryptokit $@.ml -o $@

$(TEST_CRYPTO): $(CRYPTO)
	ocamlfind ocamlc -linkpkg -package ounit -package cryptokit $(CRYPTO).ml $@.ml -o $@

clean:
	rm *.{cmi,cmo} $(CRYPTO) $(TEST_CRYPTO)

run: all
	./$(CRYPTO)
