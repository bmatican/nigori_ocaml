.PHONY: all clean

CRYPTO=crypto
TEST_CRYPTO=test_$(CRYPTO)
CONSTANTS=constants

all: $(CRYPTO)

test: $(TEST_CRYPTO)
	./$(TEST_CRYPTO)

$(CRYPTO):
	ocamlc $@.mli
	ocamlfind ocamlc -linkpkg -package cryptokit $(CONSTANTS).ml $@.ml -o $@

$(TEST_CRYPTO): $(CRYPTO)
	ocamlfind ocamlc -linkpkg -package ounit -package cryptokit $(CONSTANTS).ml $(CRYPTO).ml $@.ml -o $@

clean:
	rm *.{cmi,cmo} $(CRYPTO) $(TEST_CRYPTO)

run: all
	./$(CRYPTO)
