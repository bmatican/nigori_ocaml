.PHONY: all clean test

all:
	cd src && $(MAKE)

clean:
	cd src && $(MAKE) clean

test:
	cd src && $(MAKE) test

run: all
	./src/_build/main.byte
