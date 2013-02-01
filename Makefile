.PHONY: all clean test run-server run-client

all:
	cd src && $(MAKE)

clean:
	cd src && $(MAKE) clean

test:
	cd src && $(MAKE) test

run-server: all
	./src/_build/nigori_server.byte

run-client: all
	./src/_build/nigori_client.byte
