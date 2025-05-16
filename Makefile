all: build

fmt:
	fourmolu -i app/

build: fmt
	hpack && cabal build -O2

install: build
	hpack && cabal install --overwrite-policy=always

.PHONY: fmt build install
