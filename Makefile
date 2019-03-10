build:
	@stack build

build-doc:
	@stack haddock --haddock-arguments "--odir=./docs"

setup:
	@stack setup

.PHONY: build build-doc setup
