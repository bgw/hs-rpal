# We're required to have a Makefile for COP5555, so this is just a stub of one
# that calls cabal

all: clean build

build:
	cabal configure
	cabal build

install:
	cabal install

clean:
	cabal clean
