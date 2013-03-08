COMPILER = ghc

all: clean build

build:
	ghc -Wall hsRpal.hs -o hsRpal

clean:
	-rm -f $(wildcard *.o *.hi) hsRpal
