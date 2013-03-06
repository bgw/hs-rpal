COMPILER = ghc

all: clean build

build:
	ghc hsRpal.hs -o hsRpal

clean:
	-rm -f $(wildcard *.o *.hi) hsRpal
