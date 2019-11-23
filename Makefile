# Commands:

.PHONY: build init test clean doc deploy stage

build: 
	ghc --make -O -o checkhearse IO.hs

prof:
	ghc --make -prof -o checkhearse IO.hs

all: build test

# Cleaning commands:
clean:
	rm -f checkhearse
	rm -f *.hi
	rm -f *.o

setup:
	cabal update
	cabal install ansi-terminal
