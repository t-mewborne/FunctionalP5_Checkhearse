# Commands:

.PHONY: build init test clean doc deploy stage

build: 
	ghc --make -O -o checkhearse Checkhearse.hs

prof:
	ghc --make -prof -o checkhearse Checkhearse.hs

all: build test

# Cleaning commands:
clean:
	rm -f classifier
	rm -f *.hi
	rm -f *.o

setup:
	cabal update
	cabal install ansi-terminal
