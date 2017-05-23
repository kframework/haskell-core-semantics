all:
		time kompile --verbose --syntax-module HASKELL-CORE-SYNTAX src/haskell-core.k

clean:
		rm -rf src/haskell-core-kompiled
