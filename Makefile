all:
		time kompile --verbose --syntax-module HASKELL-CORE-SYNTAX src/haskell-core.k
		mv src/haskell-core-kompiled .

clean:
		rm -rf src/haskell-core-kompiled
