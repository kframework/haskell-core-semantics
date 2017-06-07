all:
		kompile --debug --verbose --syntax-module HASKELL-CORE-SYNTAX src/haskell-core.k
		rm -rf haskell-core-kompiled
		mv -f src/haskell-core-kompiled .

clean:
		rm -rf haskell-core-kompiled
