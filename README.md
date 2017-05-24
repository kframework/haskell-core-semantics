# GHC Core in **K**

Our ongoing work on the implementation of GHC's
[Core](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType)
language lives in this repository.

## Running

First make sure that you have [**K**](https://github.com/kframework/k) (latest
release) as well as [Haskell
Stack](https://docs.haskellstack.org/en/stable/README/) installed. Then:

* Run `make` in the root of the repository.
* Run `stack install` inside `compile-to-core`. This will build and install
our tool that converts Haskell programs into an intermediate representation of
of Haskell Core. To find out how to experiment with this tool, you can run
`to-core --help`.

You can generate some sample programs by running `gen_core.sh` in `test`.
Running `kast_all.sh` will run `kast` on all of these.
