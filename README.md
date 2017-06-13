# GHC Core in **K**

Our ongoing work on the implementation of GHC's
[Core](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType)
language lives in this repository.

## Running

### Prerequisites

First make sure that you have [**K**](https://github.com/kframework/k) (latest
release) as well as [Haskell
Stack](https://docs.haskellstack.org/en/stable/README/) installed. Then:

* Run `make` in the root of the repository.
* Run `stack install` inside `compile-to-core`. This will build and install
our tool that converts Haskell programs into an intermediate representation of
of Haskell Core. To find out how to experiment with this tool, you can run
`to-core --help`.

### Running Haskell

You can use `script/krunhaskell.sh` to directly run Haskell code. First run
`source setup.sh` to configure the environment variable
`HASKELL_CORE_SEMANTICS_DIR` that will be needed. This will also alias
`krunhaskell` to the absolute path of `script/krunhaskell.sh` so that you can
use it anywhere.

In a Haskell file `Foo.hs` that you want to run, designate an expression by
adding a top-level declaration with the definiendum `result`; the definiens of
this declaration is the expression whose evaluation will be forced. For example,
`Foo.hs` might look like:
```haskell
module Foo where

result = (\x -> \y -> (\x -> x) x) 3 5
```
Then running
```bash
krunhaskell Foo
```
yields
```
<k> lit ( litInt ( 3 , [type omitted] ) ) </k>
```
which is what the result of running the Core program generated by `Foo.hs`
throught the Core semantics is. Our concrete representation of GHC Core [is
documented
here](https://github.com/kframework/haskell-core-semantics/blob/master/compile-to-core/README.md).
