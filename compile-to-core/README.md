# compile-to-core

To run the examples, first build with `stack build`. If you have a Haskell module named `Foo` run `stack exec to-core Foo` in the directory
of the file `Foo.hs`.

Currently, the following simple definition:
```haskell
one :: Integer
one = 1
```
results in:
```
nonRec(tmVar(tyConApp(algTyCon(Integer, tyConApp(primTyCon(TYPE), tyConApp(promDataCon(dataCon(PtrRepLifted)))))), rn8),
       litInt(1, tyConApp(algTyCon(Integer, tyConApp(primTyCon(TYPE), tyConApp(promDataCon(dataCon(PtrRepLifted))))))))
``````

You can use the `--no-types` flag to omit the type information. For `one`, this results in the following:
```haskell
nonRec(tmVar(<type>, r1), litInt(1, <type>))
```

## The output format

We give the list of operators with their
arities in the style of sorted algebra. The only syntactic definition we have
is for the case of an operator `f` of arity `S₁ ⋯ Sₙ ⟶ S` being applied to
arguments `k₁, ..., kₙ` of sorts `S₁, ..., Sₙ` which is denoted:
```
f(k₁, ..., kₙ)
```

The syntactic forms of the constants (for example, the sort `Literal`) will
be explained in their corresponding sections.

### Type synonyms

Some of these type synonyms defined in the GHC source code can be confusing
to those who are not familiar with the implementation of Core. We list them to
prevent confusion:
* [`type CoreBndr = Var`](https://github.com/ghc/ghc/blob/6df8bef054db0b95bb8f9e55bb82580e27d251d6/compiler/coreSyn/CoreSyn.hs#L1734)

### `Binding`

Every Core program is a list of bindings (of type `Bind CoreBndr`). Every
binding is either recursive or not. There are two operators that construct
an AST of sort **Binding**: `rec` and `nonRec`.
```
  nonRec : CoreBndr ⟶ Expr ⟶ Binding
  rec    : BindingList ⟶ Binding
```

### `Binding` (`Bind CoreBndr` in GHC Core)

### `BindingList`

### `Expr` (`CoreExpr` in GHC Core)

There are 10 constructors for the sort `Expr`.

```
  var    : Id ⟶ Expr
  lit    : Literal ⟶ Expr
  app    : Expr Expr ⟶ Expr
  lam    : Binding Expr ⟶ Expr
  let    : Binding Expr ⟶ Expr
  case   : Expr Binding Type AltList ⟶ Expr
  cast   : Expr Coercion ⟶ Expr
  tick   : Tickish Expr ⟶ Expr
  type   : Type ⟶ Expr
  coerce : Coercion ⟶ Expr
```

### `Literal`

There are 14 operators of sort `Literal`.

```
  machChar           : Char ⟶ Literal
  machStr            : ByteString ⟶ Literal
  nullAddr           : ⟶ Literal
  machInt            : Integer ⟶ Literal
  machInt64          : Integer ⟶ Literal
  machWord           : Integer ⟶ Literal
  machWord64         : Integer ⟶ Literal
  machFloat          : Rational ⟶ Literal
  machDouble         : Rational ⟶ Literal
  machLabelFunSome   : String Int ⟶ Literal
  machLabelDataSome  : String Int ⟶ Literal
  machLabelFunNone   : String ⟶ Literal
  machLabelDataNone  : String ⟶ Literal
  litInt             : Integer Type ⟶ Literal
```

### `Var`

### `DataCon`

### `AltCon`

There are three operators of sort `AltCon`.

```
dataAlt    : DataCon ⟶ AltCon
litAlt     : Literal ⟶ AltCon
defaultAlt : ⟶ AltCon
```

### `Type`

__TODO__: Leaving this out for now as it is not a priority.

### `UnivCoProvenance`

__TODO__

### `Role`

There are 3 constants of sort `Role`.

```
nom    : ⟶ Role
repr   : ⟶ Role
phant  : ⟶ Role
```

### `Coercion`

__TODO__
