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

### Bindings

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

### `Id`

### `Literal`

### `Var`

### `Type`
