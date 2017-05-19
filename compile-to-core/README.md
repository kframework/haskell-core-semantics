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
