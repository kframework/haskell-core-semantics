mkdir -p core
for i in `ls haskell`; do
  ghc -O0 -ddump-simpl haskell/$i | tail -n +5 > core/$(basename $i .hs).hcr
done
rm haskell/*.o
rm haskell/*.hi
