mkdir -p core
for i in `ls haskell`; do
  ghc -ddump-ds -dsuppress-idinfo haskell/$i | tail -n +6 > core/$(basename $i .hs).hcr;
  echo "- Writing core/$(basename $i .hs).hcr..."
done
rm haskell/*.o
rm haskell/*.hi
