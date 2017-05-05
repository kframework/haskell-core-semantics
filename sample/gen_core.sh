mkdir -p core
for i in `ls haskell`; do
  ghc -O2 -ddump-simpl -dsuppress-idinfo haskell/$i | tail -n +5 > core/$(basename $i .hs).hcr;
  echo "- Writing core/$(basename $i .hs).hcr..."
done
rm haskell/*.o
rm haskell/*.hi
