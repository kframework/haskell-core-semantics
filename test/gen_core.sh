mkdir -p core
cd haskell
for i in `ls *.hs`; do
  to-core --no-types $@ $(basename $i .hs) -o ../core/$(basename $i .hs).pkore;
  echo "- Writing core/$(basename $i .hs).pkore..."
done
