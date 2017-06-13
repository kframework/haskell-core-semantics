mkdir -p core
cd haskell
for i in `ls *.hs`; do
  echo "file $i"
  to-core --no-types --strip-result $@ $(basename $i .hs) -o ../core/$(basename $i .hs).pkore;
  # echo "- Writing core/$(basename $i .hs).pkore..."
done
