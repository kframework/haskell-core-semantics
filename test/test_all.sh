cd ..
make
for file in `ls test/pkore-samples`;
  do krun test/pkore-samples/$file
done
