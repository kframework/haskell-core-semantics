cd ..
for file in `ls test/pkore-samples`;
  do
    echo "Running $file..."
    echo "--------------------------------------------------------------------------------"
    krun test/pkore-samples/$file | tidy -i -xml
    echo ""
done
