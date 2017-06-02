cd ..
for file in `ls test/pkore-samples`;
  do
    echo "Running $file..."
    echo "--------------------------------------------------------------------------------"
    krun test/pkore-samples/$file | tidy -i -q -xml | sed -e "s/~&gt;/⇝/g" | sed -e "s/|-&gt;/↦/g"
    echo ""
done
