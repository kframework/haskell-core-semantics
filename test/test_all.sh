cd ..
for file in `ls test/pkore-samples/*.pkore`;
  do
    echo "Running $file..."
    echo "--------------------------------------------------------------------------------"
    krun $file | tidy -i -q -xml | sed -e "s/~&gt;/⇝/g" | sed -e "s/|-&gt;/↦/g"
    echo ""
done
