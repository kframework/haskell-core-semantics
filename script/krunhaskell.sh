to-core --no-types --strip-result -o $1.pkore $1
echo "krun -d $HASKELL_CORE_SEMANTICS_DIR $1.pkore"
krun -d $HASKELL_CORE_SEMANTICS_DIR $1.pkore
rm $1.pkore
