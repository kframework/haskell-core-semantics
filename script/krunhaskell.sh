$HASKELL_CORE_SEMANTICS_DIR/script/compile_haskell.sh $1
echo "krun -d $HASKELL_CORE_SEMANTICS_DIR $1.pkore"
krun -d $HASKELL_CORE_SEMANTICS_DIR $1.pkore
rm $1.pkore
