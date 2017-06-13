to-core --no-types --strip-result -o out $1
cat out | grep "^nonRec" > $1.pkore
echo "" >> $1.pkore
cat out | grep "^[^nonRec]" >> $1.pkore
rm out
