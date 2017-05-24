./gen_core.sh
echo "\n\n"
cd ..
for i in `ls test/core`; do
  echo "RUNNING \`kast test/core/$i\`"
  echo "--------------------------------------------------------------------------------"
  time kast test/core/$i | fold -w 80
  echo "\n"
done
