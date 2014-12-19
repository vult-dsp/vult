for file in *.ml
do
  ocp-indent $file > $file.out
  cp $file.out $file
  rm $file.out
done