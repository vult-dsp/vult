#!/bin/bash
for file in src/*.ml
do
  ocp-indent $file > $file.out
  cp $file.out $file
  rm $file.out
done

for file in src/util/*.ml
do
  ocp-indent $file > $file.out
  cp $file.out $file
  rm $file.out
done

for file in src/passes/*.ml
do
  ocp-indent $file > $file.out
  cp $file.out $file
  rm $file.out
done