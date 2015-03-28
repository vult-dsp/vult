#!/bin/bash
for file in src/*.ml
do
  ocp-indent $file > $file.out
  cp $file.out $file
  rm $file.out
done