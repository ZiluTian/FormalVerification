#!/bin/bash
names=("uf20-91" "uf75-325" "uf50-218" "uuf50-218" "uf100-430" "uuf100-430" "uf125-538" "uf200-860" "uf250-1065")
for name in ${names[@]}
do
  (
    if [ ! -e $name ]; then
      mkdir $name
      cd $name
      wget "https://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/RND3SAT/$name.tar.gz"
      tar -xf $name.tar.gz
      rm $name.tar.gz
    fi
  )
done

mv uf75-325/ai/hoos/Shortcuts/UF75.325.100/* uf75-325
rm -r uf75-325/ai

mv uf125-538/ai/hoos/Research/SAT/Formulae/UF125.538.100/* uf125-538
rm -r uf125-538/ai

mv uf250-1065/ai/hoos/Shortcuts/UF250.1065.100/* uf250-1065
rm -r uf250-1065/ai

mv uuf50-218/UUF50.218.1000/* uuf50-218/
rm -r uuf50-218/UUF50.218.1000

mv uuf100-430/UUF100.430.1000/* uuf100-430
rm -r uuf100-430/UUF100.430.1000
