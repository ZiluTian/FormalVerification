#!/bin/bash
names=("uf20-91" "uf50-218")
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