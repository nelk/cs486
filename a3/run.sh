#!/usr/bin/env zsh

for n in `seq 1 71`;
do
  for i in `seq 1 10`;
  do
    f="problems/$n/$i.sd"
    echo "Running $f."
    {time ./sudoku-solver "$f"} 2>&1
    #if [ $? -ne 0 ];
    #then
      #echo "FAILED"
      #exit
    #fi
  done
done

