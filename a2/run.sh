#!/usr/bin/env zsh

for n in `seq 1 16`;
do
  for i in `seq 1 10`;
  do
    f="randTSP/$n/instance_$i.txt"
    echo "Running $f."
    cat "$f" | {time ./dist/build/tsp-search/tsp-search astar} 2>&1
    if [ $? -ne 0 ];
    then
      echo "FAILED"
      exit
    fi
  done
done

