#!/bin/bash
echo itemCount,customerCount,groundLength > custItemData.csv
for i in {1..30}
do
  for c in {1..30}
  do
    OUTPUT="$(./clingo --text --const noItem=$i --const noCust=$c bookstore.lp | wc -l)"
    echo  "$i,$c,${OUTPUT}" >> custItemData.csv
  done  
done
