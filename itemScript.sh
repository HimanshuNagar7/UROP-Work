#!/bin/bash
echo itemCount,groundLength > itemData.csv
noCust=$1
maxItem=$2
maxTime=$3
avgNoRequests=$4
for (( i=1; i<=maxItem; i++))
do
  for x in {1..15}
  do
    python ExponentialRequests.py $noCust $i $maxTime $avgNoRequests $x  
    LINES=$(./clingo --text --const maxTime=$maxTime --const noItem=$i --const noCust=$noCust allocTimeBookstore.lp happensEvents.lp | wc -l)
    echo  $i,$LINES >> itemData.csv
  done
done
