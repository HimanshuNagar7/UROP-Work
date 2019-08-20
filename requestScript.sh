#!/bin/bash
echo requestCount,groundLength > requestData.csv
maxTime=$3
noCust=$1
noItem=$2
maxRequests=$4
for (( r=1; r<=maxRequests; r++ ))
do
  for x in {1..15}
  do
    python ExponentialRequests.py $noCust $noItem $maxTime $r $x  
    LINES=$(./clingo --text --const maxTime=$maxTime --const noItem=$noItem --const noCust=$noCust allocTimeBookstore.lp happensEvents.lp | wc -l)
    echo $r,$LINES >> requestData.csv
  done
done
