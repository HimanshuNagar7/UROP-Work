#!/bin/bash
echo requestCount, time > timeData.csv
noCust=$1
noItem=$2
maxTime=$3
maxRequests=$4
for (( r=2; r <= maxRequests; r+=2))
do
  echo $r
  for x in {1..10}
  do
    python RandomRequests.py $noCust $noItem $maxTime $r $x
    ./clingo --opt-mode=optN -q --const noItem=$noItem --const noCust=$noCust --const maxTime=$maxTime allocTimeBookstore.lp happensEvents.lp | tail -2 | head -1 > line.txt
    TIME=$(python processLine.py)
    echo  $r,$TIME >> timeData.csv
  done
done
