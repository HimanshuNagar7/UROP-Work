#!/bin/bash
TIME=0
while :
do
  echo
  timeMessage="CURRENT TIME IS ${TIME}"
  echo $timeMessage
  echo
  echo Please choose your answer sets and external events occuring at the current time and change the facts in Facts.hs. 
  echo Please change the times in Facts.hs to the current time frame.
  echo Please change currentTime in CurrentTime.hs to the current time.
  echo Press press enter when you are done.
  read dummy
  runghc Resolution.hs
  echo
  echo Please replace the program in Program.hs with the resolved version in Haskell 
  echo Please change the current time and timeframe in program.lp to the current time frame
  echo Please add the facts/events in the current time to program.lp
  echo Please append the translated version of the resolved program in ASP to program.lp
  echo Press enter when you are done
  read dummy
  ./clingo program.lp 0
  echo
  ((TIME++))
done
