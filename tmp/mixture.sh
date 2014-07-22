#!/bin/bash

cmd=$(cat <<EOF
set terminal postscript "Times-Roman" 18 
set output "mixture.ps" 

set xtics nomirror out
set xrange [-5:5]
set yrange [0:1]

set ylabel "error rate"
set xlabel "formula ratio (x)"

EOF
)

tests="
ion,0.1,L2
ion,0.0001,L2
haberman,0.1,L2
wine,0.1,L2
wine,0.0001,L2
wine,0.0001,L1
wdbc,0.1,L2
wpbc,0.0001,L2
sonar,0.1,L2
sonar,0.0001,L2
"

#for i in ion,0.1  ion,0.0001  haberman,0.1  wine,0.1  wdbc,0.1  wpbc,0.0001; do
for i in $tests; do
    IFS=","
    set $i
    dataset="$1"
    reg="$3"
    #reg="L2"
    #[ "$3"=="L1" ] && reg="L1"
    lambdastr=""
    [ ! $2 == "0.1" ] && lambdastr="--regtype=$reg --regamount=$2 " 

plot=$(cat <<EOF

dataset="$1"
set title dataset."      $reg $2"
#plot "mixture/".dataset." --paramseed=0 --cvfolds=2 --cvreps=10 --monoidsplits=2 $lambdastr--monoidtype=MixtureAveTaylor" using 9:31 title "(1-x)*Average + x*Taylor" lc 1 lt 1 with lines,\
     #"mixture/".dataset." --paramseed=0 --cvfolds=2 --cvreps=10 --monoidsplits=2 $lambdastr--monoidtype=MixtureAveUpper" using 9:31 title "(1-x)*Average + x*Upper" lc 2 lt 1 with lines,\
     #"mixture/".dataset." --paramseed=0 --cvfolds=2 --cvreps=10 --monoidsplits=2 $lambdastr--monoidtype=MixtureUpperTaylor" using 9:31 title "(1-x)*Upper + x*Taylor" lc 3 lt 1 with lines,\
     #"mixture/".dataset." --paramseed=0 --cvfolds=2 --cvreps=10 --monoidsplits=10 $lambdastr--monoidtype=MixtureAveTaylor" using 9:31 notitle lc 1 lt 2 with lines,\
     #"mixture/".dataset." --paramseed=0 --cvfolds=2 --cvreps=10 --monoidsplits=10 $lambdastr--monoidtype=MixtureAveUpper" using 9:31 notitle lc 2 lt 2 with lines,\
     #"mixture/".dataset." --paramseed=0 --cvfolds=2 --cvreps=10 --monoidsplits=10 $lambdastr--monoidtype=MixtureUpperTaylor" using 9:31 notitle lc 3 lt 2 with lines

plot "mixture/$dataset --paramseed=0 --cvfolds=2 --cvreps=10 --monoidsplits=10 $lambdastr--monoidtype=MixtureAveTaylor" using 9:31 title "(1-x)*Average + x*Taylor" lc 1 lt 1 with lines,\
     "mixture/$dataset --paramseed=0 --cvfolds=2 --cvreps=10 --monoidsplits=10 $lambdastr--monoidtype=MixtureAveUpper" using 9:31 title "(1-x)*Average + x*Upper" lc 2 lt 1 with lines,\
     "mixture/$dataset --paramseed=0 --cvfolds=2 --cvreps=10 --monoidsplits=10 $lambdastr--monoidtype=MixtureUpperTaylor" using 9:31 title "(1-x)*Upper + x*Taylor" lc 3 lt 1 with lines
EOF
)

cmd="$cmd$plot"
done

echo "generating plot"
gnuplot <<< "$cmd"
