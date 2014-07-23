#!/bin/bash

cmd=$(cat <<EOF
set terminal postscript "Times-Roman" 18 
set output "numdp.ps" 

set xtics nomirror out
set xrange [0:1000]
#set yrange [0:1]

set ylabel "error rate"
set xlabel "formula ratio (x)"

EOF
)

#ion,0.0001,L2
#ion,0.0001,L1
tests="
wine,0.0001,L2
wine,0.0001,L1
wdbc,0.0001,L2
wdbc,0.0001,L1
wpbc,0.0001,L2
wpbc,0.0001,L1
sonar,0.0001,L2
sonar,0.0001,L1
"
#wine,0.0001,L1
#wdbc,0.1,L2
#wpbc,0.0001,L2
#sonar,0.1,L2
#sonar,0.0001,L2
#"

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

set title "$dataset      $reg $2"
plot "numdp/$dataset --paramseed=0 --cvfolds=2 --cvreps=10 $lambdastr--monoidsplits=2 --monoidtype=MappendAverage" using 11:31 title "Average" lc 1 lt 1 with lines,\
     "numdp/$dataset --paramseed=0 --cvfolds=2 --cvreps=10 $lambdastr--monoidsplits=2 --monoidtype=MappendTaylor" using 11:31 title "Taylor" lc 2 lt 1 with lines,\
     "numdp/$dataset --paramseed=0 --cvfolds=2 --cvreps=10 $lambdastr--monoidsplits=2 --monoidtype=MappendUpperBound" using 11:31 title "UpperBound" lc 3 lt 1 with lines,\
     "numdp/$dataset --paramseed=0 --cvfolds=2 --cvreps=10 $lambdastr--monoidsplits=1" using 11:31 title "nosplit" lc 0 lt 1 with lines
EOF
)

cmd="$cmd$plot"
done

echo "generating plot"
gnuplot <<< "$cmd"
