#!/bin/bash

if [ -z "$1" ]; then
    echo "ERROR: $1 must be a directory" >&2
    exit 1
fi

###############################################################################
# one run only

if [ -z "$2" ]; then

    cd "$1"

    datasets=$( (
        for i in stdout-*; do
            cut -d'-' -f 3 <<< "$i"
        done
        ) | uniq )

# FIXME: pretty sure this is not handling floating point results properly
    function average() {
        awk 'NR == 1 { max=$1; min=$1; sum=0 }
           { if ($1>max) max=$1; if ($1<min) min=$1; sum+=$1;}
           END {printf "%f ", sum/NR}'
           #END {printf "Min: %d\tMax: %d\tAverage: %f\n", min, max, sum/NR}'
    }
    function sum() {
        awk 'NR == 1 { max=$1; min=$1; sum=0 }
           { if ($1>max) max=$1; if ($1<min) min=$1; sum+=$1;}
           END {printf "%f ", sum}'
           #END {printf "Min: %d\tMax: %d\tAverage: %f\n", min, max, sum/NR}'
    }

    function extractFromStdout() {
        grep -h reftree_prune $1  | grep $2 | grep -o "[[:digit:]]\+\.*[[:digit:]]*" | average
    }

    function extractFromProfiler() {
        grep -h "^  *$2" $1 | awk '{ print $4; }' | sum
    }

#######################################
# datafile-ave

    echo "" > datafile-ave
    for dataset in $datasets; do
        echo "dataset=$dataset"
        numdp=$(cut -d'x' -f1 <<< $dataset)
        numdim=$(cut -d'x' -f2 <<< $dataset)
        printf "$numdp $numdim " >> datafile-ave

        functionL="stMaxDepth stNumSingletons stNumLeaves stMaxLeaves stMaxChildren ctMovableNodes ctBetterMovableNodes"
        for function in $functionL; do
            extractFromStdout "stdout-dataset-${dataset}-*" $function >> datafile-ave
        done

        ( for file in prof-dataset-${dataset}-*; do
            extractFromProfiler $file l2_isFartherThanWithDistance
        done
        ) | average >> datafile-ave

        echo >> datafile-ave
    done
    sort -k2 -n datafile-ave > datafile-ave-sorted

#######################################
# datafile-all

    echo "" > datafile-all
    for file in stdout-*; do
        dataset=$(cut -d'-' -f 3 <<< "$file")
        extension="$(cut -d'-' -f 3 <<< $file)-$(cut -d'-' -f4 <<< $file)"
        echo "extension=$extension"
        numdp=$(cut -d'x' -f1 <<< $dataset)
        numdim=$(cut -d'x' -f2 <<< $dataset)
        printf "$numdp $numdim " >> datafile-all

        functionL="stMaxDepth stNumSingletons stNumLeaves stMaxLeaves stMaxChildren ctMovableNodes ctBetterMovableNodes"
        for function in $functionL; do
            extractFromStdout "stdout-dataset-$extension" $function >> datafile-all
        done

        extractFromProfiler "prof-dataset-$extension" l2_isFartherThanWithDistance >> datafile-all

        echo >> datafile-all
    done

#######################################
# gnuplot
    echo "
    set terminal postscript enhanced

    unset key
    set y2label 'number of distance comparisons'
    set y2tics
    set y2range [0:]
    set xtics nomirror
    set ytics nomirror
    set yrange [0:]
    f(x) = a*x + b

###################

    set output 'graphs-dim.ps'
    set xlabel 'dimension'

    set ylabel 'maximum fanout of a node' tc rgb '#ff0000'
    plot 'datafile-ave-sorted' u 2:10 lt 1 lw 4 lc rgb '#000000' w lines axes x1y2,\
         'datafile-ave-sorted' u 2:7 lt 1 lw 4 lc rgb '#ff0000' w lines

    set ylabel 'nodes that can be moved to a new parent (dashed => new parent is better)' tc rgb '#ff0000'
    plot 'datafile-ave-sorted' u 2:10 lt 1 lw 4 lc rgb '#000000' w lines axes x1y2,\
         'datafile-ave-sorted' u 2:8 lt 1 lw 4 lc rgb '#ff0000' w lines,\
         'datafile-ave-sorted' u 2:9 lt 2 lw 4 lc rgb '#ff0000' w lines

    set ylabel 'maximum tree depth' tc rgb '#ff0000'
    plot 'datafile-ave-sorted' u 2:10 lt 1 lw 4 lc rgb '#000000' w lines axes x1y2,\
         'datafile-ave-sorted' u 2:3 lt 1 lw 4 lc rgb '#ff0000' w lines

    set ylabel 'nodes with exactly one child' tc rgb '#ff0000'
    plot 'datafile-ave-sorted' u 2:10 lt 1 lw 4 lc rgb '#000000' w lines axes x1y2,\
         'datafile-ave-sorted' u 2:4 lt 1 lw 4 lc rgb '#ff0000' w lines

    unset y2tics
    unset y2label

    set xlabel 'number of dimensions'
    set ylabel 'number of distance comparisons'
    fit f(x) 'datafile-all' u 2:10  via a, b
    plot 'datafile-all' u 2:10 w points lt 1 lc rgb '#ff0000',\
         f(x) lt 1 lc rgb '#ff0000'

    set xlabel 'maximum fanout'
    fit f(x) 'datafile-all' u 7:10  via a, b
    plot 'datafile-all' u 7:10 w points lt 1 lc rgb '#ff0000',\
         f(x) lt 1 lc rgb '#ff0000'

    set xlabel 'maximum depth'
    fit f(x) 'datafile-all' u 3:10  via a, b
    plot 'datafile-all' u 3:10 w points lt 1 lc rgb '#ff0000',\
         f(x) lt 1 lc rgb '#ff0000'

###################

    set y2label 'number of distance comparisons'
    set y2tics
    set y2range [0:]

    set output 'graphs-numdp.ps'
    set xlabel 'number of datapoints'

    set ylabel 'maximum fanout of a node' tc rgb '#ff0000'
    plot 'datafile-ave-sorted' u 1:10 lt 1 lw 4 lc rgb '#000000' w lines axes x1y2,\
         'datafile-ave-sorted' u 1:7 lt 1 lw 4 lc rgb '#ff0000' w lines

    set ylabel 'nodes that can be moved to a new parent (dashed => new parent is better)' tc rgb '#ff0000'
    plot 'datafile-ave-sorted' u 1:10 lt 1 lw 4 lc rgb '#000000' w lines axes x1y2,\
         'datafile-ave-sorted' u 1:8 lt 1 lw 4 lc rgb '#ff0000' w lines,\
         'datafile-ave-sorted' u 1:9 lt 2 lw 4 lc rgb '#ff0000' w lines

    set ylabel 'maximum tree depth' tc rgb '#ff0000'
    plot 'datafile-ave-sorted' u 1:10 lt 1 lw 4 lc rgb '#000000' w lines axes x1y2,\
         'datafile-ave-sorted' u 1:3 lt 1 lw 4 lc rgb '#ff0000' w lines

    set ylabel 'nodes with exactly one child' tc rgb '#ff0000'
    plot 'datafile-ave-sorted' u 1:10 lt 1 lw 4 lc rgb '#000000' w lines axes x1y2,\
         'datafile-ave-sorted' u 1:4 lt 1 lw 4 lc rgb '#ff0000' w lines

#########

    unset y2tics
    unset y2label

    set xlabel 'number of datapoints'
    set ylabel 'number of distance comparisons'
    fit f(x) 'datafile-all' u 1:10  via a, b
    plot 'datafile-all' u 1:10 w points lt 1 lc rgb '#ff0000',\
         f(x) lt 1 lc rgb '#ff0000'

    set xlabel 'maximum fanout'
    fit f(x) 'datafile-all' u 7:10  via a, b
    plot 'datafile-all' u 7:10 w points lt 1 lc rgb '#ff0000',\
         f(x) lt 1 lc rgb '#ff0000'

    set xlabel 'maximum depth'
    fit f(x) 'datafile-all' u 3:10  via a, b
    plot 'datafile-all' u 3:10 w points lt 1 lc rgb '#ff0000',\
         f(x) lt 1 lc rgb '#ff0000'


    " > mkplots.gnu

    gnuplot mkplots.gnu
fi

###############################################################################
# two runs

if [ ! -z "$2" ]; then
    #./parsedata.sh "$1"
    #./parsedata.sh "$2"

    echo "
    set terminal postscript enhanced

    unset key
    set y2label 'number of distance comparisons'
    set y2tics
    set y2range [0:]
    set xrange [0:12]
    set xtics nomirror
    set ytics nomirror
    set yrange [0:]
    f(x) = a*x + b

    ###################

    set output 'graphs-dim.ps'
    set xlabel 'dimension'

    set ylabel 'maximum fanout of a node' tc rgb '#ff0000'
    plot '$1/datafile-ave-sorted' u 2:10 lt 1 lw 4 lc rgb '#000000' w lines axes x1y2,\
         '$2/datafile-ave-sorted' u 2:10 lt 2 lw 4 lc rgb '#000000' w lines axes x1y2,\
         '$1/datafile-ave-sorted' u 2:7 lt 1 lw 4 lc rgb '#ff0000' w lines,\
         '$2/datafile-ave-sorted' u 2:7 lt 2 lw 4 lc rgb '#ff0000' w lines

    set ylabel 'nodes that can be moved to a new parent (pink => new parent is better)' tc rgb '#ff0000'
    plot '$1/datafile-ave-sorted' u 2:10 lt 1 lw 4 lc rgb '#000000' w lines axes x1y2,\
         '$2/datafile-ave-sorted' u 2:10 lt 2 lw 4 lc rgb '#000000' w lines axes x1y2,\
         '$1/datafile-ave-sorted' u 2:8 lt 1 lw 4 lc rgb '#ff0000' w lines,\
         '$1/datafile-ave-sorted' u 2:9 lt 1 lw 4 lc rgb '#ff7777' w lines

    set ylabel 'maximum tree depth' tc rgb '#ff0000'
    plot '$1/datafile-ave-sorted' u 2:10 lt 1 lw 4 lc rgb '#000000' w lines axes x1y2,\
         '$2/datafile-ave-sorted' u 2:10 lt 2 lw 4 lc rgb '#000000' w lines axes x1y2,\
         '$1/datafile-ave-sorted' u 2:3 lt 1 lw 4 lc rgb '#ff0000' w lines,\
         '$2/datafile-ave-sorted' u 2:3 lt 2 lw 4 lc rgb '#ff0000' w lines

    set ylabel 'nodes with exactly one child' tc rgb '#ff0000'
    plot '$1/datafile-ave-sorted' u 2:10 lt 1 lw 4 lc rgb '#000000' w lines axes x1y2,\
         '$2/datafile-ave-sorted' u 2:10 lt 2 lw 4 lc rgb '#000000' w lines axes x1y2,\
         '$1/datafile-ave-sorted' u 2:4 lt 1 lw 4 lc rgb '#ff0000' w lines,\
         '$2/datafile-ave-sorted' u 2:4 lt 2 lw 4 lc rgb '#ff0000' w lines

    " > mkplots.gnu
    gnuplot mkplots.gnu
fi
