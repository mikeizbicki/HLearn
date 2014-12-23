#!/bin/bash

K=1

curdir=$(pwd)
tmpdir="$(basename $1)-$(date +%Y-%m-%d--%H-%M-%S)"
cd $(dirname $0)
mkdir "$tmpdir"
cd "$tmpdir"

echo "--------------------------------------------------------------------------------"
echo "tempdir=$0/$tmpdir"
echo "--------------------------------------------------------------------------------"

hlearn_neighbors="./neighbors_hlearn.csv"
hlearn_distances="./distances_hlearn.csv"

#verbose="--verbose"
optimization="--varshift"
#method="--train-method=traininsert"

methodlist="trainmonoid traininsert_nosort traininsert_sort traininsert_parent traininsert_ancestor"

touch results

for method in $methodlist; do
    echo -e "---\n$method\n---\n"
    "$curdir/hlearn-allknn" --train-method="$method" -k $K -r "$curdir/$1" $optimization $verbose +RTS -K1000M -N1 -p -s 2>&1 | tee out.$method

    mv ./hlearn-allknn.prof ./prof.$method


    searchdistance=$(sed -n -e '/^ findEpsilon/,/^ [^ ]/ p' prof.$method | grep l2_ | awk '{ print $4 }' | awk '{s+=$1} END {print s}')
    traindistance=$(sed -n -e '/^ train/,/^ [^ ]/ p' prof.$method | grep l2_ | awk '{ print $4 }' | awk '{s+=$1} END {print s}')
    totaldistance=$(($searchdistance+$traindistance))

    echo "$method        $traindistance        $searchdistance        $totaldistance" >> results
done

echo
echo "--------------------------------------------------------------------------------"
echo "tempdir=$0/$tmpdir"
echo
cat results
echo "--------------------------------------------------------------------------------"
