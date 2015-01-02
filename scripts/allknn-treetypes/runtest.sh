#!/bin/bash

if [ -z "$1" ]; then
    echo "usage: $0 dataset [optionalparams]"
    echo "the dataset param is required; all other params get passed directly to hlearn-allknn"
    exit -1
fi

distanceName="l2_"
#distanceName="emd_"

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
    "$curdir/hlearn-allknn" --train-method="$method" -k $K -r "$curdir/$1" $2 $3 $4 $5 $optimization $verbose +RTS -K1000M -N2 -p -s 2>&1 | tee out.$method

    mv ./hlearn-allknn.prof ./prof.$method


    searchdistance=$(sed -n -e '/^ findEpsilon/,/^ [^ ]/ p' prof.$method | grep "$distanceName" | awk '{ print $4 }' | awk '{s+=$1} END {print s}')
    traindistance=$(sed -n -e '/^ train/,/^ [^ ]/ p' prof.$method | grep "$distanceName" | awk '{ print $4 }' | awk '{s+=$1} END {print s}')
    echo "searchdistance=$searchdistance"
    echo "traindistance=$traindistance"
    totaldistance=$(($searchdistance+$traindistance))

    echo "$method        $traindistance        $searchdistance        $totaldistance"
    echo "$method        $traindistance        $searchdistance        $totaldistance" >> results
done

echo
echo "--------------------------------------------------------------------------------"
echo "tempdir=$0/$tmpdir"
echo
cat results
echo "--------------------------------------------------------------------------------"
