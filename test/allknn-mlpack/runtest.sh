#!/bin/bash

K=1

tmpdir=$(mktemp --tmpdir -d)

echo "--------------------------------------------------------------------------------"
echo "tempdir=$tmpdir"
echo "--------------------------------------------------------------------------------"
hlearn_neighbors="$tmpdir/neighbors_hlearn.csv"
hlearn_distances="$tmpdir/distances_hlearn.csv"
mlpack_neighbors="$tmpdir/neighbors_mlpack.csv"
mlpack_distances="$tmpdir/distances_mlpack.csv"

hlearn-allknn -k $K -r $@ -n "$hlearn_neighbors" -d "$hlearn_distances" +RTS -K1000M -N1
allknn -r $1 -n "$mlpack_neighbors" -d "$mlpack_distances" -k $K -v

echo "-------------------------------------"
echo "num differences: " `diff $hlearn_neighbors $mlpack_neighbors | wc -l` " / " `cat $1 | wc -l`
diff $hlearn_neighbors $mlpack_neighbors > /dev/null
