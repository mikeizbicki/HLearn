#!/bin/bash

K=1

tmpdir=$(mktemp --tmpdir -d)

echo "--------------------------------------------------------------------------------"
echo "tempdir=$tmpdir"
echo "--------------------------------------------------------------------------------"
hlearn_neighbors="$tmpdir/neighbors_hlearn.csv"
hlearn_distances="$tmpdir/distances_hlearn.csv"

hlearn-allknn -k $K -r $@ -n "$hlearn_neighbors" -d "$hlearn_distances"

echo "-------------------------------------"
echo "num differences: " `diff "$hlearn_neighbors" "$1-neighbors" | wc -l` " / " `cat $1 | wc -l`
diff "$hlearn_neighbors" "$1-neighbors" > /dev/null
