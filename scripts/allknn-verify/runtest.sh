#!/bin/bash

K=1

#tmpdir=$(mktemp --tmpdir -d)
tmpdir="."
echo "--------------------------------------------------------------------------------"
echo "tempdir=$tmpdir"
echo "--------------------------------------------------------------------------------"
hlearn_neighbors="$tmpdir/neighbors_hlearn.csv"
hlearn_distances="$tmpdir/distances_hlearn.csv"
mlpack_neighbors="$tmpdir/neighbors_mlpack.csv"
mlpack_distances="$tmpdir/distances_mlpack.csv"

#verbose="--verbose"
optimization="--varshift"
#method="--train-method=traininsert_ancestor"
#method="--train-method=traininsert_orig"

time hlearn-allknn -k $K -r $@ $optimization $method $verbose -n "$hlearn_neighbors" --distances-file="$hlearn_distances" +RTS -K1000M -N1
#time hlearn-allknn -k $K -r $1 $optimization $method $verbose -n "$hlearn_neighbors" --distances-file="$hlearn_distances" +RTS -K1000M -N

time allknn -r $1 -n $mlpack_neighbors -d $mlpack_distances -k $K -v -S
#time allknn -r $1 -n "$mlpack_neighbors" -d "$mlpack_distances" -k $K -v
#time allknn -r $1 -n $mlpack_neighbors -d $mlpack_distances -k $K -v -c -S
#time allknn -r $1 -n $mlpack_neighbors -d $mlpack_distances -k $K -v -c

#time allknn -r $1 -n $mlpack_neighbors -d $mlpack_distances -k $K -v -N

echo "-------------------------------------"
#head neighbors_hlearn.csv
#echo "---"
#head neighbors_mlpack.csv
echo "num differences: " `diff $hlearn_neighbors $mlpack_neighbors | wc -l` " / " `cat $1 | wc -l`
diff $hlearn_neighbors $mlpack_neighbors | head
