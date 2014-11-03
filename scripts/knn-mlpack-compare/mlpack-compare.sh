#!/bin/bash

K=2

hlearn_neighbors="tmp/neighbors_hlearn.csv"
hlearn_distances="tmp/neighbors_hlearn.csv"
mlpack_neighbors="tmp/neighbors_mlpack.csv"
mlpack_distances="tmp/distances_mlpack.csv"

verbose="--verbose"
optimization="--varshift"
#monoid="--train-monoid"

time ./hlearn-allknn -k $K -r $1 $optimization $monoid $verbose -n "$hlearn_neighbors" --distances-file="$hlearn_distances" +RTS -K1000M # -N4

#time allknn -r $1 -n $mlpack_neighbors -d $mlpack_distances -k $K -v -S
time allknn -r $1 -n $mlpack_neighbors -d $mlpack_distances -k $K -v
#time allknn -r $1 -n $mlpack_neighbors -d $mlpack_distances -k $K -v -c -S
#time allknn -r $1 -n $mlpack_neighbors -d $mlpack_distances -k $K -v -c
#time allknn -r $1 -n $mlpack_neighbors -d $mlpack_distances -k $K -v -N

echo "-------------------------------------"
#head neighbors_hlearn.csv
#echo "---"
#head neighbors_mlpack.csv
echo "num differences: " `diff $hlearn_neighbors $mlpack_neighbors | wc -l` " / " `cat $1 | wc -l`
