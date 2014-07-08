#!/bin/bash

K=1

#time ./hlearn-allknn -k $K -r $1 -v +RTS -K1000M
time ./hlearn-allknn -k $K -r $1 -v -n "tmp/neighbors_hlearn.csv" --distances-file="tmp/distances_hlearn.csv" +RTS -K1000M -N4
#time ./hlearn-allknn-mapAccumL -k $K -r $1 -v +RTS -K1000M -N4

#time range_search --max 100 -r $1 -n neighbors_mlpack.csv -d distances_mlpack.csv -v 

#time allknn -r $1 -n neighbors_mlpack.csv -d distances_mlpack.csv -k $K -v -S
time allknn -r $1 -n "tmp/neighbors_mlpack.csv" -d "tmp/distances_mlpack.csv" -k $K -v
#time allknn -r $1 -n neighbors_mlpack.csv -d distances_mlpack.csv -k $K -v -c -S
#time allknn -r $1 -n neighbors_mlpack.csv -d distances_mlpack.csv -k $K -v -c
#time allknn -r $1 -n neighbors_mlpack.csv -d distances_mlpack.csv -k $K -v -N

echo "-------------------------------------"

#head neighbors_hlearn.csv 
#echo "---" 
#head neighbors_mlpack.csv

echo "num differences: " `diff tmp/neighbors_hlearn.csv tmp/neighbors_mlpack.csv | wc -l` " / " `cat $1 | wc -l`
