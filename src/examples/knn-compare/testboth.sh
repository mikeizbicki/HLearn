#!/bin/bash

time ./KNN -r $1 -v +RTS -N4 -K100M
#time allknn -r $1 -n neighbors_mlpack.csv -d distances_mlpack.csv -k 1 -v
time allknn -r $1 -n neighbors_mlpack.csv -d distances_mlpack.csv -k 1 -vc

echo "-------------------------------------"

#head neighbors_hlearn.csv 
#echo "---" 
#head neighbors_mlpack.csv

#diff neighbors_hlearn.csv neighbors_mlpack.csv
