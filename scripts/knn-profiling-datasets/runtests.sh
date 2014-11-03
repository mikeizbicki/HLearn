#!/bin/bash

datasetdir="/home/user/proj/HLearn/datasets"

datasets_random="
    random/dataset-10000x20.csv
    "
datasets="
    uci/docword.kos.txt
    uci/docword.nips.txt
    "

echo "running tests in parallel"

for dataset in $datasets; do
    echo "$datasetdir/$dataset"
done | parallel ./runtest.sh {} $@
