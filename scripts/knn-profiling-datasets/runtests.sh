#!/bin/bash

datasetdir="/home/user/proj/hlearn/datasets"

datasets_random="
    random/dataset-10000x20.csv
    "
datasets_docword="
    uci/docword.kos.txt
    uci/docword.nips.txt
    uci/docword.enron.txt
    uci/docword.nytimes.txt
    uci/vocab.kos.txt
    uci/vocab.nips.txt
    uci/vocab.enron.txt
    uci/vocab.nytimes.txt
    "

datasets="
    random/dataset-10000x2.csv
    "

echo "running tests in parallel"

for dataset in $datasets; do
    for sigma in 1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000; do
        echo "--sigma2-rbf=$sigma"
    done | parallel -P 2 ./runtest.sh "$datasetdir/$dataset" {}
done

#for exprat in 1.1 1.2 1.25 1.3 1.35 1.4 1.5 1.6 2.0 4.0; do
    #echo "--expansionratio=$exprat"
#for exprat in 1 2 3 4 5 6 7 8 9 10; do
    #echo "--searchepsilon=$exprat"
#done | parallel -P 2 ./runtest.sh "$datasetdir/uci/vocab.enron.txt" {} $@
