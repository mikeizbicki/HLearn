#!/bin/bash

hlearndir="../.."
#numdpL="10000 15000 20000 25000 30000 35000 40000 45000 50000 60000 70000 80000 90000 100000"
numdpL="10000"
#dimL="10"
dimL="1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20"

resdir="results-$(date +%Y-%m-%d--%H-%M-%S)"
echo "resdir=$resdir"
mkdir "$resdir"

for numdp in $numdpL; do
    for dim in $dimL; do
        for test in a b c d e f g h i j; do
            dataset="dataset-${numdp}x$dim-$test.csv"
            echo "running test $dataset"
            $hlearndir/datasets/random/DataGenerator $numdp $dim > $hlearndir/datasets/random/$dataset
            $hlearndir/dist/build/hlearn-allknn/hlearn-allknn -r $hlearndir/datasets/random/$dataset --verbose +RTS -p > $resdir/stdout-$dataset 2> $resdir/stderr-$dataset
            mv hlearn-allknn.prof "$resdir/prof-$dataset"
        done
    done
done
