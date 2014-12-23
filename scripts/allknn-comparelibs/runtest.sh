#!/bin/bash


curdir=$(pwd)
tmpdir="$(basename $1)-$(date +%Y-%m-%d--%H-%M-%S)"
cd $(dirname $0)
mkdir "$tmpdir"
cd "$tmpdir"

INPUT="$curdir/$1"

TIME="/usr/bin/time -f %E "

for cmd in \
    "$curdir/hlearn-allknn --train-method=traininsert_nosort -r $INPUT -k 1 -n neighbors_hlearn.1.csv --distances-file=distances_hlearn.1.csv +RTS -K1000M -N4" \
    "$curdir/hlearn-allknn --train-method=traininsert_nosort -r $INPUT -k 1 -n neighbors_hlearn.2.csv --distances-file=distances_hlearn.2.csv +RTS -K1000M -N3" \
    "$curdir/hlearn-allknn --train-method=traininsert_nosort -r $INPUT -k 1 -n neighbors_hlearn.3.csv --distances-file=distances_hlearn.3.csv +RTS -K1000M -N2" \
    "$curdir/hlearn-allknn --train-method=traininsert_nosort -r $INPUT -k 1 -n neighbors_hlearn.4.csv --distances-file=distances_hlearn.4.csv +RTS -K1000M -N1" \
    "allknn -r $INPUT -k 1 -n neighbors_mlpack.1.csv -d distances_mlpack.1.csv -S " \
    "allknn -r $INPUT -k 1 -n neighbors_mlpack.2.csv -d distances_mlpack.2.csv " \
    "allknn -r $INPUT -k 1 -n neighbors_mlpack.3.csv -d distances_mlpack.3.csv -c S" \
    "allknn -r $INPUT -k 1 -n neighbors_mlpack.4.csv -d distances_mlpack.4.csv -c " \
    "../allknn.R $INPUT 1 cover_tree" \
    "../allknn.R $INPUT 1 kd_tree "
    #"allknn -r $INPUT -k 1 -v -N -n tmp/neighbors_mlpack.csv -d tmp/distances_mlpack.csv "
do
    echo "$cmd"
    sanitizedcmd=$(tr ' /' '__' <<< "$cmd")
    runtime=$($TIME $cmd 2>&1 >> "stdout.$sanitizedcmd")
    runtime=$(tail -1 <<< "$runtime")
    runseconds=$(../time2sec.hs $runtime)

    echo runtime=$runtime
    echo runseconds=$runseconds

    echo "$sanitizedcmd          $runtime        $runseconds" >> results
done

echo "--------------------------------------------------------------------------------"
echo "  results:"
echo "--------------------------------------------------------------------------------"
cat results
