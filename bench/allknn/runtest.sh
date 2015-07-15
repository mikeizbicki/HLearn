#!/bin/bash

################################################################################
# initialize

# the number of neighbors to find
# NOTE:
# For many libraries, we must add one to the value of k.
# This is because these implementations do not exclude the node being searched when finding neighbors.
k=1

# maximum amount of time to let a program run
timeout="5h"

#######################################
# you probably don't want to modify the following variables

# the file we're doing knn query on
file="$(readlink -f $1)"

# the directory this script is located in
scriptdir="$(readlink -f $(dirname $0))"

# a temporary directory to put the output of all the programs
tmpdir="$scriptdir/results/$(basename $file)-$(date +%Y-%m-%d--%H-%M-%S)"
mkdir -p "$tmpdir"
cd "$tmpdir"

################################################################################
# This section defines bash arrays containing the proper invocation for performing nearest neighbor search in each of the libraries to compare.

# hlearn using only default parameters
cmds_hlearn=(
    "hlearn-allknn -r $file -k $k +RTS -N1"
    #"hlearn-allknn -r $file -k $k --fold=fold --rotate=variance +RTS -N1"
    #"hlearn-allknn -r $file -k $k --train-method=simplified --fold=foldsort --rotate=variance +RTS -N1"
    #"hlearn-allknn -r $file -k $k --train-method=simplified --fold=fold --rotate=variance +RTS -N1"
    #"hlearn-allknn -r $file -k $k --train-method=trainmonoid --fold=foldsort --rotate=variance +RTS -N1"
    #"hlearn-allknn -r $file -k $k --train-method=trainmonoid --fold=fold --rotate=variance +RTS -N1"
)

# the reference cover tree implementation
cmds_ref=(
    "$scriptdir/cover_tree/knn $[$k+1] $file $file dual"
    "$scriptdir/cover_tree/knn $[$k+1] $file $file single"
)

# mlpack
cmds_mlpack=(
    "allknn -r $file -k $k -n neighbors_mlpack.csv    -d distances_mlpack.csv       "
    "allknn -r $file -k $k -n neighbors_mlpack.S.csv  -d distances_mlpack.S.csv  -S "
    "allknn -r $file -k $k -n neighbors_mlpack.cS.csv -d distances_mlpack.cS.csv -cS"
    "allknn -r $file -k $k -n neighbors_mlpack.c.csv  -d distances_mlpack.c.csv  -c "
)

# R's fastknn package
cmds_R=(
    "$scriptdir/allknn.R $file $k cover_tree"
    "$scriptdir/allknn.R $file $k kd_tree "
)

# scikit learn
cmds_scikit=(
    "$scriptdir/allknn.scikit $file $[$k+1] ball_tree"
    "$scriptdir/allknn.scikit $file $[$k+1] kd_tree "
)

# julia
cmds_julia=(
    "$scriptdir/allknn.julia $file $[$k+1]"
)

# flann
cmds_flann=(
    "$scriptdir/allknn.flann $file $[$k+1]"
)

################################################################################
# execute the commands

cmds=(
    #"${cmds_hlearn[@]}"
    #"${cmds_ref[@]}"
    #"${cmds_mlpack[@]}"
    #"${cmds_R[@]}"
    #"${cmds_scikit[@]}"
    #"${cmds_julia[@]}"
    "${cmds_flann[@]}"
    )

for cmd in "${cmds[@]}"; do
    printf "$(sed 's;/[^ ]*/;;g' <<< $cmd)          "
    sanitizedcmd=$((sed 's;/[^ ]*/;;g' | tr ' /' '__') <<< "$cmd")

    TIME="/usr/bin/time -f %E timeout $timeout "
    runtime=$($TIME $cmd 2>&1 >> "stdout.$sanitizedcmd")
    echo "$runtime" >> "stderr.$sanitizedcmd"
    runtime=$(tail -1 <<< "$runtime")
    runseconds=$(../../time2sec.hs $runtime)

    echo "$runtime       $runseconds"

    #cat "stdout.$sanitizedcmd"
    #cat "stderr.$sanitizedcmd"

    echo "$sanitizedcmd          $runtime        $runseconds" >> results
done
