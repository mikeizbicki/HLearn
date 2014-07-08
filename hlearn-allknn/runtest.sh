#!/bin/bash

OUTPUT=$2 
INPUT=$1 
LOGFILE="$2-log-$(echo $INPUT | tr / _)"

TIME="/usr/bin/time -f %E "

echo "testing file $INPUT"
echo -n "$INPUT " >> $OUTPUT

for cmd in \
    "./hlearn-allknn -r $INPUT -k 1 -v -n tmp/neighbors_hlearn --distances-file=tmp/distances_hlearn +RTS -K1000M -N4" \
    "./hlearn-allknn -r $INPUT -k 1 -v -n tmp/neighbors_hlearn --distances-file=tmp/distances_hlearn +RTS -K1000M -N3" \
    "./hlearn-allknn -r $INPUT -k 1 -v -n tmp/neighbors_hlearn --distances-file=tmp/distances_hlearn +RTS -K1000M -N2" \
    "./hlearn-allknn -r $INPUT -k 1 -v -n tmp/neighbors_hlearn --distances-file=tmp/distances_hlearn +RTS -K1000M -N1" \
    "allknn -r $INPUT -k 1 -v -S -n tmp/neighbors_mlpack.csv -d tmp/distances_mlpack.csv " \
    "allknn -r $INPUT -k 1 -v -n tmp/neighbors_mlpack.csv -d tmp/distances_mlpack.csv " \
    "allknn -r $INPUT -k 1 -v -c -S -n tmp/neighbors_mlpack.csv -d tmp/distances_mlpack.csv " \
    "allknn -r $INPUT -k 1 -v -c -n tmp/neighbors_mlpack.csv -d tmp/distances_mlpack.csv "
    #"allknn -r $INPUT -k 1 -v -N -n tmp/neighbors_mlpack.csv -d tmp/distances_mlpack.csv " 
do
    echo -n "$cmd"
    echo "" >> "$LOGFILE"
    echo "---------------------------------------------------------------------" >> $LOGFILE
    echo "$cmd" >> "$LOGFILE"
    echo "---------------------------------------------------------------------" >> $LOGFILE
    
    restime=$( $TIME $cmd 2>&1 >> $LOGFILE )
    echo -n " = $restime "
    echo -n " $restime " >> $OUTPUT

    ressec=$(bin/time2sec $restime)
    echo " = $ressec seconds"
    echo -n " $ressec " >> $OUTPUT
done

echo "" >> $OUTPUT
