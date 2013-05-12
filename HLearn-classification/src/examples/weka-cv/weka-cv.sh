#!/bin/bash

#
# This file was modified from Weka's launch script.
#
# Usage: weka-cv.sh csvfile numberfolds
#

FILE_CSV=$1
FOLDS=$2

FILE_ARFF="$FILE_CSV.arff"
OUTFILE="$FILE_ARFF.$FOLDS"


. /usr/lib/java-wrappers/java-wrappers.sh

find_java_runtime openjdk6 sun6 || \
	( echo "$0: Java not found, aborting." >&2 && exit 1 )
find_jars weka.jar

# echo "Converting CSV"
# run_java weka.core.converters.CSVLoader $FILE_CSV > $FILE_ARFF
echo "Cross-validating $FILE_ARFF with $FOLDS folds"
run_java weka.classifiers.bayes.NaiveBayes -t $FILE_ARFF -x $FOLDS -c 15 -o > $OUTFILE
echo "Done."