This directory contains the details you need to reproduce HLearn outperforming Weka at cross-validation.

First, you must get the appropriate version of the HLearn library from hackage.  You will need GHC >= 7.6.

> cabal install HLearn-2.0.0
> cabal install cassava

Next, compile the hlearn-cv.lhs file.  

> ghc -O2 hlearn-cv.lhs

This file performs the experiments for HLearn, and the weka-cv.sh file performs the experiments for Weka.  You may need to adjust some of the settings inside the weka-cv.sh file to point to the correct install locations of Java and Weka for your system.

The files ending in .csv or .arff are the dataset files.  It's easier for HLearn to parse the CSV, but Weka like's Arff, so all the Arffs were created from the CSVs using the csv2arff.sh tool.  The full data set is in adult.csv, and the adult-X.csv files contain only the first X entries.  These are used for testing the scalability of leave-one-out cross-validation.

Finally, run the timing.sh file.  The run times will spit out to the screen.  Easy peasy.

> ./timings.sh

The full blown leave-one-out cross-validation for weka is commented out because it takes so long.  Uncomment only if you're feeling really bored!
