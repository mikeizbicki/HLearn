This directory contains all the details you need to reproduce HLearn outperforming Weka.

First, you must get the appropriate version of the HLearn library from hackage.  You will need GHC >= 7.6.

> cabal install HLearn-classification-1.0.0

Next, compile the hlearn-cv.lhs file.  

> ghc -O2 hlearn-cv.lhs

This file performs the experiments for HLearn, and the weka-cv.sh file performs the experiments for Weka.  You may need to adjust some of the settings inside the weka-cv.sh file to point to the correct install locations of Java and Weka for your system.

Finally, run the timing.sh file.  The run times will spit out to the screen.  Easy peasy.

> ./timings.sh

The full blown leave-one-out cross-validation for weka is commented out because it takes so long.  Uncomment only if you're feeling really bored!