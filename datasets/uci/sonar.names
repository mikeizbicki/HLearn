NAME: Sonar, Mines vs. Rocks

SUMMARY: This is the data set used by Gorman and Sejnowski in their study
of the classification of sonar signals using a neural network [1].  The
task is to train a network to discriminate between sonar signals bounced
off a metal cylinder and those bounced off a roughly cylindrical rock.

SOURCE: The data set was contributed to the benchmark collection by Terry
Sejnowski, now at the Salk Institute and the University of California at
San Deigo.  The data set was developed in collaboration with R. Paul
Gorman of Allied-Signal Aerospace Technology Center.

MAINTAINER: Scott E. Fahlman

PROBLEM DESCRIPTION:

The file "sonar.mines" contains 111 patterns obtained by bouncing sonar
signals off a metal cylinder at various angles and under various
conditions.  The file "sonar.rocks" contains 97 patterns obtained from
rocks under similar conditions.  The transmitted sonar signal is a
frequency-modulated chirp, rising in frequency.  The data set contains
signals obtained from a variety of different aspect angles, spanning 90
degrees for the cylinder and 180 degrees for the rock.

Each pattern is a set of 60 numbers in the range 0.0 to 1.0.  Each number
represents the energy within a particular frequency band, integrated over
a certain period of time.  The integration aperture for higher frequencies
occur later in time, since these frequencies are transmitted later during
the chirp.

The label associated with each record contains the letter "R" if the object
is a rock and "M" if it is a mine (metal cylinder).  The numbers in the
labels are in increasing order of aspect angle, but they do not encode the
angle directly.

METHODOLOGY: 

This data set can be used in a number of different ways to test learning
speed, quality of ultimate learning, ability to generalize, or combinations
of these factors.

In [1], Gorman and Sejnowski report two series of experiments: an
"aspect-angle independent" series, in which the whole data set is used
without controlling for aspect angle, and an "aspect-angle dependent"
series in which the training and testing sets were carefully controlled to
ensure that each set contained cases from each aspect angle in
appropriate proportions.

For the aspect-angle independent experiments the combined set of 208 cases
is divided randomly into 13 disjoint sets with 16 cases in each.  For each
experiment, 12 of these sets are used as training data, while the 13th is
reserved for testing.  The experiment is repeated 13 times so that every
case appears once as part of a test set.  The reported performance is an
average over the entire set of 13 different test sets, each run 10 times.

It was observed that this random division of the sample set led to rather
uneven performance.  A few of the splits gave poor results, presumably
because the test set contains some samples from aspect angles that are
under-represented in the corresponding training set.  This motivated Gorman
and Sejnowski to devise a different set of experiments in which an attempt
was made to balance the training and test sets so that each would have a
representative number of samples from all aspect angles.  Since detailed
aspect angle information was not present in the data base of samples, the
208 samples were first divided into clusters, using a 60-dimensional
Euclidian metric; each of these clusters was then divided between the
104-member training set and the 104-member test set.  

The actual training and testing samples used for the "aspect angle
dependent" experiments are marked in the data files.  The reported
performance is an average over 10 runs with this single division of the
data set.

A standard back-propagation network was used for all experiments.  The
network had 60 inputs and 2 output units, one indicating a cylinder and the
other a rock.  Experiments were run with no hidden units (direct
connections from each input to each output) and with a single hidden layer
with 2, 3, 6, 12, or 24 units.  Each network was trained by 300 epochs over
the entire training set.

The weight-update formulas used in this study were slightly different from
the standard form.  A learning rate of 2.0 and momentum of 0.0 was used.
Errors less than 0.2 were treated as zero.  Initial weights were uniform
random values in the range -0.3 to +0.3.

RESULTS: 

For the angle independent experiments, Gorman and Sejnowski report the
following results for networks with different numbers of hidden units:

Hidden	% Right on	Std.	% Right on	Std.
Units	Training set	Dev.	Test Set	Dev.
------	------------	----	----------	----
0	89.4		2.1	77.1		8.3
2	96.5		0.7	81.9		6.2
3	98.8		0.4	82.0		7.3
6	99.7		0.2	83.5		5.6
12	99.8		0.1	84.7		5.7
24	99.8		0.1	84.5		5.7

For the angle-dependent experiments Gorman and Sejnowski report the
following results:

Hidden	% Right on	Std.	% Right on	Std.
Units	Training set	Dev.	Test Set	Dev.
------	------------	----	----------	----
0	79.3		3.4	73.1		4.8
2	96.2		2.2	85.7		6.3
3	98.1		1.5	87.6		3.0
6	99.4		0.9	89.3		2.4
12	99.8		0.6	90.4		1.8
24     100.0		0.0	89.2		1.4

Not surprisingly, the network's performance on the test set was somewhat
better when the aspect angles in the training and test sets were balanced.

Gorman and Sejnowski further report that a nearest neighbor classifier on
the same data gave an 82.7% probability of correct classification.

Three trained human subjects were each tested on 100 signals, chosen at
random from the set of 208 returns used to create this data set.  Their
responses ranged between 88% and 97% correct.  However, they may have been
using information from the raw sonar signal that is not preserved in the
processed data sets presented here.

REFERENCES: 

1. Gorman, R. P., and Sejnowski, T. J. (1988).  "Analysis of Hidden Units
in a Layered Network Trained to Classify Sonar Targets" in Neural Networks,
Vol. 1, pp. 75-89.
