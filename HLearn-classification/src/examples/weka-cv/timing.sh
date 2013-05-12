#!/bin/bash

time ./hlearn-cv "adult.csv" 10
time ./hlearn-cv "adult.csv" 20
time ./hlearn-cv "adult.csv" 30
time ./hlearn-cv "adult.csv" 40
time ./hlearn-cv "adult.csv" 50
time ./hlearn-cv "adult.csv" 60
time ./hlearn-cv "adult.csv" 70
time ./hlearn-cv "adult.csv" 80
time ./hlearn-cv "adult.csv" 90
time ./hlearn-cv "adult.csv" 100

time ./hlearn-cv "adult-1000.csv" 1000
time ./hlearn-cv "adult-2000.csv" 2000
time ./hlearn-cv "adult-3000.csv" 3000
time ./hlearn-cv "adult-4000.csv" 4000
time ./hlearn-cv "adult-5000.csv" 5000
time ./hlearn-cv "adult-6000.csv" 6000
time ./hlearn-cv "adult-7000.csv" 7000
time ./hlearn-cv "adult-8000.csv" 8000
time ./hlearn-cv "adult-9000.csv" 9000
time ./hlearn-cv "adult-10000.csv" 10000

time ./weka-cv.sh "adult.csv" 100
time ./weka-cv.sh "adult.csv" 200
time ./weka-cv.sh "adult.csv" 300
time ./weka-cv.sh "adult.csv" 400
time ./weka-cv.sh "adult.csv" 500
time ./weka-cv.sh "adult.csv" 600
time ./weka-cv.sh "adult.csv" 700
time ./weka-cv.sh "adult.csv" 800
time ./weka-cv.sh "adult.csv" 900
time ./weka-cv.sh "adult.csv" 1000

time ./weka-cv.sh "adult-1000.csv" 1000
time ./weka-cv.sh "adult-2000.csv" 2000
time ./weka-cv.sh "adult-3000.csv" 3000
time ./weka-cv.sh "adult-4000.csv" 4000
time ./weka-cv.sh "adult-5000.csv" 5000
time ./weka-cv.sh "adult-6000.csv" 6000
time ./weka-cv.sh "adult-7000.csv" 7000
time ./weka-cv.sh "adult-8000.csv" 8000
time ./weka-cv.sh "adult-9000.csv" 9000
time ./weka-cv.sh "adult-10000.csv" 10000

#
# This take 69m53s on my system, so uncomment only if you're bored!
#
# time ./weka-cv.sh "adult.csv" 32560