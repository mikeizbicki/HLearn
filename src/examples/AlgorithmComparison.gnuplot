#
set datafile separator ","
#set size 3.0/3, 1
#set size ratio 1

#set terminal postscript enhanced color solid lw 2 "Times-Roman" 20
#set terminal postscript eps enhanced color
set terminal postscript solid lw 2 "Times-Roman" 25
set output "plot.ps"

set xrange [0:1]
set yrange [0:1]
set xtics (0,1)
set ytics (0,1)
unset key
 
file = "AlgorithmComparison-results/dud.csv"
#file = "AlgorithmComparison-results/boost.sg9.concat.algstump-v-boost.sg9.sort.algstump.csv"

set xlabel "Accuracy (%)"
set ylabel "Accuracy (%)"
#set xlabel "AdaBoost.AlgStump.sg9-concat \nAccuracy (%)"
#set ylabel "AdaBoost.AlgStump.sg9-sort \nAccuracy (%)"

#plot 
plot x lc rgb "#f0fff0" with filledcurves x1,\
     x lc rgb "#00ff00",\
     file using 5:($3==2 ? $7 : 1/0) lc rgb "#ff0000",\
     file using 5:($3>2 ? $7 : 1/0) lc rgb "#0000ff"
     #"AlgorithmComparison-results/boost.exact.algstump-v-boost.sg9.concat.algstump.csv" using 5:($3==2 ? $7 : 1/0) lc rgb "#ff0000",\
     #"AlgorithmComparison-results/boost.exact.algstump-v-boost.sg9.concat.algstump.csv" using 5:($3>2 ? $7 : 1/0) lc rgb "#0000ff"
     #lc rgb "#0000ff" 
     #"algstump-v-dstump.csv" using 5:7 with linespoints linecolor rgb "#00ff00" pt 3

quit