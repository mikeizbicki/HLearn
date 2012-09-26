#
set datafile separator ","
#set size 3.0/3, 1
#set size ratio 1

#set terminal postscript solid lw 2 "Times-Roman" 25
set terminal postscript "Times-Roman" 25
set output "plot.ps"

set xrange [0:25]
set yrange [0.6:0.8]
#set xtics (0,1)
set ytics (0.6,0.65,0.7,0.75,0.8)
unset key
 
set xlabel "Number of splits in data"
set ylabel "Accuracy (%)"
 
#file="SemigroupBagger-results/semigroupbagger-nbayes-tic-tac-toe.data.csv"
#f(x)=0.69575
#file="SemigroupBagger-results/semigroupbagger-nbayes-pima-indians-diabetes.data.csv"
#f(x)=0.75547
file="SemigroupBagger-results/semigroupbagger-nbayes-german.data.csv"
f(x)=0.742781

g(x) = 0.65
h(x) = 0.7

plot f(x) w l lt 2 lw 1 lc rgb "#ff0000",\
     0.65 w l lt 2 lw 1 lc rgb "#ff0000",\
     0.7 w l lt 2 lw 1 lc rgb "#ff0000",\
     #file using 1:2 with lines lw 4 lt 1 lc rgb "#ff0000",\
     #file using 1:4 with lines lw 1 lt 1 lc rgb "#00ff00"
