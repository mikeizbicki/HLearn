time="/usr/bin/time --format=%U"

tests="
../../datasets/uci/wine.csv,0
../../datasets/uci/wdbc-mod2.csv,0
../../datasets/uci/wpbc-mod2.csv,0
../../datasets/uci/sonar.all-data,60
../../datasets/uci/ionosphere.csv,0
../../datasets/uci/pima-indians-diabetes.csv,8
../../datasets/uci/magic04.data,10
../../datasets/uci/haberman.data,3
../../datasets/uci/optdigits.train.data,64
../../datasets/uci/pendigits.train.data,16
../../datasets/uci/winequality-red.csv,11
../../datasets/uci/winequality-white.csv,11
"

for i in $tests; do
    IFS=','
    set $i

    longfile=$1
    shortfile=`basename $1`
    col=$2

    sklearn=`/usr/bin/time --format="%U" ./logreg.py $longfile $col 2>&1 >/dev/null`
    echo "sklearn $shortfile $sklearn"

    #hlearn=`/usr/bin/time --format="%U" hlearn-linear -d $longfile -l $col --nocv 2>&1 >/dev/null | tail -n 1` 
    #echo "hlearn $shortfile $hlearn"

done
