
test=$1
data=$2

case "$1" in
    mixture     ) echo "testing MonoidMixture";;

    * ) echo "test [$1] not supported";;
esac

case "$data" in
    wine        ) col=0;  file="datasets/uci/wine.csv";;
    wdbc        ) col=0;  file="datasets/uci/wdbc-mod2.csv";;
    wpbc        ) col=0;  file="datasets/uci/wpbc-mod2.csv";;
    sonar       ) col=60; file="datasets/uci/sonar.all-data";;
    ion         ) col=0;  file="datasets/uci/ionosphere.csv";;
    pima        ) col=8;  file="datasets/uci/pima-indians-diabetes.csv";;
    magic       ) col=10; file="datasets/uci/magic04.data";;
    haberman    ) col=3;  file="datasets/uci/haberman.data";;

    optdigits   ) col=64; file="datasets/uci/optdigits.train.data";;
    pendigits   ) col=16; file="datasets/uci/pendigits.train.data";;
    winered     ) col=11; file="datasets/uci/winequality-red.csv";;
    winewhite   ) col=11; file="datasets/uci/winequality-white.csv";;

    *) echo  "dataset [$data] is not recognized... exiting";;
esac


# create param line arguments
array=( $@ )
len=${#array[@]}
cmdargs="--paramseed=0 --cvfolds=2 --cvreps=10 ${array[@]:2:$len}"
echo "Command line args: $cmdargs"

# create tmp directories if they don't exist
[ ! -d "tmp"       ] && mkdir "tmp"
[ ! -d "tmp/$test" ] && mkdir "tmp/$test"

# rename old output files; delete very old files
output="tmp/$test/$data $cmdargs"
[ -e "$output" ] && mv "$output" "$output.old"

# run tests
case "$test" in

    mixture     )
        output1="$output --monoidtype=MixtureAveTaylor"
        output2="$output --monoidtype=MixtureAveUpper"
        output3="$output --monoidtype=MixtureUpperTaylor"
        [ -e "$output1" ] && mv "$output1" "$output1.old"
        [ -e "$output2" ] && mv "$output2" "$output2.old"
        [ -e "$output3" ] && mv "$output3" "$output3.old"
        for i in -50 -40 -30 -20 -10 -8 -6 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 16 18 20 30 40 50; do
            ./dist/build/hlearn-linear/hlearn-linear \
                -d "$file" -l "$col" \
                --monoidtype="MixtureAveTaylor ($i % 10)" \
                $cmdargs \
                >> "$output1"
            ./dist/build/hlearn-linear/hlearn-linear \
                -d "$file" -l "$col" \
                --monoidtype="MixtureAveUpper ($i % 10)" \
                $cmdargs \
                >> "$output2"
            ./dist/build/hlearn-linear/hlearn-linear \
                -d "$file" -l "$col" \
                --monoidtype="MixtureUpperTaylor ($i % 10)" \
                $cmdargs \
                >> "$output3"
        done
        ;;
esac
