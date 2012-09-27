import System.IO

import HLearn.Base
import HLearn.DataContainers
import HLearn.DataContainers.DS_List
import HLearn.Evaluation.CrossValidation
import HLearn.Evaluation.Metrics
import HLearn.Math.Functors
import HLearn.Math.TypeClasses
import HLearn.Models.AlgTree
-- import HLearn.Models.Distributions.Categorical
import HLearn.Models.Distributions
import HLearn.Models.Distributions.Gaussian as Gaussian
import HLearn.Models.DTree
import HLearn.Models.KNN
import HLearn.Models.NBayes
import HLearn.Models.Ensemble
import HLearn.Models.Ensemble.ASSEMBLE
import HLearn.Models.Ensemble.Bagging
            
main = do
{-    let datafileL = 
            [ {-DatafileDesc "../../datasets/uci/abalone.data"   FirstC  Nothing
-- *            , DatafileDesc "../../datasets/uci/adult.data"     LastC   $ Just "?"
            , DatafileDesc "../../datasets/uci/anneal.data"     LastC   $ Just "?"
--             , DatafileDesc "../../datasets/uci/arrhythmia.data"     LastC   $ Just "?" 
            , DatafileDesc "../../datasets/uci/audiology.standardized.data"     LastC   $ Just "?"
            , DatafileDesc "../../datasets/uci/balance-scale.data"     FirstC   Nothing
--             , DatafileDesc "../../datasets/uci/bank-full.csv.modified"     LastC   Nothing
            , DatafileDesc "../../datasets/uci/transfusion.data.modified"     LastC   Nothing
            , DatafileDesc "../../datasets/uci/breast-cancer-wisconsin.data"     LastC   $ Just "?"
--             , DatafileDesc "../../datasets/uci/wdbc.data"     LastC   $ Just "?"
            , DatafileDesc "../../datasets/uci/wpbc.data"     LastC   $ Just "?"
--             , DatafileDesc "../../datasets/uci/car.data"     LastC   $ Just "?"
--  *           , DatafileDesc "../../datasets/uci/krkopt.data"     LastC   $ Nothing
--  *           , DatafileDesc "../../datasets/uci/kr-vs-kp.data"     LastC   $ Nothing
--             , DatafileDesc "../../datasets/uci/CNAE-9.data"     FirstC   $ Nothing
            , DatafileDesc "../../datasets/uci/house-votes-84.data"     FirstC   $ Just "?"
            , DatafileDesc "../../datasets/uci/sonar.all-data"     LastC   $ Nothing
            , DatafileDesc "../../datasets/uci/cmc.data"     LastC   $ Just "?"
            , DatafileDesc "../../datasets/uci/crx.data"     LastC   $ Just "?"
--             , DatafileDesc "../../datasets/uci/bands.data"     LastC   $ Just "?"
--             , DatafileDesc "../../datasets/uci/dermatology.data"     LastC   $ Just "?"
--             , DatafileDesc "../../datasets/uci/echocardiogram.data"     FirstC   $ Just "?"
--             , DatafileDesc "../../datasets/uci/ecoli.data"     FirstC   $ Nothing
            , DatafileDesc "../../datasets/uci/glass.data"     LastC   $ Nothing
            , DatafileDesc "../../datasets/uci/haberman.data"     LastC   $ Nothing
--  *           , DatafileDesc "../../datasets/uci/hayes-roth.data"     LastC   $ Nothing
--             , DatafileDesc "../../datasets/uci/hepatitis.data"     LastC   $ Just "?"
--             , DatafileDesc "../../datasets/uci/Indian Liver Patient Dataset (ILPD).csv"     LastC   $ Nothing
            , DatafileDesc "../../datasets/uci/segmentation.data"     FirstC   $ Nothing
            , DatafileDesc "../../datasets/uci/ionosphere.data"     LastC   $ Nothing
            , DatafileDesc "../../datasets/uci/iris.data"     LastC   $ Nothing
--  *           , DatafileDesc "../../datasets/uci/letter-recognition.data"     FirstC   $ Nothing
            , DatafileDesc "../../datasets/uci/movement_libras.data"     LastC   $ Nothing
--             , DatafileDesc "../../datasets/uci/lung-cancer.data"     LastC   $ Just "?"
--  *           , DatafileDesc "../../datasets/uci/magic04.data"     LastC   $ Nothing
--             , DatafileDesc "../../datasets/uci/mammographic_masses.data"     LastC   $ Just "?"
--  *           , DatafileDesc "../../datasets/uci/agaricus-lepiota.data"     FirstC   $ Just "?"
--             , DatafileDesc "../../datasets/uci/nursery.data"     LastC   $ Nothing
--  *           , DatafileDesc "../../datasets/uci/optdigits.train.data"     LastC   $ Nothing
--             , DatafileDesc "../../datasets/uci/eighthr.data"     LastC   $ Just "?"
--             , DatafileDesc "../../datasets/uci/onehr.data"     LastC   $ Just "?"
--  *           , DatafileDesc "../../datasets/uci/pendigits.train.data"     LastC   $ Nothing
            , DatafileDesc "../../datasets/uci/pima-indians-diabetes.data"     LastC   $ Nothing
--             , DatafileDesc "../../datasets/uci/bridges.data.version2"     LastC   $ Just "?"
--             , DatafileDesc "../../datasets/uci/post-operative.data"     LastC   $ Just "?"
            , DatafileDesc "../../datasets/uci/soybean-large.data"     FirstC   $ Just "?"
            , DatafileDesc "../../datasets/uci/spambase.data"     LastC   $ Nothing
-- ?             , DatafileDesc "../../datasets/uci/SPECT.train"     FirstC   $ Nothing 
            , DatafileDesc "../../datasets/uci/SPECTF.train"     FirstC   $ Nothing
-- ?             , DatafileDesc "../../datasets/uci/australian.dat"     LastC   $ Nothing 
            , -}DatafileDesc "../../datasets/uci/german.data"     LastC   $ Nothing
-- ?             , DatafileDesc "../../datasets/uci/heart.dat"     LastC   $ Nothing
            , DatafileDesc "../../datasets/uci/segment.dat"     LastC   $ Nothing
            , DatafileDesc "../../datasets/uci/sat.trn"     LastC   $ Nothing
-- *             , DatafileDesc "../../datasets/uci/shuttle.tst"     LastC   $ Nothing
--             , DatafileDesc "../../datasets/uci/Faults.NNA"     LastC   $ Nothing
            , DatafileDesc "../../datasets/uci/tae.data"     LastC   $ Nothing
            , DatafileDesc "../../datasets/uci/sick-euthyroid.data"     FirstC   $ Just "?"
            , DatafileDesc "../../datasets/uci/new-thyroid.data"     FirstC   $ Nothing
            , DatafileDesc "../../datasets/uci/hypothyroid.data"     FirstC   $ Just "?"
--             , DatafileDesc "../../datasets/uci/ann-train.data"     LastC   $ Nothing
            , DatafileDesc "../../datasets/uci/tic-tac-toe.data"     LastC   $ Nothing
            , DatafileDesc "../../datasets/uci/Wall-following-sensor_readings_24.data"     LastC   $ Nothing
--             , DatafileDesc "../../datasets/uci/wine.data"     FirstC   $ Nothing
--             , DatafileDesc "../../datasets/uci/winequality-white.data"     LastC   $ Nothing
-- *            , DatafileDesc "../../datasets/uci/yeast.data"     LastC   $ Nothing
            , DatafileDesc "../../datasets/uci/zoo.data"     LastC   $ Nothing
            ]-}
        
    -- select which file to test on
    let datafile=DatafileDesc "../../datasets/uci/breast-cancer-wisconsin.data"     LastC   $ Nothing
--     let datafile=DatafileDesc "../../datasets/uci/german.data"     LastC   $ Nothing
--     let datafile=DatafileDesc "../../datasets/uci/tic-tac-toe.data"     LastC   $ Nothing
--     let datafile=DatafileDesc "../../datasets/uci/pima-indians-diabetes.data"     LastC   $ Nothing        
--     let datafile=DatafileDesc "../../datasets/uci/sick-euthyroid.data"     FirstC   $ Just "?"

    -- open and preprocess data
    putStrLn $ "Openning file: " ++ (datafileName datafile)
    ds <- loadDataCSV datafile :: IO (DS_List String (LDPS String))
    deepseq ds $ putStrLn "done."
    let dsint = runHLearn 10 $ randomize $ ds2intds ds

    hout <- openFile ("semigroupbagger-"++(datafileName datafile)++".csv") WriteMode
    hPutStrLn hout "Iteration,Semigroup Accuracy, Semigroup Stddev, Divisor Accuracy, Divisor Stddev"
    
    -- perform experiments
    forM (filter odd [1..29]) $ \i -> do
--         let baseparams = defAlgTreeParams
        let baseparams = defNBayesParams
        let paramsSG = defSemigroupTrainer i $ VotingSemigroupParams baseparams
        let paramsD = DataDivider (fi i) baseparams
        
        let gaussianSG = runHLearn 0 $ crossValidation Accuracy (Trainer2TrainerSS paramsSG) dsint 0.8 1 100
        let gaussianD = runHLearn 0 $ crossValidation Accuracy (Trainer2TrainerSS paramsD) dsint 0.8 1 100
        let (accSG,acc_stddevSG) = (mean gaussianSG, Gaussian.varianceSample gaussianSG)
        let (accD,acc_stddevD) = (mean gaussianD, Gaussian.varianceSample gaussianD)
        putStrLn $ 
            "i="++show i++
            ", accSG = " ++ show accSG ++ 
            ", acc_stddevSG = " ++ show acc_stddevSG ++
            ", accD = " ++ show accD ++ 
            ", acc_stddevD = " ++ show acc_stddevD
        hPutStrLn hout $ 
            show i++","++
            show accSG ++ "," ++ 
            show acc_stddevSG++","++
            show accD ++ "," ++ 
            show acc_stddevD
        hFlush hout

    hClose hout
