import Control.DeepSeq
import Data.Monoid
import Data.Map

import HMine.Base
import HMine.DataContainers
import HMine.DataContainers.DS_List
import HMine.Evaluation.CrossValidation
import HMine.Evaluation.Metrics
import HMine.Math.Functors
import HMine.Math.TypeClasses
import HMine.Models.AlgTree
import HMine.Models.Distributions.Dirichlet
import HMine.Models.DTree
import HMine.Models.KNN
import HMine.Models.NBayes
import HMine.Models.Ensemble
import HMine.Models.Ensemble.ASSEMBLE
import HMine.Models.Ensemble.AdaBoost
import HMine.Models.Ensemble.Bagging
import HMine.Models.Ensemble.SemiBoost

--         [ DatafileDesc { datafileName = "../datasets/uci/abalone.data", datafileLabelColumn = FirstC, datafileMissingStr = Nothing }
            
main = do
    let datafileL = 
            [ DatafileDesc "../datasets/uci/abalone.data"   FirstC  Nothing
            , DatafileDesc "../datasets/uci/adult.data"     LastC   $ Just "?"
            , DatafileDesc "../datasets/uci/anneal.data"     LastC   $ Just "?"
            , DatafileDesc "../datasets/uci/audiology.data"     LastC   $ Just "?"
            , DatafileDesc "../datasets/uci/arrhythmia.data"     LastC   $ Just "?"
            , DatafileDesc "../datasets/uci/audiology.standardized.data"     LastC   $ Just "?"
            , DatafileDesc "../datasets/uci/balance-scale.data"     FirstC   Nothing
--             , DatafileDesc "../datasets/uci/bank-full.csv.modified"     LastC   Nothing
            , DatafileDesc "../datasets/uci/transfusion.data.modified"     LastC   Nothing
            , DatafileDesc "../datasets/uci/breast-cancer-wisconsin.data"     LastC   $ Just "?"
--             , DatafileDesc "../datasets/uci/wdbc.data"     LastC   $ Just "?"
            , DatafileDesc "../datasets/uci/wpbc.data"     LastC   $ Just "?"
--             , DatafileDesc "../datasets/uci/car.data"     LastC   $ Just "?"
            , DatafileDesc "../datasets/uci/krkopt.data"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/kr-vs-kp.data"     LastC   $ Nothing
--             , DatafileDesc "../datasets/uci/CNAE-9.data"     FirstC   $ Nothing
            , DatafileDesc "../datasets/uci/house-votes-84.data"     FirstC   $ Just "?"
            , DatafileDesc "../datasets/uci/sonar.all-data"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/cmc.data"     LastC   $ Just "?"
            , DatafileDesc "../datasets/uci/crx.data"     LastC   $ Just "?"
--             , DatafileDesc "../datasets/uci/bands.data"     LastC   $ Just "?"
            , DatafileDesc "../datasets/uci/dermatology.data"     LastC   $ Just "?"
--             , DatafileDesc "../datasets/uci/echocardiogram.data"     FirstC   $ Just "?"
--             , DatafileDesc "../datasets/uci/ecoli.data"     FirstC   $ Nothing
            , DatafileDesc "../datasets/uci/glass.data"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/haberman.data"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/hayes-roth.data"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/hepatitis.data"     LastC   $ Just "?"
--             , DatafileDesc "../datasets/uci/Indian Liver Patient Dataset (ILPD).csv"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/segmentation.data"     FirstC   $ Nothing
            , DatafileDesc "../datasets/uci/ionosphere.data"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/iris.data"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/letter-recognition.data"     FirstC   $ Nothing
            , DatafileDesc "../datasets/uci/movement_libras.data"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/lung-cancer.data"     LastC   $ Just "?"
            , DatafileDesc "../datasets/uci/magic04.data"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/mammographic_masses.data"     LastC   $ Just "?"
            , DatafileDesc "../datasets/uci/agaricus-lepiota.data"     FirstC   $ Just "?"
--             , DatafileDesc "../datasets/uci/nursery.data"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/optdigits.train.data"     LastC   $ Nothing
--             , DatafileDesc "../datasets/uci/eighthr.data"     LastC   $ Just "?"
--             , DatafileDesc "../datasets/uci/onehr.data"     LastC   $ Just "?"
            , DatafileDesc "../datasets/uci/pendigits.train.data"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/pima-indians-diabetes.data"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/bridges.data.version2"     LastC   $ Just "?"
--             , DatafileDesc "../datasets/uci/post-operative.data"     LastC   $ Just "?"
            , DatafileDesc "../datasets/uci/soybean-large.data"     FirstC   $ Just "?"
            , DatafileDesc "../datasets/uci/spambase.data"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/SPECT.train"     FirstC   $ Nothing
            , DatafileDesc "../datasets/uci/SPECTF.train"     FirstC   $ Nothing
            , DatafileDesc "../datasets/uci/australian.dat"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/german.data"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/heart.dat"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/segment.dat"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/sat.trn"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/shuttle.tst"     LastC   $ Nothing
--             , DatafileDesc "../datasets/uci/Faults.NNA"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/tae.data"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/sick-euthyroid.data"     FirstC   $ Just "?"
            , DatafileDesc "../datasets/uci/new-thyroid.data"     FirstC   $ Nothing
            , DatafileDesc "../datasets/uci/hypothyroid.data"     FirstC   $ Just "?"
--             , DatafileDesc "../datasets/uci/ann-train.data"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/tic-tac-toe.data"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/Wall-following-sensor_readings_24.data"     LastC   $ Nothing
--             , DatafileDesc "../datasets/uci/wine.data"     FirstC   $ Nothing
--             , DatafileDesc "../datasets/uci/winequality-white.data"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/yeast.data"     LastC   $ Nothing
            , DatafileDesc "../datasets/uci/zoo.data"     LastC   $ Nothing
            ]
        
    let algparams1 = defAlgTreeParams
    let algparams2 = defDStumpParams
    
    mapM (\datafile -> compareAlgorithms datafile algparams1 algparams2) [head datafileL]
            

compareAlgorithms datafile algparams1 algparams2 = do
    
    -----------------------------------
    -- load data
    
    putStrLn "loading data"
    ds <- loadDataCSV datafile :: IO (DS_List String (LDPS String))
    deepseq ds $ putStrLn "done."
    let dsint = runHMine 10 $ randomize $ ds2intds ds
    let dsbool = ds2boolds [0] dsint

    let ds=dsint

    -----------------------------------
    -- run tests
        
    putStrLn "Testing algparams1"
    let alg1 = runHMine 10 $ trainBatch algparams1 dsint
    deepseq alg1 $ return () -- print stumpy
    putStrLn $ "accuracy = " ++ show (accuracy alg1 dsint)
    putStrLn $ "CM = " ++ show (genConfusionMatrix alg1 dsint)

