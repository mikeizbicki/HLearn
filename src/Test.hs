import Control.DeepSeq
import Control.Monad.Random
import Data.Monoid

import HMine.Base
import HMine.DataContainers
import HMine.DataContainers.DS_List
import HMine.Classifiers.TypeClasses
import HMine.Classifiers.Dirichlet
import HMine.Classifiers.DTree
import HMine.Classifiers.KNN
import HMine.Classifiers.LazyNBayes
import HMine.Classifiers.Ensemble
import HMine.Classifiers.Ensemble.AdaBoost
import HMine.Classifiers.Ensemble.Bagging
import HMine.Testing

-- testdata :: DS_List Int (LDPS Int)
-- testdata=DS_List dpdesc [dp i | i<-[0..7]] 8 [0,1]

main = do
    let datafile = DatafileDesc 
--             { datafileName = "/home/user/proj/haskell-classification/testdata/german.csv" 
            { datafileName = "/home/user/proj/haskell-classification/testdata/haberman.csv" 
            , datafileTrueClass = Nothing
            , datafileMissingStr = Nothing
            }
    putStrLn "loading data"
    ds <- loadDataCSV datafile :: IO (DS_List String (LDPS String))
    deepseq ds $ putStrLn "done."
    let dsint = ds2intds ds
{-    let nb = runHMine 10 $ trainBatch defNBayesParams dsint
    let bag = runHMine 10 $ trainBatch (defBaggingParams 0 10 defNBayesParams) dsint
    let ada = runHMine 10 $ trainBatch (AdaBoostParams 10 (SampleTrainer defNBayesParams 0.5)) dsint
    let disp i = (fst $ (getDataL dsint) !! i,probabilityClassify nb $ (getDataL $ lds2uds dsint) !! i)-}
--     sequence_ [ print $ disp i | i<-[0..9]]

--     print $ evalRand (crossValidation (AdaBoostParams 10 (SampleTrainer defKNNParams 0.1)) dsint 0.90 10) (mkStdGen 10)
--     print $ evalRand (crossValidation (defBaggingParams 0 1 defNBayesParams) dsint 0.90 10) (mkStdGen 10)
--     print $ evalRand (crossValidation defNBayesParams dsint 0.9 10) (mkStdGen 10)
--     print $ evalRand (crossValidation defKNNParams dsint 0.9 20) (mkStdGen 10)
--     let x= evalRand (crossValidation (AdaBoostParams 20 $ Trainer2WeightedTrainer 0.5 defDTreeParams) dsint 0.80 10) (mkStdGen 10)
--     let x= evalRand (crossValidation (AdaBoostParams 20 $ Trainer2WeightedTrainer 0.5 defNBayesParams) dsint 0.80 10) (mkStdGen 10)
--     print x
--     print x
--     print $ evalRand (crossValidation DirichletParams ({-takeFirst 50 -}dsint) 0.9 10) (mkStdGen 10)
    let x= evalRand (crossValidation (defDTreeParams {maxDepth=2,leafModelParams=defNBayesParams}) ({-takeFirst 50 -}dsint) 0.9 10) (mkStdGen 10)
    let y= evalRand (crossValidation defDStumpParams ({-takeFirst 50 -}dsint) 0.9 10) (mkStdGen 10)
    print x
    print y
    print x
    print y

--     putStrLn "Stump -------------------------------"
--     let stumpModel = runHMine 10 $ trainBatch defDStumpParams dsint
--     deepseq stumpModel $ print stumpModel
-- 
--     putStrLn "DTree-2 -------------------------------"
--     let stumpModel = runHMine 10 $ trainBatch (defDStumpParams {maxDepth=2,leafModelParams=defNBayesParams})dsint
--     deepseq stumpModel $ print stumpModel

ada=[0.712707182320442,0.7101449275362319,0.6927083333333334,0.6815642458100558,0.7433155080213903,0.735632183908046,0.6756756756756757,0.7004830917874396,0.6903553299492385,0.7210526315789474]
stump=[0.7263157894736842,0.6727272727272727,0.7053571428571429,0.7169811320754716,0.6633663366336634,0.7310924369747899,0.7096774193548387,0.6952380952380952,0.7446808510638298,0.6571428571428571,0.6428571428571429,0.717948717948718,0.6864406779661016,0.6176470588235294,0.6666666666666666,0.7222222222222222,0.6666666666666666,0.7549019607843137,0.6698113207547169,0.7373737373737373]