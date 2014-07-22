{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Data.List.Extras
import Data.Maybe
import Data.Typeable
import qualified Data.ByteString.Lazy as BS
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import Foreign.Storable
import System.IO

import Data.Csv
import Numeric.LinearAlgebra hiding ((<>))
import System.Console.CmdArgs.Implicit

import Debug.Trace

import Data.Version
import Paths_HLearn

import HLearn.Algebra
import qualified HLearn.Algebra.LinearAlgebra as LA
import HLearn.Evaluation.CrossValidation
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common
import HLearn.Models.Classifiers.LinearClassifier
import qualified HLearn.Optimization.GradientDescent as Recipe
import qualified HLearn.Optimization.Common as Recipe
import HLearn.Optimization.Trace

import Timing
import LoadData

-------------------------------------------------------------------------------
-- command line parameters

data Params = Params
    { data_file         :: Maybe String 
    , label_col         :: Int

    , maxdp             :: Maybe Int

    , paramCVFolds      :: Int
    , paramCVReps       :: Int

    , monoidType        :: String
    , monoidSplits      :: Int

    , regType           :: String
    , regAmount         :: Double

    , pca_data          :: Bool
    , varshift_data     :: Bool

    , paramSeed         :: Maybe Int
    , verbose           :: Bool
    , debug             :: Bool
    } 
    deriving (Show, Data, Typeable)

usage = Params 
    { data_file      = Nothing
                    &= help "file to perform classification on"

    , label_col      = 0
                    &= help "label column"

    , maxdp          = Nothing
                    &= help "maximum number of datapoints to use from data set"

    , paramCVFolds   = 10
                    &= help "number of folds for cross validation"
                    &= name "cvfolds"
                    &= explicit
                    &= groupname "Test Configuration"

    , paramCVReps    = 1
                    &= help "number of times to repeat cross validation"
                    &= name "cvreps"
                    &= explicit

    , monoidSplits   = 1
                    &= help "when training the classifier, first split the dataset into this many subsets; then combine using the specified monoidType"

    , monoidType     = "MappendAverage"
                    &= help "see monoid splts"

    , regType        = "L2"
                    &= help "valid values are: L1, L2, ElasticNet"

    , regAmount      = 0.1
                    &= help "regularization parameter lambda"

    , pca_data       = False 
                    &= groupname "Data Preprocessing" 
                    &= help "Rotate the data points using the PCA transform.  Speeds up nearest neighbor searches, but computing the PCA can be expensive in many dimensions."
                    &= name "pca"
                    &= explicit

    , varshift_data  = False 
                    &= help "Sort the attributes according to their variance.  Provides almost as much speed up as the PCA transform during neighbor searches, but much less expensive in higher dimensions." 
                    &= name "varshift"
                    &= explicit

    , paramSeed      = Nothing
                    &= help "specify the seed to the random number generator (default is seeded by current time)"
                    &= groupname "Debugging"
    
    , verbose        = False 
                    &= help "Print tree statistics (takes some extra time)" 

    , debug          = False 
                    &= help "Test created trees for validity (takes lots of time)" 
                    &= name "debug"
                    &= explicit
    }
    &= summary ("HLearn linear classifiers, version " ++ showVersion version)

-------------------------------------------------------------------------------
-- main

main = do
    
    -----------------------------------
    -- proccess command line

    params <- cmdArgs usage

    when (data_file params == Nothing) $ error "must specify a data file"

    seed <- case paramSeed params of
            Nothing -> randomIO
            Just x -> return x

    let filename = fromJust $ data_file params
        label_index = label_col params
        
    let verbose = True

    -----------------------------------
    -- load data

    dps :: V.Vector (MaybeLabeled String (LA.Vector Double))
        <- loadLabeledNumericData $ DataParams
        { datafile  = fromJust $ data_file params
        , labelcol  = Just $ label_col params
        , pca       = False
        , varshift  = False
        }

    -----------------------------------
    -- run test

    let traceEvents =
            [ traceBFGS
            , traceNewtonRaphson
            , traceLinearClassifier (undefined::MaybeLabeled String (LA.Vector Double))
            ]

    let withMonoid :: ( Monad m )
                   => ( model -> model -> model )
                   -> Int
                   -> ( [dp] -> m model )
                   -> ( [dp] -> m model )
        withMonoid f n _train dps = liftM (F.foldl1 f) $ mapM _train $ partition n dps

    let reg = case regType params of
            "L1" -> l1reg
            "L2" -> l2reg
            "ElasticNet" -> elasticNet

    let readMonoidType = read $ monoidType params :: MonoidType
    let monoidOperation = case readMonoidType of
            MappendAverage -> mappendAverage
            MappendTaylor -> mappendQuadratic
            MappendUpperBound -> mappendQuadratic
            MixtureUpperTaylor _ -> mappendQuadratic
            MixtureAveTaylor _ -> mappendQuadratic
            MixtureAveUpper _ -> mappendQuadratic
            otherwise -> error $ "monoidtype ["++monoidType params++"] not supported"
    seq monoidOperation $ return () 

    let maxdpselector :: SamplingMethod -> SamplingMethod 
        maxdpselector = case maxdp params of
            Just x -> setMaxDatapoints x
            Nothing -> \x -> x

    let res = traceHistory traceEvents $ flip evalRandT (mkStdGen seed) $ validateM
            ( repeatExperiment 
                ( paramCVReps params ) 
                ( maxdpselector
                    ( kfold $ paramCVFolds params) 
                )
            )
            errorRate
            dps
            ( withMonoid monoidOperation (monoidSplits params) 
                $ trainLogisticRegression 
                    ( readMonoidType )
                    ( regAmount params )
                    reg
                    logloss
            )

    let showMaybe Nothing = "Nothing"
        showMaybe (Just x) = show x

    putStrLn $ (showMaybe $ paramSeed params)
       ++" "++ show (pca_data params)
       ++" "++ show (varshift_data params)
       ++" "++ show (paramCVFolds params)
       ++" "++ show (paramCVReps params)
       ++" "++ show (monoidSplits params) 
       ++" "++ (head $ words $ monoidType params)
       ++" "++ (show (fromRational $ monoidMixRate readMonoidType::Double))
       ++" "++ (regType params)
       ++" "++ show (regAmount params)
       ++" "++ (showMaybe $ maxdp params)
       ++ concat (replicate 19 " -- ")
       ++" "++ show (mean res)
       ++" "++ show (variance res)

    hPutStrLn stderr $ "mean = "++show (mean res)
    hPutStrLn stderr $ "var  = "++show (variance res)


    -----------------------------------
    -- temporarily needed for type checking
--     putStrLn $ show $ map mean $ foldl1 (zipWith (<>)) 
--         [ flip evalRand (mkStdGen seed') $ monoidtest (VG.toList dps) 1
--         | seed' <- [seed..seed+paramReps params-1]
--         ]

--     let m = train dps :: LogisticRegression (MaybeLabeled String (LA.Vector Double))
--     deepseq m $ print $ m

--     let testM n f = validate
--             (numSamples n (kfold 10))
--             errorRate
--             dps
--             (f train :: [MaybeLabeled String (LA.Vector Double)] 
--                                -> LogisticRegression (MaybeLabeled String (LA.Vector Double)))
-- 
--     let runtest n f = reduce [flip evalRand (mkStdGen i) $ testM n f | i <- [0..10]]
-- 
--     let tests = 
--             [ do 
--                 putStr $ show n++", "
--                 let res = runtest n id
--                 putStr $ show (mean res)++", "++show (variance res)++", "
--                 let res_taylor = runtest n (withMonoid mappendQuadratic 2)
--                 putStr $ show (mean res_taylor)++", "++show (variance res_taylor)++", "
--                 let res_ave = runtest n (withMonoid mappendAverage 2)
--                 putStrLn $ show (mean res_ave)++", "++show (variance res_ave)
--                 hFlush stdout
--             | n <- [100,200,300,400,500]
--             ]

--     let repl ',' = ' '
--         repl c = c
-- 
--     sequence_ 
--         [ do
--             putStr $ show n++" "
--             putStrLn $ map repl $ tail $ init $ show $ map mean $ foldl1 (zipWith (<>)) 
--                 [ flip evalRand (mkStdGen seed) $ monoidtest (VG.toList dps) n
--                 | seed <- [1..100]
--                 ]
--             hFlush stdout
--         | n <- [28..50]
--         | n <- [1,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280,290,300,310,320,330,340,350,360,370,380,390,400]
--         | n <- [1,50,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850,900,950,1000]
--         | n <- map (*10) [1..5]
--         ]

--     let testM n f = validate
--             (kfold 10)
--             errorRate
--             dps
--             (f train :: [MaybeLabeled String (LA.Vector Double)] 
--                                -> LogisticRegression (MaybeLabeled String (LA.Vector Double)))
-- 
--     let runtest n f = reduce [flip evalRand (mkStdGen i) $ testM n f | i <- [0,1]]
-- 
--     let tests = 
--             [ do 
--                 putStr $ show n++", "
--                 let res_taylor = runtest n (withMonoid mappendQuadratic n)
--                 putStr $ show (mean res_taylor)++", "++show (variance res_taylor)++", "
--                 let res_ave = runtest n (withMonoid mappendAverage n)
--                 putStrLn $ show (mean res_ave)++", "++show (variance res_ave)
--                 hFlush stdout
--             | n <- [1..50]
--             ]
-- 
--     sequence_ tests

    hPutStrLn stderr "done."

