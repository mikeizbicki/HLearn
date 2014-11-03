{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ConstraintKinds #-}

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
import System.Exit
import System.IO

import Data.Csv
import System.Console.CmdArgs.Implicit

import Debug.Trace
import Prelude (head,words,replicate)

import Data.Version
import Paths_HLearn

import SubHask
import SubHask.Algebra.HMatrix
-- import HLearn.Algebra
import HLearn.Evaluation.CrossValidation
import HLearn.History
import HLearn.History.DisplayMethods
-- import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common
import HLearn.Models.Classifiers.LinearClassifier
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Univariate.Normal
import HLearn.Optimization.Common
-- import HLearn.Optimization.StochasticGradientDescent
import HLearn.Optimization.StepSize
import HLearn.Optimization.StepSize.Linear
import HLearn.Optimization.LineMinimization.Univariate
import HLearn.Optimization.GradientDescent
-- import HLearn.Optimization.NewtonRaphson
-- import qualified HLearn.Optimization.GradientDescent as Recipe
-- import qualified HLearn.Optimization.Common as Recipe
-- import HLearn.Optimization.Trace

import Timing
import LoadData

-------------------------------------------------------------------------------
-- command line parameters

data Params = Params
    { data_file         :: Maybe String
    , label_col         :: Int

    , maxdp             :: Maybe Int

    , noCV              :: Bool
    , paramCVFolds      :: Int
    , paramCVReps       :: Int

    , monoidType        :: String
    , monoidSplits      :: Int

    , regType           :: String
    , regAmount         :: Double
    , step_eta          :: Double
    , maxitr            :: Int

    , pca_data          :: Bool
    , varshift_data     :: Bool

    , paramSeed         :: Maybe Int
    , quiet             :: Bool
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

    , noCV           = False
                    &= help "do not cross validate; just build model"
                    &= groupname "Test Configuration"

    , paramCVFolds   = 10
                    &= help "number of folds for cross validation"
                    &= name "cvfolds"
                    &= explicit

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

    , step_eta       = 0.001
                    &= help "eta parameter for step sizes"

    , maxitr         = 10000
                    &= help "maximum number of iterations"

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

    , quiet          = False
                    &= help "supress all output"

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
    -- process command line

    params <- cmdArgs usage

    when (data_file params == Nothing) $ error "must specify a data file"

    seed <- case paramSeed params of
            Nothing -> randomIO
            Just x -> return x

    let filename = fromJust $ data_file params
        label_index = label_col params

    -----------------------------------
    -- load data

    dps :: V.Vector (MaybeLabeled String (Vector Double))
        <- loadLabeledNumericData $ DataParams
            { datafile  = fromJust $ data_file params
            , labelcol  = Just $ label_col params
            , pca       = pca_data params
            , varshift  = False
            }

    -----------------------------------
    -- test parameters

    let traceEvents =
            []
--             [ traceBFGS
--             , traceNewtonRaphson
--             , traceLinearClassifier (undefined::MaybeLabeled String (Vector Double))
--             ]

--     let withMonoid :: ( Monad m )
--                    => ( model -> model -> model )
--                    -> Int
--                    -> ( [dp] -> m model )
--                    -> ( [dp] -> m model )
--         withMonoid f n _train dps = liftM (F.foldl1 f) $ mapM _train $ partition n dps

    let reg = case regType params of
            "L1" -> l1reg
            "L2" -> l2reg
            "ElasticNet" -> elasticNet

    let readMonoidType = read $ monoidType params :: MonoidType
    let monoidOperation = case readMonoidType of
--             MappendAverage -> mappendAverage
--             MappendTaylor -> mappendQuadratic
--             MappendUpperBound -> mappendQuadratic
--             MixtureUpperTaylor _ -> mappendQuadratic
--             MixtureAveTaylor _ -> mappendQuadratic
--             MixtureAveUpper _ -> mappendQuadratic
            otherwise -> error $ "monoidtype ["++monoidType params++"] not supported"
--     seq monoidOperation $ return ()

    let maxdpselector :: SamplingMethod -> SamplingMethod
        maxdpselector = case maxdp params of
            Just x -> setMaxDatapoints x
            Nothing -> \x -> x

--     let debugHistory = if debug params
--             then runHistory compactTrace
--             else runHistory compactTrace

--     let debugHistory = runHistory linearTrace
    let debugHistory = runDynamicHistory
              $ {- summaryStatistics
            === -}removeLineMin
--                 ||| sampleSGD 10000
                ||| linearTrace
--             === removeLineMin
--                 ||| (allowPasses (undefined::NewtonRaphson (Vector Double)) [2]
--                 ||| linearTrace)
--             === allowFirstPass (undefined::ConjugateGradientDescent (Vector Double))
--                 ||| mkOptimizationPlot
--                     (undefined::ConjugateGradientDescent (Vector Double))
--                     "optplot.dat"

    let optMethod ::
            ( HistoryMonad m
            , Reportable m (LineBracket Double)
            , Reportable m (Brent Double)
            , Reportable m (ConjugateGradientDescent (Vector Double))
            ) => OptimizationMethod m (MaybeLabeled String (Vector Double))
        optMethod = cgd
--         optMethod = sgd
--             [ maxIterations $ maxitr params ]
--             randomSample
-- --             linearScan
-- --             ( lrAlmeidaLanglois )
--             ( Hyperparams { eta = step_eta params, gamma = regAmount params } )

    -----------------------------------
    -- single models

    when (noCV params) $ do
        model <- runDynamicHistory
            ( summaryStatistics
          === removeLineMin
            ||| linearTrace
          === allowFirstPass (undefined::ConjugateGradientDescent (Vector Double))
            ||| mkOptimizationPlot
                ( undefined::ConjugateGradientDescent (Vector Double))
                "optplot.dat"
            )
            ( trainLinearClassifier
                readMonoidType
                ( regAmount params )
                reg
                logloss
                optMethod
                dps
            )

        timeIO "training model" $ deepseq model $ return ()
        exitSuccess

    -----------------------------------
    -- run cv tests

    res <- debugHistory $ flip evalRandT (mkStdGen seed) $ validateM
            ( repeatExperiment
                ( paramCVReps params )
                ( maxdpselector
                    ( kfold $ paramCVFolds params)
                )
            )
            errorRate
            dps
            ( --withMonoid monoidOperation (monoidSplits params) $
                trainLinearClassifier
                    ( readMonoidType )
                    ( regAmount params )
                    reg
                    logloss
                    optMethod
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
    hPutStrLn stderr "done."

