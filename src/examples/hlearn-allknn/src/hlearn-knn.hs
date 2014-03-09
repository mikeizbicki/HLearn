{-# LANGUAGE ScopedTypeVariables,TemplateHaskell,DeriveDataTypeable,DataKinds,FlexibleInstances,TypeFamilies,RankNTypes,BangPatterns,FlexibleContexts,StandaloneDeriving,GeneralizedNewtypeDeriving,TypeOperators, UndecidableInstances, ExistentialQuantification, GADTs #-}

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Data.Csv
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Foldable as F
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import System.Console.CmdArgs.Implicit
import System.Environment
import System.IO
import Text.Read

import Debug.Trace

import qualified Control.ConstraintKinds as CK
import HLearn.Algebra
import HLearn.Models.Classifiers.Common
import HLearn.Models.Classifiers.NearestNeighbor
import HLearn.Models.Classifiers.NaiveNN
import HLearn.DataStructures.CoverTree
import HLearn.DataStructures.SpaceTree
import HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor
import HLearn.DataStructures.SpaceTree.DualTreeMonoids
import HLearn.Evaluation.CrossValidation
import HLearn.Metrics.Lebesgue
import HLearn.Metrics.Mahalanobis
import HLearn.Metrics.Mahalanobis.Normal
import HLearn.Metrics.Mahalanobis.ITML
import HLearn.Metrics.Mahalanobis.Lego
import HLearn.Metrics.Mahalanobis.LegoPaper hiding (shuffle)
import HLearn.Metrics.Mahalanobis.Mega
import HLearn.Models.Distributions

import Utils

type DP = MaybeLabeled String (L2 VU.Vector Double)

instance Num r => HasRing (V.Vector r) where type Ring (V.Vector r) = r
instance Num r => HasRing (VU.Vector r) where type Ring (VU.Vector r) = r

instance (NFData label, NFData attr) => NFData (MaybeLabeled label attr) where
    rnf (MaybeLabeled l a) = deepseq l $ rnf a

-------------------------------------------------------------------------------
-- command line args

data Params = Params
    { k                 :: Int
    , folds             :: Int
    , trials            :: Int
    , param_nummetricsamples  :: String
    , regularization    :: String 
    , regularization2   :: String 
    , splits            :: String 
    , takes             :: String 
    , metric            :: String
    , label_column      :: Maybe Int
    , data_file         :: Maybe String 
    , verbose           :: Bool
    } 
    deriving (Show, Data, Typeable)

sample = Params 
    { k                 = 1 &= help "number of nearest neighbors to find" 
    , folds             = 2 &= help "number of folds for crossvalidation"
    , trials            = 10 &= help "number of trials to repeat crossvalidation"
    , param_nummetricsamples  = "10000" &= help "number of training samples for the metric learner"
    , regularization    = "[0.01]" &= help "regularization rate for metric learning"
    , regularization2   = "[0]" &= help "regularization rate for metric learning (2nd param)"
    , splits            = "[1]" &= help "splits for monoid test"
    , takes             = "[1]" &= help "how many splits to actually use"
    , metric            = "[mega]" &= help "metric to use for nearest neighbor classificaiton"
    , label_column      = Nothing &= help "column of the label in data_file"
    , data_file         = def &= help "reference data set in CSV format" &= typFile
    , verbose           = True &= help "print tree statistics (takes some extra time)"
    }
    &= summary "HLearn k-nearest neighbor classifier, version 1.0"

readParamList :: forall a. Read a => String -> [a]
readParamList str = case readMaybe str :: Maybe a of
    Just x -> [x]
    Nothing -> case readMaybe str :: Maybe [a] of
        Just xs -> xs
        Nothing -> error $ "readList failed to parse: "++str

-------------------------------------------------------------------------------
-- main

main = do

    -----------------------------------
    -- validate command line args

    params <- cmdArgs sample

    let numMetricSamplesL = readParamList $ param_nummetricsamples params :: [Int]
    print $ numMetricSamplesL

    let filename = case data_file params of
            Just str -> str
            Nothing -> error "must specify a data file"

    let label_index = case label_column params of
            Just x -> x
            Nothing -> error "must specify a label column"
    
    let metricLstr = words $ tail $ init $ map (\x -> if x==',' then ' ' else x) $  metric params :: [String]
        regularizationL = readParamList $ regularization params :: [Double]
        regularization2L = readParamList $ regularization2 params :: [Double]
        splitsL = readParamList $ splits params :: [Int]
        takesL = readParamList $ takes params :: [Int]
        
    print metricLstr
    let metricL = join $ map go [ (a,b) | a <- metricLstr, b <- regularizationL ]
        go (a,b) = case a of
            "mega" -> [("Mega "++show b++" "++show c, MetricBox $ (mkMega b c
                                    :: [(Double, VU.Vector Double)] -> Mega (1/1) (VU.Vector Double)))
                      | c<-regularization2L
                      ]
            "lego" -> [("Lego "++show b, MetricBox $ train_LegoPaper 1 b)]
            "lMon1" -> [("LegoMonoid1 "++show b++"; splits="++show c++"; takes="++show d, MetricBox $ train_LegoMonoid mappend1 c d b)
                       | c <- splitsL, d <- takesL ]
            "lMon2" -> [("LegoMonoid2 "++show b++"; splits="++show c++"; takes="++show d, MetricBox $ train_LegoMonoid mappend1 c d b)
                       | c <- splitsL, d <- takesL ]
            "itml" -> [("ITML "++show b, MetricBox $ train_ITML b)]
            otherwise -> error $ a++" is not a known metric"

    -----------------------------------
    -- load data

    xse :: Either String (V.Vector (V.Vector String))  
        <- timeIO "loading reference dataset" $ fmap (decode HasHeader) $ BS.readFile filename
    xs <- case xse of 
        Right rs -> return rs
        Left str -> error $ "failed to parse CSV file " ++ filename ++ ": " ++ take 1000 str

    let numdp = VG.length xs
    let numdim = VG.length $ xs VG.! 0
    let numlabels = Set.size $ Set.fromList $ VG.toList $ VG.map (VG.! label_index) xs

    if verbose params
        then do 
            hPutStrLn stderr "  dataset info:"
            hPutStrLn stderr $ "    num dp:     " ++ show numdp
            hPutStrLn stderr $ "    num dim:    " ++ show numdim
            hPutStrLn stderr $ "    num labels: " ++ show numlabels
        else return ()

    -----------------------------------
    -- convert to right types

    let ys = VG.map (\x -> MaybeLabeled   
            { label = Just $ x VG.! label_index
            , attr = VG.convert ( 
                VG.map read $ VG.fromList $ VG.toList $ VG.take label_index x <> VG.drop (label_index+1) x
                :: V.Vector Double
                )
            })
            xs
            :: V.Vector DP 

    -----------------------------------
    -- perform tests

    forM numMetricSamplesL $ \nummetricsamples -> do
    forM metricL $ \(msg,trainmetric) -> do

        hPutStrLn stderr $ replicate 80 '-'
        hPutStrLn stderr $ "-- Test: "++show msg++"; nummetricsamples="++show nummetricsamples
        hPutStrLn stderr $ replicate 80 '-'
        
        cvraw <- forM [1..trials params] $ \i -> 
            timeIO ("iteration "++show i) $ evalRandIO $ cv_legit_MetricBox
                (kfold $ folds params)
                errorRate
                ys 
                trainmetric
                nummetricsamples
                (train :: F.Foldable container => container DP -> 
                    KNearestNeighbor (AddUnit (CoverTree' (13/10) V.Vector V.Vector) ()) 4
                        (MaybeLabeled String (L2 VU.Vector Double)))

        let cv = reduce cvraw
        putStrLn $ msg ++ "    " ++ show nummetricsamples ++ "    " ++ show (mean cv)
        hFlush stdout

        hPutStrLn stderr $   "mean:   "++show (mean cv)
        hPutStrLn stderr $   "stddev: "++show (sqrt $ variance cv)
        hPutStrLn stderr ""
        hPutStrLn stderr $   "meanL: "++show (map mean cvraw)
        hPutStrLn stderr $   "varL:  "++show (map variance cvraw)

--         if verbose params
--             then putStrLn $ "\nresults_raw: " ++ show cvraw
--             else return ()

-------------------------------------------------------------------------------

printTreeStats str t = do
    putStrLn (str++" stats:")
    putStr (str++"  stNumDp..............") >> hFlush stdout >> putStrLn (show $ stNumDp t) 
    putStr (str++"  stNumNodes...........") >> hFlush stdout >> putStrLn (show $ stNumNodes t) 
    putStr (str++"  stNumLeaves..........") >> hFlush stdout >> putStrLn (show $ stNumLeaves t) 
    putStr (str++"  stNumGhosts..........") >> hFlush stdout >> putStrLn (show $ stNumGhosts t) 
    putStr (str++"  stNumGhostSingletons.") >> hFlush stdout >> putStrLn (show $ stNumGhostSingletons t) 
    putStr (str++"  stNumGhostLeaves.....") >> hFlush stdout >> putStrLn (show $ stNumGhostLeaves t) 
    putStr (str++"  stNumGhostSelfparent.") >> hFlush stdout >> putStrLn (show $ stNumGhostSelfparent t) 
    putStr (str++"  stAveGhostChildren...") >> hFlush stdout >> putStrLn (show $ mean $ stAveGhostChildren t) 
    putStr (str++"  stMaxChildren........") >> hFlush stdout >> putStrLn (show $ stMaxChildren t) 
    putStr (str++"  stAveChildren........") >> hFlush stdout >> putStrLn (show $ mean $ stAveChildren t) 
    putStr (str++"  stMaxDepth...........") >> hFlush stdout >> putStrLn (show $ stMaxDepth t) 
    putStr (str++"  stNumSingletons......") >> hFlush stdout >> putStrLn (show $ stNumSingletons t) 
    putStr (str++"  stExtraLeaves........") >> hFlush stdout >> putStrLn (show $ stExtraLeaves t) 

    putStrLn (str++" properties:")
    putStr (str++"  covering...............") >> hFlush stdout >> putStrLn (show $ property_covering $ UnitLift t) 
    putStr (str++"  leveled................") >> hFlush stdout >> putStrLn (show $ property_leveled $ UnitLift t) 
    putStr (str++"  separating.............") >> hFlush stdout >> putStrLn (show $ property_separating $ UnitLift t)
    putStr (str++"  maxDescendentDistance..") >> hFlush stdout >> putStrLn (show $ property_maxDescendentDistance $ UnitLift t) 

    putStrLn ""


-------------------------------------------------------------------------------
--

data MetricBox = forall metric. (MahalanobisMetric metric, Double~Ring metric) => 
        MetricBox ([(Double,VU.Vector Double)] -> metric)

cv_legit_MetricBox :: 
    ( HomTrainer model
    , Classifier model
    , RandomGen g
    , Eq (Datapoint model)
    , Eq (Label (Datapoint model))
    , F.Foldable container
    , Functor container
    , Datapoint model ~ DP
    ) => SamplingMethod 
      -> LossFunction 
      -> container (Datapoint model) 
      -> MetricBox
      -> Int
      -> ([DP] -> model)
      -> Rand g (Normal Double Double)
cv_legit_MetricBox genfolds loss xs (MetricBox trainmetric) nummetric trainmodel = do
    xs' <- genfolds $ F.toList xs
    return $ train $ do
        testset <- xs'
        let trainingset = concat $ filter (/=testset) xs'
            metric = trainmetric $ genMetricConstraints (V.fromList $ F.toList trainingset) nummetric
            trainingset' = fmap (\dp -> MaybeLabeled (label dp) (applyMahalanobis metric $ attr dp)) trainingset
            testset' = fmap (\dp -> MaybeLabeled (label dp) (applyMahalanobis metric $ attr dp)) testset 
        let model = trainmodel trainingset'
        return $ loss model testset'

cvnormal :: 
    ( HomTrainer model
    , Classifier model
    , RandomGen g
    , Eq (Datapoint model)
    , Eq (Label (Datapoint model))
    , F.Foldable container
    ) => SamplingMethod 
      -> LossFunction 
      -> container (Datapoint model) 
      -> ([Datapoint model] -> model)
      -> Rand g (Normal Double Double)
cvnormal genfolds loss xs trainmodel = do
    xs' <- genfolds $ F.toList xs
    return $ train $ do
        testset <- xs'
        let trainingset = concat $ filter (/=testset) xs'
        let model = trainmodel trainingset
        return $ loss model testset

cv_legit :: 
    ( HomTrainer model
    , Classifier model
    , MahalanobisMetric metric
    , RandomGen g
    , Eq (Datapoint model)
    , Eq (Label (Datapoint model))
    , F.Foldable container
    , Functor container
    , Double ~ Ring metric
    , Datapoint model ~ DP
    ) => SamplingMethod 
      -> LossFunction 
      -> container (Datapoint model) 
      -> ([(Double,VU.Vector Double)] -> metric)
      -> Int
      -> ([DP] -> model)
      -> Rand g (Normal Double Double)
cv_legit genfolds loss xs trainmetric nummetric trainmodel = do
    xs' <- genfolds $ F.toList xs
    return $ train $ do
        testset <- xs'
        let trainingset = concat $ filter (/=testset) xs'
            metric = trainmetric $ genMetricConstraints (V.fromList $ F.toList xs) nummetric
            trainingset' = fmap (\dp -> MaybeLabeled (label dp) (applyMahalanobis metric $ attr dp)) trainingset
            testset' = fmap (\dp -> MaybeLabeled (label dp) (applyMahalanobis metric $ attr dp)) testset 
        let model = trainmodel trainingset'
        return $ loss model testset'

cv :: 
    ( HomTrainer model
    , Classifier model
    , MahalanobisMetric metric
    , RandomGen g
    , Eq (Datapoint model)
    , Eq (Label (Datapoint model))
    , F.Foldable container
    , Functor container
    , Double ~ Ring metric
    , Datapoint model ~ DP
    ) => SamplingMethod 
      -> LossFunction 
      -> container (Datapoint model) 
      -> ([(Double,VU.Vector Double)] -> metric)
      -> Int
      -> ([DP] -> model)
      -> Rand g (Normal Double Double)
cv genfolds loss xs trainmetric nummetric trainmodel = do
    xs' <- genfolds $ F.toList xsmetric
    return $ train $ do
        testset <- xs'
        let trainingset = concat $ filter (/=testset) xs'
        let model = trainmodel trainingset
        return $ loss model testset
    where
        xsmetric = fmap (\dp -> MaybeLabeled (label dp) (applyMahalanobis metric $ attr dp)) xs
        metric = trainmetric $ genMetricConstraints (V.fromList $ F.toList xs) nummetric

genClassifier :: 
    ( MahalanobisMetric metric
    , Ring metric ~ Double
    ) => ([(Double,VU.Vector Double)] -> metric)
      -> Int
      -> ([DP] -> classifier)
      -> [DP]
      -> classifier
genClassifier trainmetric n traintree dps = traintree dps'
    where
        dpsv = V.fromList dps
        dps' = map (\dp -> MaybeLabeled (label dp) (applyMahalanobis metric $ attr dp)) dps
        metric = trainmetric $ genMetricConstraints dpsv n

genMetricConstraints :: VG.Vector vec DP => vec DP -> Int -> [(Double, VU.Vector Double)]
genMetricConstraints dps n = flip evalRand (mkStdGen seed) $ replicateM n genConstraint
    where
        seed=0
        numdp = VG.length dps

        distances :: MetricSpace dp => [dp] -> [Ring dp]
        distances [] = []
        distances (x:xs) = map (distance x) xs ++ distances xs

--         distances (x1:x2:x3:xs) = distance x1 x2:distance x1 x3: distances (x2:x3:xs)
--         distances _ = []

        dps_shuffle = flip evalRand (mkStdGen $ seed+100000) $ shuffle $ VG.toList dps
        ys2 = sort . distances $ map (L2 . attr) dps_shuffle
        p05 = ys2 !! (floor $ 0.05 * fromIntegral (length ys2))
        p95 = ys2 !! (floor $ 0.95 * fromIntegral (length ys2))

        genConstraint = do
            i <- getRandomR (0,numdp-1)
            j <- getRandomR (0,numdp-1)
            let targetdist = if label (dps VG.! i) == label (dps VG.! j)
                    then p05
                    else p95
            let dp = VG.zipWith (-) (attr $ dps VG.! i) (attr $ dps VG.! j)
            return (targetdist,unL2 dp)

data MetricNN metric k dp = MetricNN 
    { knnmetric :: metric 
    , knntree :: KNearestNeighbor (AddUnit (CoverTree' (13/10) V.Vector V.Vector) ()) k dp
    }

instance Monoid (MetricNN metric k dp) where
    mempty = undefined
    mappend = undefined

instance
    ( dp ~ vec Double
    , VG.Vector vec Double
    , Ring (vec Double) ~ Double
    , Ord (vec Double)
    , Double ~ Ring metric
    , MahalanobisMetric metric
    , HomTrainer metric
    , MetricSpace dp
    , Datapoint metric ~ dp 
    , Show (vec Double)
    ) => HomTrainer (MetricNN metric k dp) 
        where

    type Datapoint (MetricNN metric k dp) = dp
    train dps = MetricNN 
        { knnmetric = metric
        , knntree = train dps'
        }
        where
            metric = train dps
            dps' = map (applyMahalanobis metric) $ F.toList dps
