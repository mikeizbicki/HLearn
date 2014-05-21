{-# OPTIONS_GHC -fllvm -O2 -funbox-strict-fields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

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
-- import qualified Numeric.LinearAlgebra as LA

import Debug.Trace

import HLearn.Algebra
import qualified HLearn.Algebra.LinearAlgebra as LA
import HLearn.Evaluation.CrossValidation
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common
import HLearn.Models.Classifiers.LinearClassifier
import qualified HLearn.Optimization.GradientDescent as Recipe
import qualified HLearn.Optimization.Common as Recipe

withMonoid f n _train dps = F.foldl1 f $ map _train $ partition n dps

main = do
    
--     let {filename = "../../datasets/uci/haberman.data"; label_index=3}
--     let {filename = "../../datasets/uci/iris.data"; label_index=4}
--     let {filename = "../../datasets/uci/wdbc-mod2.csv"; label_index=0}
--     let {filename = "../../datasets/uci/wpbc-mod2.csv"; label_index=0}
    let {filename = "../../datasets/uci/wine.csv"; label_index=0}
--     let {filename = "../../datasets/uci/pima-indians-diabetes.data"; label_index=8}
--     let {filename = "../../datasets/uci/sonar.csv"; label_index=60}
--     let {filename = "../../datasets/uci/magic04.data"; label_index=10}
--     let {filename = "../../datasets/uci/spambase.data"; label_index=57}
--     let {filename = "../../datasets/uci/ionosphere.csv"; label_index=34}
--     let {filename = "../../datasets/uci/yeast-mod2.csv"; label_index=8}
--     let {filename = "../../datasets/uci/pendigits.train.data"; label_index=16}
--     let {filename = "../../datasets/uci/optdigits.train.data"; label_index=64}
--     let {filename = "../../datasets/uci/winequality-white.csv"; label_index=11}
        
    let verbose = True

    -----------------------------------
    -- load data

    xse :: Either String (V.Vector (V.Vector String))  
        <- trace "loading reference dataset" $ fmap (decode HasHeader) $ BS.readFile filename
    xs <- case xse of 
        Right rs -> return rs
        Left str -> error $ "failed to parse CSV file " ++ filename ++ ": " ++ take 1000 str

    let numdp = VG.length xs
    let numdim = VG.length $ xs VG.! 0
    let numlabels = Set.size $ Set.fromList $ VG.toList $ VG.map (VG.! label_index) xs

    if verbose 
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
                VG.map read $ VG.fromList $ (:) "1" $ VG.toList $ VG.take label_index x <> VG.drop (label_index+1) x
--                 VG.map read $ VG.fromList $ VG.toList $ VG.take label_index x <> VG.drop (label_index+1) x
                :: V.Vector Double
                )
            })
            xs
            :: V.Vector (MaybeLabeled String (LA.Vector Double))

    -----------------------------------
    -- runtests

    let repl ',' = ' '
        repl c = c

    sequence_ 
        [ do
            putStr $ show funny++" "
            putStrLn $ map repl $ tail $ init $ show $ map mean $ foldl1 (zipWith (<>)) 
                [ flip evalRand (mkStdGen seed) $ funnytest funny (VG.toList ys) n
                | seed <- [1..10]
                ]
            hFlush stdout
        | funny <- [ i/20 | i <- [1..40]] --[ 0.5,1,1.5]
        , n <- [1]
        ]

    putStrLn "done."

-------------------------------------------------------------------------------

funnytest funny dps n = do
    dps' <- shuffle dps
    xs' <- kfold 10 dps'
    return $ foldl1 (zipWith (<>)) $ map (map (train1dp :: Double -> Normal Double Double)) $ do
        testset <- xs'
        let trainingset = concat $ filter (/=testset) xs'
        return $ runtest_funny funny (partition n trainingset) testset

runtest_funny ::
    ( Scalar dp ~ Double
    , Labeled dp 
    , LA.Field (Scalar dp)
    , VectorSpace (Scalar dp)
    , Ord (Scalar dp)
    , Ord (Label dp)
    , Attributes dp ~ LA.Vector Double
    , Show (Label dp)
    , Show dp
    , Typeable dp
    , Typeable (Label dp)
    , Typeable (Attributes dp)
    , Ord dp
    ) => Scalar dp -> [[dp]] -> [dp] -> [Scalar dp]
runtest_funny funny dpsL testset = 
    [ 0/1/0
--     , errorRate lr1 testset
--     , errorRate lr2 testset
--     , errorRate lr3 testset
--     , errorRate lr4 testset
--     , errorRate lr5 testset
--     , errorRate lr6 testset
--     , errorRate lr7 testset
    , 0/1/0
    , errorRate funny1 testset
    , errorRate funny2 testset
    , errorRate funny3 testset
    , errorRate funny4 testset
    , errorRate funny5 testset
    , errorRate funny6 testset
--     , errorRate funny7 testset
    ]
    ++ [0/1/0]
    ++ [0/1/0]
    ++ [0/1/0]
--     ++ Map.elems (Map.mapWithKey go (weights lr1))
--     ++ [0/1/0]
--     ++ Map.elems (Map.mapWithKey go (weights lr2))
--     ++ [0/1/0]
--     ++ Map.elems (Map.mapWithKey go (weights lr3))
--     ++ [0/1/0]
--     ++ Map.elems (Map.mapWithKey go (weights lr4))
--     ++ [0/1/0]
--     ++ Map.elems (Map.mapWithKey go (weights lr5))
--     ++ [0/1/0]
--     ++ Map.elems (Map.mapWithKey go (weights lr6))
--     ++ [0/1/0]
--     ++ Map.elems (Map.mapWithKey go (weights lr7))
    ++ [0/1/0]
    ++ [0/1/0]
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights funny1))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights funny2))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights funny3))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights funny4))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights funny5))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights funny6))
    ++ [0/1/0]
--     ++ Map.elems (Map.mapWithKey go (weights funny7))
    
    where
        lrL1 = map (lrtrain 1e-2) dpsL
        lrL2 = map (lrtrain 1e-3) dpsL
        lrL3 = map (lrtrain 1e-4) dpsL
        lrL4 = map (lrtrain 1e-5) dpsL
        lrL5 = map (lrtrain 1e-6) dpsL
        lrL6 = map (lrtrain 1e-7) dpsL
        lrL7 = map (lrtrain 0) dpsL
        funnyL1 = map (funnytrain funny 1e-2) dpsL
        funnyL2 = map (funnytrain funny 1e-3) dpsL
        funnyL3 = map (funnytrain funny 1e-4) dpsL
        funnyL4 = map (funnytrain funny 1e-5) dpsL
        funnyL5 = map (funnytrain funny 1e-6) dpsL
        funnyL6 = map (funnytrain funny 1e-7) dpsL
        funnyL7 = map (funnytrain funny 0) dpsL
        lr1 = head lrL1
        lr2 = head lrL2
        lr3 = head lrL3
        lr4 = head lrL4
        lr5 = head lrL5
        lr6 = head lrL6
        lr7 = head lrL7
        funny1 = foldl1 mappendAverage funnyL1
        funny2 = foldl1 mappendAverage funnyL2
        funny3 = foldl1 mappendAverage funnyL3
        funny4 = foldl1 mappendAverage funnyL4
        funny5 = foldl1 mappendAverage funnyL5
        funny6 = foldl1 mappendAverage funnyL6
        funny7 = foldl1 mappendAverage funnyL7

        dps = concat dpsL

        go l (_,w0,_) = f w0
            where
                f w = sumOver dps $ \dp ->
                        logSumOfExp2 0 $ -(y dp * inner w (getAttributes dp))

                y dp = bool2num $ getLabel dp==l 


