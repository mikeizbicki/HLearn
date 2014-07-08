{-# OPTIONS_GHC -fllvm -O2 -funbox-strict-fields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Random
import Data.Functor
import Data.List hiding (partition)
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
import HLearn.Metrics.Lebesgue
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
--     let {filename = "../../datasets/uci/wine.csv"; label_index=0}
    let {filename = "../../datasets/uci/pima-indians-diabetes.data"; label_index=8}
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
    -- run test

    let repl ',' = ' '
        repl c = c

    sequence_ 
        [ do
            putStr $ show k++" "
            putStrLn $ map repl $ tail $ init $ show $ map mean $ foldl1 (zipWith (<>)) 
                [ flip evalRand (mkStdGen seed) $ rmdptest (VG.toList ys) n k
                | seed <- [1..100]
                ]
            hFlush stdout
        | n <- [2..50]
        , k <- [1000]
        ]

    putStrLn "done."

-------------------------------------------------------------------------------
-- test helpers

rmdptest dps n k = do
    dps' <- take (k*2) <$> shuffle dps
    xs' <- kfold 2 dps'
    return $ foldl1 (zipWith (<>)) $ map (map (train1dp :: Double -> Normal Double Double)) $ do
        testset <- xs'
        let trainingset = concat $ filter (/=testset) xs'
        return $ rmdphelper (partition n trainingset) testset

rmdphelper ::
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
    ) => [[dp]] -> [dp] -> [Scalar dp]
rmdphelper dpsL testset = 
    [ errorRate lr4 testset
    , errorRate lrAve4 testset
    , errorRate lrTaylor4 testset
    , errorRate lrUpper4 testset
    , 0/1/0
    , 0/1/0
    , 0/1/0
    ]
    ++ Map.elems (Map.mapWithKey (calcLoss dps) (weights lr4))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey (calcLoss dps) (weights lrAve4))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey (calcLoss dps) (weights lrTaylor4))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey (calcLoss dps) (weights lrUpper4))
    ++ [0/1/0,0/1/0,0/1/0]
    ++ (zipWithByList (+) $ map (\model -> Map.elems . Map.mapWithKey (calcLoss $ datapoints model) $ weights model) lrL4ub)
    ++ [0/1/0]
    ++ (zipWithByList min $ calcMaxList lrL4)
    ++ [0/1/0]
    ++ (zipWithByList max $ calcMaxList lrL4)
    ++ [0/1/0]
    ++ (zipWithByList min $ calcMaxList lrL4ub)
    ++ [0/1/0]
    ++ (zipWithByList max $ calcMaxList lrL4ub)
    ++ [0/1/0,0/1/0,0/1/0]
    ++ calcDistances min L2 lrAve4 lrL4
    ++ [0/1/0]
    ++ calcDistances max L2 lrAve4 lrL4
    ++ [0/1/0]
    ++ calcDistances min L1 lrAve4 lrL4
    ++ [0/1/0]
    ++ calcDistances max L1 lrAve4 lrL4
    ++ [0/1/0]
    ++ calcDistances min L2 lrTaylor4 lrL4
    ++ [0/1/0]
    ++ calcDistances max L2 lrTaylor4 lrL4
    ++ [0/1/0]
    ++ calcDistances min L1 lrTaylor4 lrL4
    ++ [0/1/0]
    ++ calcDistances max L1 lrTaylor4 lrL4
    ++ [0/1/0]
    ++ calcDistances min L2 lrUpper4 lrL4ub
    ++ [0/1/0]
    ++ calcDistances max L2 lrUpper4 lrL4ub
    ++ [0/1/0]
    ++ calcDistances min L1 lrUpper4 lrL4ub
    ++ [0/1/0]
    ++ calcDistances max L1 lrUpper4 lrL4ub
    ++ [0/1/0,0/1/0,0/1/0]
    ++ calcDistances max (L2) (lrL4ub !! 0) [lrL4ub !! 1]
    ++ [0/1/0]
    ++ calcDistances max (L1) (lrL4ub !! 0) [lrL4ub !! 1]
    where
        zero = LinearClassifier 
            { weights = Map.map (\w -> (0,w,NoTaylor)) $ zeroWeights $ head dpsL
            , datapoints = V.fromList $ F.toList $ head dpsL
            , reg=0
            }
--         lrL4 = map (lrtrain 1e-5) dpsL
--         lrL4ub = map (lrtrainub 1e-5) dpsL
--         lr4 = head lrL4
--         lrAve4 = foldl1 mappendAverage lrL4
--         lrTaylor4 = foldl1 mappendTaylor lrL4
--         lrUpper4 = foldl1 mappendTaylor lrL4ub

        lrL4 = lrL4ub
        lrL4ub = map (lrtrainub 1e-5) dpsL
        lr4 = head lrL4
        lrAve4 = foldl1 mappendTaylor lrL4
        lrTaylor4 = foldl1 mappendTaylor lrL4
        lrUpper4 = foldl1 mappendTaylor lrL4ub

        dps = concat dpsL

calcDistances f disttype m ms = zipWithByList f $ go ms
    where
        go [] = []
        go (x:xs) = (zipWith modeldistance (Map.elems $ weights m) (Map.elems $ weights x)) : go xs
        modeldistance (_,w0m,_) (_,w0x,_) = distance (disttype w0m) (disttype w0x)

calcLoss dps l (_,w0,_) = f w0
    where
        f w = sumOver dps $ \dp ->
                logSumOfExp2 0 $ -(y dp * inner w (getAttributes dp))
        y dp = bool2num $ getLabel dp==l 

calcMaxList :: forall dp.
    ( Labeled dp
    , Ord (Label dp)
    , Ord (Scalar dp)
    , InnerProduct (Attributes dp)
    , Eq (Label dp)
    , Attributes dp ~ LA.Vector (Scalar dp)
    , Typeable (Scalar dp)
    , Show (Scalar dp)
    ) => [ LinearClassifier dp ] -> [[Scalar dp]]
calcMaxList xs = go xs
    where
        dps = V.concat $ map datapoints xs

--         go (y:ys) = (zipWith (+) yweights ubweights) : go ys
--             where
--                 ubweights = undefined
--                 yweights = Map.elems $ Map.mapWithKey (calcLoss dps) (weights y)

        go [] = []
        go (y:ys) = weights : go ys
            where
                weights = Map.elems $ foldl1' (Map.unionWith (+)) $ map (taylor2function y) xs

--         taylor2function :: Attributes dp -> LinearClassifier dp -> Map.Map (Label dp) (Scalar dp)
        taylor2function model linearclassifier = foldl1 (Map.unionWith (+)) $ map f xs
            where
                f m  = Map.mapWithKey (\l (_,_,tay) -> apTaylor tay (pos l) + c l) $ weights m
                pos l = (fromJust $ Map.lookup l $ weights model)^._2
                c l = calcLoss (datapoints linearclassifier) l (fromJust $ Map.lookup l $ weights linearclassifier)

zipWithByList :: (a -> a -> a) ->  [[a]] -> [a]
zipWithByList f xs = go $ transpose xs
    where
        go [] = []
        go (y:ys) = foldl1' f y : go ys



apTaylor :: 
    ( VG.Vector v t
    , dp ~ v t
    , t ~ Scalar dp
    , t ~ Scalar t
    , v ~ LA.Vector
    , LA.Field t
    , VectorSpace t
    , Typeable t
    , Show t
    , Ord t
    ) => Taylor dp -> dp -> Scalar dp
apTaylor (Taylor b a) dp = inner (dp `LA.matProduct` a) dp + inner dp b
apTaylor NoTaylor _ = 0
