{-# LANGUAGE ScopedTypeVariables,TemplateHaskell,DeriveDataTypeable,DataKinds,FlexibleInstances,TypeFamilies,RankNTypes,BangPatterns,FlexibleContexts,StandaloneDeriving,GeneralizedNewtypeDeriving,TypeOperators #-}

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Data.Csv
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import System.Console.CmdArgs.Implicit
import System.Environment
import System.IO

import Debug.Trace
import Diagrams.TwoD.Size
import Diagrams.Backend.SVG

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
import HLearn.Metrics.Mahalanobis.Lego
import HLearn.Models.Distributions

import Utils

type DP = MaybeLabeled String (L2 V.Vector Double)

instance Num r => HasRing (V.Vector r) where
    type Ring (V.Vector r) = r

data Params = Params
    { k :: Int
    , label_column :: Maybe Int
    , data_file :: Maybe String 
    , verbose :: Bool
    } 
    deriving (Show, Data, Typeable)

sample = Params 
    { k              = 1 &= help "number of nearest neighbors to find" 
    , label_column   = Nothing &= help "column of the label in data_file"
    , data_file      = def &= help "reference data set in CSV format" &= typFile
    , verbose        = False &= help "print tree statistics (takes some extra time)"
    }
    &= summary "HLearn k-nearest neighbor classifier, version 1.0"


main = do
    -- cmd line args
    params <- cmdArgs sample

--     case k params of 
--         1 -> runit params (undefined :: KNearestNeighbor (AddUnit (CoverTree' (2/1)) ()) 1 DP)
--         otherwise -> error "specified k value not supported"
-- 
-- -- {-# SPECIALIZE runit :: Params -> Tree -> IO ()#-}
-- runit :: forall k tree base dp ring model. 
--     ( MetricSpace dp
--     , Ord dp
--     , SingI k
--     , Show dp
--     , Show (Ring dp)
--     , NFData dp
--     , NFData (Ring dp)
--     , RealFloat (Ring dp)
--     , FromRecord dp 
--     , VU.Unbox (Ring dp)
--     , dp ~ Datapoint model
--     , HomTrainer model
--     , Classifier model
--     , dp ~ DP
--     ) => Params -> model -> IO ()
-- runit params _ = do


    -----------------------------------
    -- validate command line args

    let filename = case data_file params of
            Just str -> str
            Nothing -> error "must specify a data file"

    let label_index = case label_column params of
            Just x -> x
            Nothing -> error "must specify a label column"

    -----------------------------------
    -- load data

    xse :: Either String (V.Vector (V.Vector String))  
        <- timeIO "loading reference dataset" $ fmap (decode False) $ BS.readFile filename
    xs <- case xse of 
        Right rs -> return rs
        Left str -> error $ "failed to parse CSV file " ++ filename ++ ": " ++ take 1000 str

    let numdp = V.length xs
    let numdim = VG.length $ xs V.! 0
    let numlabels = Set.size $ Set.fromList $ VG.toList $ VG.map (VG.! label_index) xs

    putStrLn "  dataset info:"
    putStrLn $ "    num dp:     " ++ show numdp
    putStrLn $ "    num dim:    " ++ show numdim
    putStrLn $ "    num labels: " ++ show numlabels

    -----------------------------------
    -- convert to right types

    let ys = VG.map (\x -> MaybeLabeled   
            { label = Just $ x VG.! label_index
            , attr = VG.map read $ VG.fromList $ VG.toList $ VG.take label_index x <> VG.drop (label_index+1) x
            })
            xs
            :: V.Vector DP 

    -----------------------------------
    -- distance percentiles

    let distances [] = []
        distances (x:xs) = map (distance x) xs ++ distances xs

    let ys2 = sort . distances $ map (L2 . attr) $ VG.toList ys
        p05 = ys2 !! (floor $ 0.05 * fromIntegral (length ys2))
        p95 = ys2 !! (floor $ 0.95 * fromIntegral (length ys2))
    
    putStrLn $ "0.05 = " ++ show p05
    putStrLn $ "0.95 = " ++ show p95

    -----------------------------------
    -- learn metric

    let maxp=90
        minres=10
        pmult=2
    let loop p = forM [1..10] $ \i -> timeIO ("iteration "++show i) $ do
        metricdata <- replicateM (20*numlabels^2) $ do
--         metricdata <- replicateM (minres+p*pmult) $ do
            i <- randomRIO (0,numdp-1)
            j <- randomRIO (0,numdp-1)
            let targetdist = if label (ys VG.! i) == label (ys VG.! j)
                    then p05
                    else p95
            let dp = unL2 $ VG.zipWith (-) (attr $ ys VG.! i) (attr $ ys VG.! j)
            return (targetdist,dp) 
--         timeIO "generating metric labels" $ return $ rnf metricdata

        let legoI1 = train metricdata :: Lego Identity (1/1) (V.Vector Double)
        let legoI2 = train metricdata :: Lego Identity (1/10) (V.Vector Double)
        let legoI3 = train metricdata :: Lego Identity (1/100) (V.Vector Double)
        let legoI4 = train metricdata :: Lego Identity (1/1000) (V.Vector Double)
        let legoI5 = train metricdata :: Lego Identity (1/10000) (V.Vector Double)
        let legoI6 = train metricdata :: Lego Identity (1/100000) (V.Vector Double)
        let legoI7 = train metricdata :: Lego Identity (1/1000000) (V.Vector Double)
        let legoI8 = train metricdata :: Lego Identity (1/10000000) (V.Vector Double)
        let legoI9 = train metricdata :: Lego Identity (1/100000000) (V.Vector Double)
        let legoI0 = train metricdata :: Lego Identity (0/1) (V.Vector Double)

        let legoM1 = train metricdata :: Lego InvCovar (1/1) (V.Vector Double)
        let legoM2 = train metricdata :: Lego InvCovar (1/10) (V.Vector Double)
        let legoM3 = train metricdata :: Lego InvCovar (1/100) (V.Vector Double)
        let legoM4 = train metricdata :: Lego InvCovar (1/1000) (V.Vector Double)
        let legoM5 = train metricdata :: Lego InvCovar (1/10000) (V.Vector Double)
        let legoM6 = train metricdata :: Lego InvCovar (1/100000) (V.Vector Double)
        let legoM7 = train metricdata :: Lego InvCovar (1/1000000) (V.Vector Double)
        let legoM8 = train metricdata :: Lego InvCovar (1/10000000) (V.Vector Double)
        let legoM9 = train metricdata :: Lego InvCovar (1/100000000) (V.Vector Double)
        let legoM0 = train metricdata :: Lego InvCovar (0/1) (V.Vector Double)

        let mahal = train (VG.map (unL2 . attr) ys) :: MahalanobisParams (V.Vector Double)


        -----------------------------------
        -- cross-validation

        let docv zs = evalRandIO $ crossValidate 
                (kfold 2)
                errorRate
                zs (undefined :: KNearestNeighbor (AddUnit (CoverTree' (2/1)) ()) 4 
                                               (MaybeLabeled String (Mahalanobis (V.Vector Double)))) 

        x_legoI1 <- docv $ zs ys legoI1
        x_legoI2 <- docv $ zs ys legoI2
        x_legoI3 <- docv $ zs ys legoI3
        x_legoI4 <- docv $ zs ys legoI4
        x_legoI5 <- docv $ zs ys legoI5
        x_legoI6 <- docv $ zs ys legoI6
        x_legoI7 <- docv $ zs ys legoI7
        x_legoI8 <- docv $ zs ys legoI8
        x_legoI9 <- docv $ zs ys legoI9
        x_legoI0 <- docv $ zs ys legoI0

        x_legoM1 <- docv $ zs ys legoM1
        x_legoM2 <- docv $ zs ys legoM2
        x_legoM3 <- docv $ zs ys legoM3
        x_legoM4 <- docv $ zs ys legoM4
        x_legoM5 <- docv $ zs ys legoM5
        x_legoM6 <- docv $ zs ys legoM6
        x_legoM7 <- docv $ zs ys legoM7
        x_legoM8 <- docv $ zs ys legoM8
        x_legoM9 <- docv $ zs ys legoM9
        x_legoM0 <- docv $ zs ys legoM0

        x_mahal <- docv $ zs ys mahal
        x_id <- docv $ VG.map (\y -> MaybeLabeled
            { label = label y
            , attr = mkIdentity $ unL2 $ attr y
            })
            ys

        let cv = 
                [ x_legoI1
                , x_legoI2
                , x_legoI3
                , x_legoI4
                , x_legoI5
                , x_legoI6
                , x_legoI7
                , x_legoI8
                , x_legoI9
                , x_legoI0

                , x_legoM1
                , x_legoM2
                , x_legoM3
                , x_legoM4
                , x_legoM5
                , x_legoM6
                , x_legoM7
                , x_legoM8
                , x_legoM9
                , x_legoM0

                , x_mahal
                , x_id
                ]
        deepseq cv $ return cv

    cv <- fmap (map reduce . transpose) $ loop 10
    putStrLn $ show (map mean cv)
    putStrLn $ "  raw:  " ++ show cv
    return cv

--     forM (zip [1..] xs) $ \(p,cv) -> do
-- --         putStrLn $ show (p/maxp) ++ " " ++ show (mean cv)
--         putStrLn $ show (minres+p*pmult) ++ " " ++ show (mean cv)

    -- end
    putStrLn "end"

zs ys matrix = VG.map (\y -> MaybeLabeled
        { label = label y
        , attr = mkMahalanobis matrix $ unL2 $ attr y
        })
        ys
