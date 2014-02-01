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
import HLearn.Metrics.Mahalanobis.LegoPaper
import HLearn.Metrics.Mahalanobis.Mega
import HLearn.Models.Distributions

import Utils

type DP = MaybeLabeled String (L2 VU.Vector Double)

instance Num r => HasRing (V.Vector r) where type Ring (V.Vector r) = r
instance Num r => HasRing (VU.Vector r) where type Ring (VU.Vector r) = r

instance (NFData label, NFData attr) => NFData (MaybeLabeled label attr) where
    rnf (MaybeLabeled l a) = deepseq l $ rnf a

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
        <- timeIO "loading reference dataset" $ fmap (decode HasHeader) $ BS.readFile filename
    xs <- case xse of 
        Right rs -> return rs
        Left str -> error $ "failed to parse CSV file " ++ filename ++ ": " ++ take 1000 str

    let numdp = VG.length xs
    let numdim = VG.length $ xs VG.! 0
    let numlabels = Set.size $ Set.fromList $ VG.toList $ VG.map (VG.! label_index) xs

    putStrLn "  dataset info:"
    putStrLn $ "    num dp:     " ++ show numdp
    putStrLn $ "    num dim:    " ++ show numdim
    putStrLn $ "    num labels: " ++ show numlabels

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
--         metricdata <- replicateM (20*numlabels^2) $ do
        metricdata <- replicateM (10000) $ do
            i <- randomRIO (0,numdp-1)
            j <- randomRIO (0,numdp-1)
            let targetdist = if label (ys VG.! i) == label (ys VG.! j)
                    then p05
                    else p95
            let dp = unL2 $ VG.zipWith (-) (attr $ ys VG.! i) (attr $ ys VG.! j)
            return (targetdist,dp) 
--         timeIO "generating metric labels" $ return $ rnf metricdata

        let mega1000 = mkMega metricdata :: Mega (1000/1) (VU.Vector Double)
        let mega100 = mkMega metricdata :: Mega (100/1) (VU.Vector Double)
        let mega10 = mkMega metricdata :: Mega (10/1) (VU.Vector Double)
        let mega1 = mkMega metricdata :: Mega (1/1) (VU.Vector Double)
        let mega2 = mkMega metricdata :: Mega (1/10) (VU.Vector Double)
        let mega3 = mkMega metricdata :: Mega (1/100) (VU.Vector Double)
        let mega4 = mkMega metricdata :: Mega (1/1000) (VU.Vector Double)
        let mega5 = mkMega metricdata :: Mega (1/10000) (VU.Vector Double)
        let mega6 = mkMega metricdata :: Mega (1/100000) (VU.Vector Double)
        let mega7 = mkMega metricdata :: Mega (1/1000000) (VU.Vector Double)
        let mega0 = mkMega metricdata :: Mega (0/1) (VU.Vector Double)

        let legopaper1 = train_LegoPaper 1 metricdata :: LegoPaper  (VU.Vector Double)
        let legopaper2 = train_LegoPaper 0.1 metricdata :: LegoPaper  (VU.Vector Double)
        let legopaper3 = train_LegoPaper 0.01 metricdata :: LegoPaper  (VU.Vector Double)
        let legopaper4 = train_LegoPaper 0.001 metricdata :: LegoPaper  (VU.Vector Double)

        let mahal = train (VG.map (unL2 . attr) ys) :: MahalanobisParams (VU.Vector Double)

--         let tmpmodel = train (zs ys mega3) :: AddUnit (CoverTree' (13/10) V.Vector V.Vector) () 
--                                                       (MaybeLabeled String (L2 V.Vector Double))
--         let tmpmodel = train (zs ys mahal) :: AddUnit (CoverTree' (13/10) V.Vector V.Vector) () 
--                                                       (MaybeLabeled String (L2 V.Vector Double))
--         printTreeStats "reftree      " $ unUnit tmpmodel 

        -----------------------------------
        -- cross-validation

        let docv zs = evalRandIO $ crossValidate 
                (kfold 2)
                errorRate
                zs 
--                 (undefined :: NaiveNN 4 V.Vector (MaybeLabeled String (L2 VU.Vector Double)))
                (undefined :: KNearestNeighbor (AddUnit (CoverTree' (13/10) V.Vector V.Vector) ()) 4 
                                               (MaybeLabeled String ((L2 VU.Vector Double)))) 

        x_mega1000 <- docv $ zs ys mega1000 
        x_mega100 <- docv $ zs ys mega100 
        x_mega10 <- docv $ zs ys mega10
        x_mega1 <- docv $ zs ys mega1 
        x_mega2 <- docv $ zs ys mega2 
        x_mega3 <- docv $ zs ys mega3 
        x_mega4 <- docv $ zs ys mega4 
        x_mega5 <- docv $ zs ys mega5 
        x_mega6 <- docv $ zs ys mega6 
        x_mega7 <- docv $ zs ys mega7 
        x_mega0 <- docv $ zs ys mega0 

--         timeIO "deepseq mega3" $ return $ rnf mega3
--         timeIO "deepseq $ zs ys mega3" $ return $ rnf $ zs ys mega3
--         timeIO "deepseq x_mega3" $ return $ rnf x_mega3

        x_legopaper1 <- docv $ zs ys legopaper1 
        x_legopaper2 <- docv $ zs ys legopaper2 
        x_legopaper3 <- docv $ zs ys legopaper3 
        x_legopaper4 <- docv $ zs ys legopaper4 

        x_mahal <- docv $ zs ys mahal
        x_id <- docv $ VG.map (\y -> MaybeLabeled
            { label = label y
--             , attr = mkIdentity $ unL2 $ attr y
            , attr = attr y
            })
            ys

        let cv = 
                [ x_mega1000
                , x_mega100
                , x_mega10
                , x_mega1
                , x_mega2
                , x_mega3
                , x_mega4
                , x_mega5
                , x_mega6
                , x_mega7
--                 , x_mega0

                , x_legopaper1
                , x_legopaper2
                , x_legopaper3
                , x_legopaper4

                , x_mahal
                , x_id
                ]
        deepseq cv $ return cv

    putStrLn "\nresults:"
    cvraw <- loop 10
    let cv = (map reduce . transpose) cvraw
    putStrLn $   "mean: "++show (map mean cv)
    putStrLn $   "var:  "++show (map variance cv)

    putStrLn $ "\nresults_raw: " ++ show cvraw
    return cv

--     forM (zip [1..] xs) $ \(p,cv) -> do
-- --         putStrLn $ show (p/maxp) ++ " " ++ show (mean cv)
--         putStrLn $ show (minres+p*pmult) ++ " " ++ show (mean cv)

    -- end
    putStrLn "end"

zs ys matrix = VG.map (\y -> MaybeLabeled
        { label = label y
--         , attr = mkMahalanobis matrix $ unL2 $ attr y
        , attr = applyMahalanobis matrix $ attr y
        })
        ys



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
