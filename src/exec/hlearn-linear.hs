{-# LANGUAGE ScopedTypeVariables #-}

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
    let {filename = "../../datasets/uci/wpbc-mod2.csv"; label_index=0}
--     let {filename = "../../datasets/uci/wine.csv"; label_index=0}
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
    -- convert to right types

--     let m = train ys :: LogisticRegression (MaybeLabeled String (LA.Vector Double))
--     deepseq m $ print $ m
-- 
--     let res = flip evalRand (mkStdGen 100) $ crossValidate
--             (repeatExperiment 1 (kfold 10))
--             errorRate
--             ys
--             (undefined :: LogisticRegression (MaybeLabeled String (LA.Vector Double)))
-- 
--     putStrLn $ "mean = "++show (mean res)
--     putStrLn $ "var  = "++show (variance res)

--     let testM n f = validate
--             (numSamples n (kfold 10))
--             errorRate
--             ys
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
--                 let res_taylor = runtest n (withMonoid mappendTaylor 2)
--                 putStr $ show (mean res_taylor)++", "++show (variance res_taylor)++", "
--                 let res_ave = runtest n (withMonoid mappendAverage 2)
--                 putStrLn $ show (mean res_ave)++", "++show (variance res_ave)
--                 hFlush stdout
--             | n <- [100,200,300,400,500]
--             ]

    let repl ',' = ' '
        repl c = c

    sequence_ 
        [ do
            putStr $ show n++" "
            putStrLn $ map repl $ tail $ init $ show $ map mean $ foldl1 (zipWith (<>)) 
                [ flip evalRand (mkStdGen seed) $ monoidtest (VG.toList ys) n
                | seed <- [1..100]
                ]
            hFlush stdout
        | n <- [28..50]
--         | n <- [1,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280,290,300,310,320,330,340,350,360,370,380,390,400]
--         | n <- [1,50,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,850,900,950,1000]
--         | n <- map (*10) [1..5]
        ]

--     let testM n f = validate
--             (kfold 10)
--             errorRate
--             ys
--             (f train :: [MaybeLabeled String (LA.Vector Double)] 
--                                -> LogisticRegression (MaybeLabeled String (LA.Vector Double)))
-- 
--     let runtest n f = reduce [flip evalRand (mkStdGen i) $ testM n f | i <- [0,1]]
-- 
--     let tests = 
--             [ do 
--                 putStr $ show n++", "
--                 let res_taylor = runtest n (withMonoid mappendTaylor n)
--                 putStr $ show (mean res_taylor)++", "++show (variance res_taylor)++", "
--                 let res_ave = runtest n (withMonoid mappendAverage n)
--                 putStrLn $ show (mean res_ave)++", "++show (variance res_ave)
--                 hFlush stdout
--             | n <- [1..50]
--             ]
-- 
--     sequence_ tests

    putStrLn "done."

