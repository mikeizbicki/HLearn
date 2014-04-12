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
import System.CPUTime

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
import HLearn.Models.Classifiers.KernelizedLinearClassifier hiding (DP)
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

    deepseq ys $ hPutStrLn stderr "deepseqing ys"

    -----------------------------------
    -- run tests

    let runtest f = flip evalRand (mkStdGen 100) $ validate_monoid
            (repeatExperiment 1 (kfold 10))
            errorRate
            ys
            (f (lrtrain 1e-4) :: [DP] -> LinearClassifier DP)
            (mappendTaylor)

    let test_nomappend trials k lambda = do
            (secdist,errdist) <- fmap reduce $ sequence
                [ timeIO $ do 
                    hPutStr stderr $ show n++", "
                    let res = flip evalRand (mkStdGen n) $ validate
                            (repeatExperiment 1 (kfold k))
                            errorRate
                            ys
                            (klrtrain lambda) --  :: [DP] -> LinearClassifier DP)
                    hPutStrLn stderr $ show (mean res)++", "++show (variance res)
                    return (train1dp (mean res) :: Normal Double Double)
                | n <- [1..1+trials-1]
                ]

            putStr $ "      " ++ show (mean secdist)
                  ++ "      " ++ show (variance secdist)
                  ++ "      " ++ show (mean errdist)
                  ++ "      " ++ show (variance errdist)
                  ++ "      " ++ "---"
            hFlush stdout

    let test_mappend trials k lambda _mappend = do
            (secdist,errdist) <- fmap reduce $ sequence
                [ timeIO $ do 
                    hPutStr stderr $ show n++", "
                    let res = flip evalRand (mkStdGen n) $ validate_monoid
                            (repeatExperiment 1 (kfold k))
                            errorRate
                            ys
                            (lrtrain lambda :: [DP] -> LinearClassifier DP)
                            _mappend
                    hPutStrLn stderr $ show (mean res)++", "++show (variance res)
                    return (train1dp (mean res) :: Normal Double Double)
                | n <- [1..1+trials-1]
                ]

            putStr $ "      " ++ show (mean secdist)
                  ++ "      " ++ show (variance secdist)
                  ++ "      " ++ show (mean errdist)
                  ++ "      " ++ show (variance errdist)
                  ++ "      " ++ "---"
            hFlush stdout
            
    let blanktest = putStr "     ---    ---    ---    ---    ---"

    let tests = 
            [ do
                putStr $ show k
                      ++ "      " ++ show lambda
                      ++ "      " ++ "---"
                let numtrials=1
                test_nomappend numtrials k lambda
--                 test_mappend numtrials k lambda mappendAverage
--                 blanktest
--                 test_mappend numtrials k lambda (reoptimize mappendAverage)
--                 test_mappend numtrials k lambda mappendTaylor
--                 blanktest
--                 test_mappend numtrials k lambda (reoptimize mappendTaylor)
                putStrLn ""
--             | k <- [2..50]
--             | k <- [5,50,100,150,200,250,300,350,400,450,500]
--             , lambda <- [0]
            | k <- [10]
--             , lambda <- [1e5,1e4,1e3,1e2,1e1,1,1e-1,1e-2,1e-3,1e-4,1e-5,1e-6,1e-7,1e-8,1e-9,1e-10,0]
            , lambda <- [1e-3]
            ]

    ioxs <- sequence tests

--     putStrLn "\n\nResults:"
--     print $ ioxs

    hPutStrLn stderr "done."

timeIO :: NFData nf => IO nf -> IO (Normal Double Double,nf)
timeIO ionf = do
    startTime <- getCPUTime
    nf <- ionf
    deepseq nf $ hPutStrLn stderr "timeIO deepseq"
    stopTime <- getCPUTime
    let sec= train1dp $ 1e-12 * fromIntegral (stopTime-startTime) :: Normal Double Double
    return (sec,nf)
