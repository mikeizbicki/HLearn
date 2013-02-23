{-# LANGUAGE DataKinds #-}

import Criterion.Main
import Statistics.Distribution.Normal

import Statistics.Distribution.Normal
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as G

import Data.Array.Unboxed

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Distributions.Moments
import HLearn.Models.Distributions.MultiNormal
-- import qualified HLearn.Models.Distributions.GaussianOld as GO
-- import qualified HLearn.Models.Distributions.GaussianOld2 as GO2

import qualified Control.ConstraintKinds as CK


-- comparing different types

{-size = 10^6
main = defaultMain
    [ bench "Moments Double" $ nf (train :: [Double] -> Moments Double) [(0::Double)..size]
    ]-}
size = 10^6
main = defaultMain
    [ bench "HLearn-Gaussian" $ whnf
        (train :: VU.Vector Double -> Gaussian Double)
        (VU.enumFromN (0::Double) size)
    , bench "HLearn-Moments" $ whnf
        ((train :: VU.Vector Double -> Moments Double))
        (VU.enumFromN (0::Double) size)
    , bench "HLearn-MultiNormal" $ whnf
        ((train :: [UArray Int Double] -> MultiNormal Double 1))
        $ map (\x -> listArray (0,0) [x]) [(0::Double)..fromIntegral size]
    ]
-- size = 10^6
-- main = defaultMain
--     [ bench "Moments 2 Double" $ nf (train :: V.Vector Double -> Moments Double) (V.enumFromN (0::Double) size)
--     , bench "Moments 2 Float" $ nf (train :: V.Vector Float -> Moments Float) (V.enumFromN (0::Float) size)
--     , bench "Moments 2 Rational" $ nf (train :: V.Vector Rational -> Moments Rational) (V.enumFromN (0::Rational) size)
--     ]

{-main = defaultMain 
    [ bench "HLearn-Gaussian" $ nf ((train :: VU.Vector Double -> Gaussian Double)) (VU.enumFromN (0::Double) size)
    , bench "HLearn-Gaussian-Parallel" $ whnf (parallel $ (train :: VU.Vector Double -> Gaussian Double)) (VU.enumFromN (0::Double) size)
--     , bench "HLearn-Gaussian-List" $ nf (train :: [Double] -> Gaussian Double) [0..fromIntegral size]
    , bench "statistics-Gaussian" $ whnf (normalFromSample . VU.enumFromN 0) (size)
    ]-}
    
    {-bench "batch train [] 1e6" $ nf ((batch train) GaussianParams) [0..1e6::Double]
--     , bench "batch train V 1e6"  $ nf ((batch train) GaussianParams) (V.enumFromN (0::Double) (10^6))
--     , bench "batch train VU 1e6" $ nf ((batch train) GaussianParams) (VU.enumFromN (0::Double) [0..1e6::Double])
--     , bench "parallel2 batch train [] 1e6" $ nf ((parallel $ batch train) GaussianParams) (V.enumFromN (0::Double) (10^7))
    ,-} 
--     bench "Parallel GaussianOld2" $ nf (parallel $ batch (train GO2.GaussianParams)) (VU.enumFromN (0::Double) (10^8))
--     , bench "GaussianOld2" $ nf (batch (train GO2.GaussianParams)) (VU.enumFromN (0::Double) (10^8))
--     bench "Parallel GaussianOld" $ nf (parallel $ batch (train GO.GaussianParams)) (VU.enumFromN (0::Double) (10^8))
--     , bench "GaussianOld" $ nf (batch (train GO.GaussianParams)) (VU.enumFromN (0::Double) (10^8))
--      bench "Parallel Gaussian2" $ nf (parallel $ batch (train GaussianParams)) (VU.enumFromN (0::Double) (10^8))
--     , bench "Gaussian2" $ nf (batch (train GaussianParams)) (VU.enumFromN (0::Double) (10^8))
    
    {-bench "Parallel Gaussian" $ nf (parallel $ batch (trainSG)) (VU.enumFromN (0::Double) (10^8))
    , bench "Gaussian" $ nf (batch (trainSG)) (VU.enumFromN (0::Double) (10^8))
    ,-} -- bench "Categorical" $ nf (train :: [String] -> Categorical String Double) (concat $ replicate (10^4) ["a","b"])
--     , bench "normalFromSample - 1e5" $ whnf (normalFromSample . VU.enumFromN 1) (10^8)

    
-- instance NFData NormalDistribution where
--     rnf (NormalDistribution a b c d) = deepseq a $ deepseq b $ deepseq c $ rnf d

-- parallel3 strat train = \modelparams datapoint ->
--     foldl1 (<>) $ map (CK.foldl1 (<>) . CK.fmap (train modelparams . CK.pure)) (CK.partition 2 datapoint)


-- test (ds1,ds2) = m1 <> m2
--     where
--         [m1,m2] = parMap rdeepseq ((batch train) GaussianParams) [ds1,ds2]
-- --        [m1,m2] = parMap rdeepseq ((batch train) GaussianParams) [ds1,ds2]
-- 
-- -- test (ds1,ds2) = runEval $ do
-- --     let [m1,m2] = {-parMap rdeepseq-} map (batch train GaussianParams) [ds1,ds2]
-- -- --     m1 <- rparWith rseq $ batch train GaussianParams ds1
-- -- --     m2 <- rparWith rseq $ batch train GaussianParams ds2
-- -- {-    let m1 = batch train GaussianParams ds1
-- --     let m2 = batch train GaussianParams ds2-}
-- --     return $ m1 <> m2
-- 
-- dsL = replicate 4 ds
-- -- dsL = [ds1,ds2]
-- ds = [0..1000000::Double]
-- ds1 = V.enumFromN (0::Double) (10^7)
-- ds2 = V.enumFromN (0.5::Double) (10^7)