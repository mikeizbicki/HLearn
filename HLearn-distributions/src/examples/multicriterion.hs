{-# LANGUAGE DataKinds #-}

import Data.Array.Unboxed

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Distributions.MultiNormal

import qualified Data.Vector.Unboxed as V

import Criterion.Main

size = 10^3 :: Double
main = defaultMain
    [ bench "MultiCategorical Double 1" $ nf (train :: [UArray Int Double] -> MultiNormal Double 1) $ map (\x -> listArray (0,0) [x]) [1..size]
--     , bench "Gaussian Double" $ nf (train :: [Double] -> Gaussian Double) [(1::Double)..size]
    , bench "Gaussian Double" $ whnf (train :: V.Vector Double -> Gaussian Double) (V.enumFromN (0::Double) (floor size))
    ]