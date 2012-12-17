{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module HLearn.Models.Distributions.MultiGaussian
    where

import Data.Array
import Data.Semigroup
import GHC.TypeLists

import HLearn.Math.TypeClasses

data MultiGaussian (n :: Int) = MultiGaussian
    { dim :: Int
    , mean :: Array Int Double
    , covar :: Array (Int,Int) Double
    , n :: Int
    }
    
instance Monoid (MultiGaussian String) where
    mempty = undefined
    mappend = undefined
    
-- instance Distribution MultiGaussian Double where
--     add1sample :: dist -> datatype -> dist
--     pdf :: dist -> datatype -> LogFloat
--     cdf :: dist -> datatype -> LogFloat
--     cdfInverse :: dist -> LogFloat -> datatype
