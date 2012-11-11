{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module HLearn.Models.Distributions.Common
    ( Distribution(..)
    , module Data.Number.LogFloat
    )
    where

import Control.Monad.Random
import Data.Number.LogFloat
import HLearn.Algebra

-------------------------------------------------------------------------------
-- Distribution
          
{-class (Monoid dist) => DistributionEstimator dist intype where
    trainSamples :: [intype] -> dist
    trainSamples xs = foldl1 mappend $ map train1sample xs
    
    train1sample :: intype -> dist
    train1sample dp = add1sample mempty dp
    
    add1sample :: dist -> intype -> dist-}
    
-- instance (DistributionEstimator dist (Weighted label)) => DistributionEstimator dist label where
--     add1sample dist label = add1sample dist (label,1::Double)
    
class Distribution dist sample probtype | dist -> sample{-, dist -> probtype-} where
    pdf :: dist -> sample -> probtype 
    cdf :: dist -> sample -> probtype 
    cdfInverse :: dist -> probtype -> sample
    mean :: dist -> sample
    
    drawSample :: (RandomGen g) => dist -> Rand g sample
