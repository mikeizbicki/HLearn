{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module HLearn.Models.Distributions.Common
    ( Distribution(..)
    , module Data.Number.LogFloat
    )
    where

import Control.Monad.Random
import Data.Number.LogFloat hiding (log)
import HLearn.Algebra

-------------------------------------------------------------------------------
-- Distribution
    
-- | We use the same class for both discrete and continuous distributions.  Unfortunately, we cannot use the type classes from the 'statistics' package because we require more generalilty.
class Distribution dist sample prob | dist -> sample where
    pdf :: dist -> sample -> prob 
    cdf :: dist -> sample -> prob 
    cdfInverse :: dist -> prob -> sample

--     mean :: dist -> sample
--     drawSample :: (RandomGen g) => dist -> Rand g sample
