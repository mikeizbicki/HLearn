{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module HLearn.Models.Distributions.Common
    ( Distribution(..)
    )
    where

import HLearn.Algebra

-------------------------------------------------------------------------------
-- Distribution
    
-- | We use the same class for both discrete and continuous distributions.  Unfortunately, we cannot use the type classes from the 'statistics' package because we require more generalilty.
class Distribution dist dp prob | dist -> dp, dist -> prob where
    pdf :: dist -> dp -> prob 
--     cdf :: dist -> dp -> prob 
--     cdfInverse :: dist -> prob -> dp

--     mean :: dist -> sample
--     drawSample :: (RandomGen g) => dist -> Rand g sample
