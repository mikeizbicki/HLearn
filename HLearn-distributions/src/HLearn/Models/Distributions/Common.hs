{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module HLearn.Models.Distributions.Common
    ( PDF(..)
    , CDF(..)
    , nonoverlap
    )
    where

import Data.List
import HLearn.Algebra

-------------------------------------------------------------------------------
-- Distribution
    
class PDF dist dp prob | dist -> dp, dist -> prob where
    pdf :: dist -> dp -> prob 
    
-- | We use the same class for both discrete and continuous distributions.  Unfortunately, we cannot use the type classes from the 'statistics' package because we require more generalilty.
--
-- Every distribution has a Cummulative Distribution Function (CDF).  Not every distribution has the properties of the other type classes.
class CDF dist dp prob | dist -> dp, dist -> prob where
    cdf :: dist -> dp -> prob 
    cdfInverse :: dist -> prob -> dp

--     mean :: dist -> sample
--     drawSample :: (RandomGen g) => dist -> Rand g sample

-- class (Distribution dist dp prob) => DistributionOverlap dist dp prob where
--     overlap :: [dist] -> prob
    
-- instance DistributionOverlap dist Double prob where
--     overlap xs = fmap (sort . (flip pdf) [-10..10]) xs


nonoverlap :: (Enum prob, Fractional prob, Ord prob, PDF dist dp prob, CDF dist dp prob) => [dist] -> prob
nonoverlap xs = (sum scoreL)/(fromIntegral $ length scoreL)
    where
        scoreL = fmap (diffscore . reverse . sort . normalizeL) $ transpose $ fmap ((flip fmap) sampleL . pdf) xs
        samplelen = length sampleL
        sampleL = concat $ fmap sampleDist xs
        sampleDist dist = fmap (cdfInverse dist . (/100)) [1..99]
        
diffscore :: (Num prob) => [prob] -> prob
diffscore (x1:x2:xs) = x1-x2

weightedscore :: (Num prob) => [prob] -> prob
weightedscore = sum . fmap (\(i,v) -> (fromIntegral i)*v) . zip [0..]
        
normalizeL :: (Fractional a) => [a] -> [a]
normalizeL xs = fmap (/tot) xs
    where
        tot = sum xs
