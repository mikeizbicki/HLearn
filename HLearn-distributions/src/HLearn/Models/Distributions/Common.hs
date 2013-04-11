{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module HLearn.Models.Distributions.Common
    ( 
    -- * Type classes
    Distribution(..)
    , CDF(..)
    , PDF(..)
    , Mean(..)
    , Variance(..)
    
    -- * Utility functions
    , nonoverlap
    )
    where

import Data.List
import HLearn.Algebra

-------------------------------------------------------------------------------
-- Distribution

-- | We use the same class for both discrete and continuous distributions.  Unfortunately, we cannot use the type classes from the 'statistics' package because we require more flexibility than they offer.
class (HomTrainer dist) => Distribution dist where
    type Probability dist

-- |  Technically, every distribution has a Cumulative Distribution Function (CDF), and so this type class should be merged with the "Distribution" type class.  However, I haven't had a chance to implement the CDF for most distributions yet, so this type class has been separated out.
class (Distribution dist) => CDF dist where
-- class CDF dist dp prob | dist -> dp, dist -> prob where
    cdf :: dist -> Datapoint dist -> Probability dist
    cdfInverse :: dist -> Probability dist -> Datapoint dist

-- | Not every distribution has a Probability Density Function (PDF), however most distributions in the HLearn library do.  For many applications, the PDF is much more intuitive and easier to work with than the CDF.  For discrete distributions, this is often called a Probability Mass Function (PMF); however, for simplicity we use the same type class for both continuous and discrete data.
class (Distribution dist) => PDF dist where
    pdf :: dist -> Datapoint dist -> Probability dist


class (Distribution dist) => Mean dist where
    mean :: dist -> Probability dist
    
class (Distribution dist) => Variance dist where
    variance :: dist -> Probability dist


-- class PDF dist dp prob | dist -> dp, dist -> prob where
--     pdf :: dist -> dp -> prob 
    

--     mean :: dist -> sample
--     drawSample :: (RandomGen g) => dist -> Rand g sample

-- class (Distribution dist dp prob) => DistributionOverlap dist dp prob where
--     overlap :: [dist] -> prob
    
-- instance DistributionOverlap dist Double prob where
--     overlap xs = fmap (sort . (flip pdf) [-10..10]) xs


nonoverlap :: 
    ( Enum (Probability dist), Fractional (Probability dist), Ord (Probability dist)
    , PDF dist
    , CDF dist
    ) => [dist] -> Probability dist
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
