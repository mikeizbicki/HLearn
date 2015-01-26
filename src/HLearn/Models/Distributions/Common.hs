{-# LANGUAGE EmptyDataDecls #-}

-- | This module contains the type classes for manipulating distributions.
--
-- We use the same classes for both discrete and continuous distributions.  Unfortunately, we cannot use the type classes from the 'statistics' package because we require more flexibility than they offer.

module HLearn.Models.Distributions.Common
    ( Datapoint
    -- * Type classes
    , CDF(..)
    , PDF(..)
    , Mean(..)
    , Variance(..)

    -- * Utility functions
--     , nonoverlap
--     , stddev
    )
    where

import SubHask
-- import Data.List
-- import HLearn.Algebra

type family Datapoint m

-------------------------------------------------------------------------------
-- Distribution

-- |  Technically, every distribution has a Cumulative Distribution Function (CDF), and so this type class should be merged with the "Distribution" type class.  However, I haven't had a chance to implement the CDF for most distributions yet, so this type class has been separated out.
class CDF dist where
    cdf :: dist -> Datapoint dist -> Scalar dist
    cdfInverse :: dist -> Scalar dist -> Datapoint dist

-- | Not every distribution has a Probability Density Function (PDF), however most distributions in the HLearn library do.  For many applications, the PDF is much more intuitive and easier to work with than the CDF.  For discrete distributions, this is often called a Probability Mass Function (PMF); however, for simplicity we use the same type class for both continuous and discrete data.
class PDF dist where
    pdf :: dist -> Datapoint dist -> Scalar dist

class Mean dist where
    mean :: dist -> Scalar dist

class Variance dist where
    variance :: dist -> Scalar dist

    stddev :: (Variance dist, Floating (Scalar dist)) => dist -> Scalar dist
    stddev = sqrt . variance

-------------------------------------------------------------------------------
-- Continuity

data Discrete
data Continuous

type family Continuity t :: *
type instance Continuity Int = Discrete
type instance Continuity Integer = Discrete
type instance Continuity Char = Discrete
type instance Continuity String = Discrete

type instance Continuity Float = Continuous
type instance Continuity Double = Continuous
type instance Continuity Rational = Continuous

-------------------------------------------------------------------------------
-- Utilities

-- | If you were to plot a list of distributions, nonoverlap returns the amount of area that only a single distribution covers.  That is, it will be equal to number of distributions - the overlap.
--
-- This function is used by the HomTree classifier.

{-
nonoverlap ::
    ( Enum (Scalar dist)
    , Field (Scalar dist)
    , Ord (Scalar dist)
    , PDF dist
    , CDF dist
    ) => [dist] -> Scalar dist
nonoverlap xs = (sum scoreL)/(fromIntegral $ length scoreL)
    where
        scoreL = map (diffscore . reverse . sort . normalizeL) $ transpose $ map ((flip map) sampleL . pdf) xs
        samplelen = length sampleL
        sampleL = concat $ map sampleDist xs
        sampleDist dist = map (cdfInverse dist . (/100)) [1..99]

diffscore :: Group prob => [prob] -> prob
diffscore (x1:x2:xs) = x1-x2

weightedscore :: Semigroup prob => [prob] -> prob
weightedscore = foldl1 (+) . map (\(i,v) -> (fromIntegral i)*v) . zip [0..]

normalizeL :: Field a => [a] -> [a]
normalizeL xs = map (/tot) xs
    where
        tot = foldl1 (+) xs
-}
