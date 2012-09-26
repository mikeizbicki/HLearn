{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module HMine.Models.Distributions.Common
    where
          
import Control.Monad.Random
import Data.Number.LogFloat
import Data.Semigroup

-------------------------------------------------------------------------------
-- Distribution
          
class (Monoid dist) => DistributionEstimator dist intype where
    trainSamples :: [intype] -> dist
    trainSamples xs = foldl1 mappend $ map train1sample xs
    
    train1sample :: intype -> dist
    train1sample dp = add1sample mempty dp
    
    add1sample :: dist -> intype -> dist
    
-- instance (DistributionEstimator dist (Weighted label)) => DistributionEstimator dist label where
--     add1sample dist label = add1sample dist (label,1::Double)
    
class Distribution dist outtype | dist -> outtype where
    pdf :: dist -> outtype -> LogFloat
    cdf :: dist -> outtype -> LogFloat
    cdfInverse :: dist -> LogFloat -> outtype
    mean :: dist -> outtype
    
    drawSample :: (RandomGen g) => dist -> Rand g outtype
    drawSample dist = do
        x <- getRandomR (0,1)
        return $ cdfInverse dist $ logFloat (x::Double)

class IntersectableDistribution dist datatype where
    intersection :: dist -> dist -> [datatype]
    intersection d1 d2 = intersectionScaled (1,d1) (1,d2)
    
    intersectionScaled :: (Double,dist) -> (Double,dist) -> [datatype]
