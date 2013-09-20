-- | Metric spaces are mathematical structures that have a notion of distance between objects. See wikipedia for more information: <https://en.wikipedia.org/wiki/Metric_space>
module HLearn.Algebra.Structures.MetricSpace
    where
          
import Data.List
import Data.Monoid
import HLearn.Algebra.Structures.Modules
                    
-------------------------------------------------------------------------------
-- classes

-- | We assume that the MetricSpace on s is compatible with the ordering on s
class
    ( HasRing s
    , Ord (Ring s)
    , Fractional (Ring s)
    , Real (Ring s)
    , RealFrac (Ring s)
    ) => MetricSpace s
        where

    distance :: s -> s -> Ring s
    
    isFartherThan :: s -> s -> Ring s -> Bool
    isFartherThan s1 s2 b = distance s1 s2>b

    distanceFastMono :: s -> s -> Ring s
    distanceFastMono = distance

class (HasRing m, Ord (Ring m)) => Norm m where
    magnitude :: m -> Ring m

class MetricSpaceParams params space where
    
data MetricWrapper params dp = MetricWrapper
    { getdp :: dp
    , params :: params
    }

-------------------------------------------------------------------------------
-- instances

instance Num a => HasRing (a,a) where
    type Ring (a,a) = a

instance (RealFrac a, Floating a) => MetricSpace (a,a) where
    {-# INLINABLE distance #-}
    distance (x1,y1) (x2,y2) = sqrt $ (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)

    {-# INLINABLE isFartherThan #-}
    isFartherThan (!x1,!y1) (!x2,!y2) !b = if pt1 > threshold
        then True
        else pt1 + (y1-y2)*(y1-y2) > threshold
        where
            {-# INLINE pt1 #-}
            {-# INLINE threshold #-}
            pt1 = (x1-x2)*(x1-x2)
            threshold=b*b

    distanceFastMono (x1,y1) (x2,y2) = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)
         
