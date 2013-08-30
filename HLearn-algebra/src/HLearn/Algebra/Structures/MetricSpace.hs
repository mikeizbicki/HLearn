-- | Metric spaces are mathematical structures that have a notion of distance between objects.  See wikipedia for more information: <https://en.wikipedia.org/wiki/Metric_space>
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
    
    distanceFastBound :: s -> s -> Ring s -> Ring s
    distanceFastBound s1 s2 b = distance s1 s2

    distanceFastMono :: s -> s -> Ring s
    distanceFastMono = distance

class (HasRing m, Ord (Ring m)) => Norm m where
    magnitude :: m -> Ring m

-------------------------------------------------------------------------------
-- instances

