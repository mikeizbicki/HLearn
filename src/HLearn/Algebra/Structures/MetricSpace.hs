-- | Metric spaces are mathematical structures that have a notion of distance between objects. See wikipedia for more information: <https://en.wikipedia.org/wiki/Metric_space>
module HLearn.Algebra.Structures.MetricSpace
    where
          
import Data.List
import qualified Data.Strict.Maybe as Strict
import Data.Monoid
import HLearn.Algebra.Structures.CanError
import HLearn.Algebra.Structures.Modules
                    
-------------------------------------------------------------------------------
-- classes

-- | We assume that the MetricSpace on s is compatible with the ordering on s
class
    ( Ord (Scalar s)
    , Fractional (Scalar s)
    , Real (Scalar s)
    , RealFrac (Scalar s)
    ) => MetricSpace s
        where

    distance :: s -> s -> Scalar s

    {-# INLINE isFartherThan #-}
    isFartherThan :: s -> s -> Scalar s -> Bool
    isFartherThan s1 s2 b = case isFartherThanWithDistance s1 s2 b of
        Strict.Nothing -> True
        Strict.Just _ -> False 

    {-# INLINE isFartherThanWithDistance #-}
    isFartherThanWithDistance :: s -> s -> Scalar s -> Strict.Maybe (Scalar s)
    isFartherThanWithDistance s1 s2 b = if dist > b
        then Strict.Nothing
        else Strict.Just $ dist
        where
            dist = distance s1 s2

    {-# INLINE isFartherThanWithDistanceCanError #-}
    isFartherThanWithDistanceCanError :: CanError (Scalar s) => s -> s -> Scalar s -> Scalar s
    isFartherThanWithDistanceCanError s1 s2 b = if dist > b
        then errorVal
        else dist
        where
            dist = distance s1 s2

class (Ord (Scalar m)) => Norm m where
    magnitude :: m -> Scalar m

---------------------------------------

class MetricSpace s => MkCentroid s where
    mkCentroid :: s -> s -> s

---------------------------------------

class MetricSpaceParams params space where
    
data MetricWrapper params dp = MetricWrapper
    { getdp :: dp
    , metricspaceparams :: params
    }

-------------------------------------------------------------------------------
-- instances

-- type instance Scalar (a,a) = a
-- 
-- instance (RealFrac a, Floating a) => MetricSpace (a,a) where
--     {-# INLINABLE distance #-}
--     distance (x1,y1) (x2,y2) = sqrt $ (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)
-- 
--     {-# INLINABLE isFartherThan #-}
--     isFartherThan (!x1,!y1) (!x2,!y2) !b = if pt1 > threshold
--         then True
--         else pt1 + (y1-y2)*(y1-y2) > threshold
--         where
--             {-# INLINE pt1 #-}
--             {-# INLINE threshold #-}
--             pt1 = (x1-x2)*(x1-x2)
--             threshold=b*b
-- 
-- instance HasScalar (Double,Double,Double,Double,Double) where
--     type Scalar (Double,Double,Double,Double,Double) = Double
-- 
-- instance MetricSpace (Double,Double,Double,Double,Double) where
--     distance (a0,a1,a2,a3,a4) (b0,b1,b2,b3,b4) = sqrt 
--         $ (a0+b0)*(a0+b0)
--         + (a1+b1)*(a1+b1)
--         + (a2+b2)*(a2+b2)
--         + (a3+b3)*(a3+b3)
--         + (a4+b4)*(a4+b4)

-------------------------------------------------------------------------------
-- quick check

property_isFartherThan :: MetricSpace dp => dp -> dp -> Scalar dp -> Bool
property_isFartherThan dp1 dp2 dist = (distance dp1 dp2 > abs dist) == isFartherThan dp1 dp2 (abs dist)

