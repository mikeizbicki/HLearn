{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Metric spaces are mathematical structures that have a notion of distance between objects.  See wikipedia for more information: <https://en.wikipedia.org/wiki/Metric_space>
module HLearn.Algebra.Structures.MetricSpace
    where
          
import Data.List
import Data.Monoid
import HLearn.Algebra.Structures.Modules
                    
-------------------------------------------------------------------------------
-- MetricSpaces

-- | We assume that the MetricSpace on s is compatible with the ordering on s
class (HasRing s) => MetricSpace s where
    distance :: s -> s -> Ring s
    
-------------------------------------------------------------------------------
-- Norms

class (HasRing m, Ord (Ring m)) => Norm m where
    magnitude :: m -> Ring m

-- class (Module m, MetricSpace m) => Norm m where
--     {-# INLINE magnitude #-}
--     magnitude :: m -> Ring m
--     magnitude m = distance m mempty

-- instance (Module m, MetricSpace m) => Norm m where
          