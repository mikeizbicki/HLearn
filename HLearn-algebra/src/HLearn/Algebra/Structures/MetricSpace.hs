{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Metric spaces are mathematical structures that have a notion of distance between objects.  See wikipedia for more information: <https://en.wikipedia.org/wiki/Metric_space>
module HLearn.Algebra.Structures.MetricSpace
    where
          
import Data.List
          
          
-- | We assume that the MetricSpace on s is compatible with the ordering on s
class MetricSpace r s | s -> r where
    distance :: s -> s -> r
    
instance MetricSpace Double Double where
    distance x y = abs $ x - y

-- instance MetricSpace String Int where