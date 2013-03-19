{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HLearn.Algebra.Structures.MetricSpace
    where
          
import Data.List
          
          
-- | We assume that the MetricSpace on s is compatible with the ordering on s
class MetricSpace r s | s -> r where
    dist :: s -> s -> r
    
instance MetricSpace Double Double where
    dist x y = abs $ x - y

-- instance MetricSpace String Int where