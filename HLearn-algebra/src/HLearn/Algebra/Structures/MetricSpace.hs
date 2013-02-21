{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HLearn.Algebra.Structures.MetricSpace
    where
          
import Data.List
          
class (Ord m, Ord r, Num r) => Norm m r | m -> r where
    magnitude :: m -> r
          
instance (Norm a r) => Norm [a] r where
    magnitude xs = foldl' (+) 0 $ map magnitude xs
          
-- | We assume that the MetricSpace on m is compatible with the ordering on m
class (Norm m r) => MetricSpace m r | m -> r where
    dist :: m -> m -> r
    
instance MetricSpace Double Double where
    dist x y = abs $ x - y

instance Norm Double Double where
    magnitude x = abs x

-- instance MetricSpace String Int where