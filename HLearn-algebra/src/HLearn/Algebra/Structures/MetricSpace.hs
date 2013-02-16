{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HLearn.Algebra.Structures.MetricSpace
    where
          
class (Ord m) => Norm m r | m -> r where
    magnitude :: m -> r
          
-- | We assume that the MetricSpace on m is compatible with the ordering on m
class (Norm m r) => MetricSpace m r | m -> r where
    dist :: m -> m -> r
    
-- magnitude :: (MetricSpace m r) => m -> r
-- magnitude x = dist origin x
    
instance MetricSpace Double Double where
    dist x y = abs $ x - y

instance Norm Double Double where
    magnitude x = abs x

-- instance MetricSpace String Int where