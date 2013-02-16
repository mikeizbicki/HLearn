{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HLearn.Algebra.Structures.MetricSpace
    where
          
class MetricSpace m r | m -> r where
    dist :: m -> m -> r
    origin :: m
    
magnitude :: (MetricSpace m r) => m -> r
magnitude x = dist origin x
    
instance MetricSpace Double Double where
    dist x y = sqrt $ x*x+y*y
    origin = 0
    
-- instance MetricSpace String Int where