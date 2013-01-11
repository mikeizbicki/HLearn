{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module HLearn.Algebra.Structures.Triangles
    ( Triangle (..)
--     , module Data.Sequence
    )
    where

import qualified Data.Sequence as S
import Data.Sequence hiding ((<|),(|>))

class Triangle f a where
    (<|) :: a -> f -> f
    (|>) :: f -> a -> f
    
instance Triangle [a] a where
    a <| xs = a:xs
    xs |> a = xs++[a]

instance Triangle (S.Seq a) a where
    (<|) = (S.<|)
    (|>) = (S.|>)