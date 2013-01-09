module HLearn.Algebra.Structures.Triangles
    ( Triangle (..)
    , module Data.Sequence
    )
    where

import qualified Data.Sequence as S
import Data.Sequence hiding ((<|),(|>))

class Triangle f where
    (<|) :: a -> f a -> f a
    (|>) :: f a -> a -> f a
    
instance Triangle [] where
    a <| xs = a:xs
    xs |> a = xs++[a]

instance Triangle S.Seq where
    (<|) = (S.<|)
    (|>) = (S.|>)