{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Provides a generic interface to structures that offer both a left and right cons.  It's based on the Data.Sequence interface.
module HLearn.Algebra.Structures.Triangles
    ( Triangle (..)
--     , module Data.Sequence
    )
    where

import qualified Data.Sequence as S
import Data.Sequence hiding ((<|),(|>))

-- | Methods for left and right cons on a data type
class Triangle f a where
    (<|) :: a -> f -> f
    (|>) :: f -> a -> f
    
instance Triangle [a] a where
    a <| xs = a:xs
    xs |> a = xs++[a]

instance Triangle (S.Seq a) a where
    (<|) = (S.<|)
    (|>) = (S.|>)