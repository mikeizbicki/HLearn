{-# LANGUAGE DeriveFunctor,DeriveFoldable #-}

module HLearn.Algebra.StrictList
    where

import qualified Data.Foldable as F 

data StrictList a = SNil | !a `SCons` !(StrictList a)
    deriving (Read,Show,Eq,Ord,Functor,F.Foldable)
