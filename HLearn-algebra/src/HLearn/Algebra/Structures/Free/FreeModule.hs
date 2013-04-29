{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module HLearn.Algebra.Structures.Free.FreeModule
    where

import qualified Control.ConstraintKinds as CK
import Data.List
import qualified Data.Map as Map
import HLearn.Algebra.Structures.Groups

-------------------------------------------------------------------------------
-- FreeModule

newtype FreeMod r a = FreeMod (Map.Map a r)
    deriving (Read,Show,Eq)

instance CK.Functor (FreeMod r) where
    type FunctorConstraint (FreeMod r) a = (Num r, Ord a)
    fmap f (FreeMod m) = FreeMod $ Map.mapKeysWith (+) f m

instance CK.Foldable (FreeMod r) where
    type FoldableConstraint (FreeMod r) a = (Num r, Ord a, Operator r a)
--     foldr f b (FreeMod m) = Map.foldrWithKey (\a r b -> f (r .* a) b) b m
    foldr f b (FreeMod m) = foldr (\(a,r) b -> f (r .* a) b) b $ Map.toList m
    foldl f b (FreeMod m) = foldl (\b (a,r) -> f b (r .* a)) b $ Map.toList m
    foldl' f b (FreeMod m) = foldl' (\b (a,r) -> f b (r .* a)) b $ Map.toList m
    
    foldr1 f (FreeMod m) = foldr1 f $ map (\(a,r) -> r.*a) $ Map.toList m
    foldl1 f (FreeMod m) = foldl1 f $ map (\(a,r) -> r.*a) $ Map.toList m

instance (Num r, Ord a) => Abelian (FreeMod r a)
instance (Num r, Ord a) => Monoid (FreeMod r a) where
    mempty = FreeMod mempty
    mappend = (<>)
    
instance (Num r, Ord a) => Group (FreeMod r a) where
    inverse (FreeMod m) = FreeMod $ Map.map negate m

instance (Num r, Ord a) => LeftModule r (FreeMod r a)
instance (Num r, Ord a) => LeftOperator r (FreeMod r a) where
    r .* (FreeMod m) = FreeMod $ Map.map (r*) m
    
instance (Num r, Ord a) => RightModule r (FreeMod r a)
instance (Num r, Ord a) => RightOperator r (FreeMod r a) where
    a *. r = r .* a

list2module :: (Num r, Ord r, Ord a) => [a] -> FreeMod r a
list2module xs = FreeMod $ Map.fromList $ go 0 (head sorted) [] sorted
    where
        sorted = sort xs
        
        go n x retL [] = (x,n):retL
        go n x retL xs = if (head xs) == x
            then go (n+1) x retL (tail xs)
            else go 1 (head xs) ((x,n):retL) (tail xs)

