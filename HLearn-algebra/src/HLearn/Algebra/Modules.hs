{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Modules are a generalization of vector spaces

module HLearn.Algebra.Modules
    where

import Data.List
import qualified Data.Map as Map
import HLearn.Algebra.Structures

-------------------------------------------------------------------------------
-- Operators

class (LeftOperator r m, RightOperator r m) => Operator r m
instance (LeftOperator r m, RightOperator r m) => Operator r m

class LeftOperator r m where
    (<|) :: r -> m -> m

class RightOperator r m where
    (|>) :: m -> r -> m

-------------------------------------------------------------------------------
-- FreeOp

-- | Bug: Technically, the free operator should just require that r be a semigroup and use (<>) to chain the r's together.  But this would make things awkward because the number types aren't instances of semigroup.  Constraining r to be of type Num reduces our generality but makes FreeOp easier to work with in most practical use cases.

newtype (Num r) => FreeOp r a = FreeOp [(r,a)]
    deriving (Read,Show)

instance (Num r) => Functor (FreeOp r) where
    fmap f (FreeOp xs) = FreeOp $ map (\(r,a) -> (r,f a)) xs

instance (Num r) => LeftOperator r (FreeOp r m) where
    r <| (FreeOp xs) = FreeOp $ map (\(r2,m) -> (r*r2,m)) xs
    
instance (Num r) => RightOperator r (FreeOp r m) where
    m |> r = r <| m
    
list2freeop :: (Num r) => [a] -> FreeOp r a
list2freeop = FreeOp . map (\x -> (1,x))



-------------------------------------------------------------------------------
-- Modules

-- | Bug: The module classes have the constraint that r be of type Num.  Technically, this should be a Ring.  But creating a Ring class would be awkward because it would conflict with the Num class and require importing a different Prelude.

class (LeftModule r g, RightModule r g) => Module r g
instance (LeftModule r g, RightModule r g) => Module r g

class (LeftOperator r g, Num r, Group g, Abelian g) => LeftModule r g
instance (LeftOperator r g, Num r, Group g, Abelian g) => LeftModule r g

class (RightOperator r g, Num r, Group g, Abelian g) => RightModule r g
instance (RightOperator r g, Num r, Group g, Abelian g) => RightModule r g

-------------------------------------------------------------------------------
-- FreeModule

newtype (Num r) => FreeMod r a = FreeMod (Map.Map a r)
    deriving (Read,Show,Eq)

instance (Num r, Ord a) => Semigroup (FreeMod r a) where
    (FreeMod m1) <> (FreeMod m2) = FreeMod $ Map.unionWith (+) m1 m2

instance (Num r, Ord a) => Monoid (FreeMod r a) where
    mempty = FreeMod mempty
    mappend = (<>)
    
instance (Num r, Ord a) => RegularSemigroup (FreeMod r a) where
    inverse (FreeMod m) = FreeMod $ Map.map negate m

instance (Num r) => LeftOperator r (FreeMod r m) where
    r <| (FreeMod m) = FreeMod $ Map.map (r*) m
    
instance (Num r) => RightOperator r (FreeMod r m) where
    m |> r = r <| m

list2module :: (Num r, Ord r, Ord a) => [a] -> FreeMod r a
list2module xs = FreeMod $ Map.fromList $ go 0 (head sorted) [] sorted
    where
        sorted = sort xs
        
        go n x retL [] = (x,n):retL
        go n x retL xs = if (head xs) == x
            then go (n+1) x retL (tail xs)
            else go 1 (head xs) ((x,n):retL) (tail xs)
