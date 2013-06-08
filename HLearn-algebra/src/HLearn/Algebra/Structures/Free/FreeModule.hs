module HLearn.Algebra.Structures.Free.FreeModule
    ( FreeModule (..)
    , list2module
    )
    where

import Control.Applicative
import qualified Control.ConstraintKinds as CK
import Control.DeepSeq
import Data.List
import qualified Data.Map as Map

import HLearn.Algebra.Models.HomTrainer
import HLearn.Algebra.Structures.Groups
import HLearn.Algebra.Structures.Modules

-------------------------------------------------------------------------------
-- data types

newtype FreeModule r a = FreeModule { getMap :: Map.Map a r }
    deriving (Read,Show,Eq,Ord,NFData)

list2module :: (Num r, Ord r, Ord a) => [a] -> FreeModule r a
list2module xs = FreeModule $ Map.fromList $ go 0 (head sorted) [] sorted
    where
        sorted = sort xs
        
        go n x retL [] = (x,n):retL
        go n x retL xs = if (head xs) == x
            then go (n+1) x retL (tail xs)
            else go 1 (head xs) ((x,n):retL) (tail xs)
            
-------------------------------------------------------------------------------
-- algebra

instance (Num r, Ord a) => Abelian (FreeModule r a)
instance (Num r, Ord a) => Monoid (FreeModule r a) where
    mempty = FreeModule mempty
    (FreeModule m1) `mappend` (FreeModule m2) = FreeModule $ Map.unionWith (+) m1 m2
    
instance (Num r, Ord a) => Group (FreeModule r a) where
    inverse (FreeModule m) = FreeModule $ Map.map negate m

instance (Num r) => HasRing (FreeModule r a) where
    type Ring (FreeModule r a) = r

instance (Num r, Ord a) => Module (FreeModule r a) where
    r .* (FreeModule m) = FreeModule $ Map.map (r*) m

---------------------------------------

instance CK.Functor (FreeModule r) where
    type FunctorConstraint (FreeModule r) x = Ord x
    fmap f fm = FreeModule $ Map.mapKeys f $ getMap fm

-------------------------------------------------------------------------------
-- training

instance (Num r, Ord a) => HomTrainer (FreeModule r a) where
    type Datapoint (FreeModule r a) = a
    train1dp dp = FreeModule $ Map.singleton dp 1 