
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
import HLearn.Algebra.Structures.Groups
import HLearn.Algebra.Structures.Modules

-------------------------------------------------------------------------------
-- FreeModuleule

newtype FreeModule r a = FreeModule { getMap :: Map.Map a r }
    deriving (Read,Show,Eq,Ord,NFData)

-- instance CK.Functor (FreeModule r) where
--     type FunctorConstraint (FreeModule r) a = (Num r, Ord a)
--     fmap f (FreeModule m) = FreeModule $ Map.mapKeysWith (+) f m
-- 
-- instance CK.Foldable (FreeModule r) where
--     type FoldableConstraint (FreeModule r) a = (Num r, Ord a{-, Operator r a-})
-- --     foldr f b (FreeModule m) = Map.foldrWithKey (\a r b -> f (r .* a) b) b m
--     foldr f b (FreeModule m) = foldr (\(a,r) b -> f (r .* a) b) b $ Map.toList m
--     foldl f b (FreeModule m) = foldl (\b (a,r) -> f b (r .* a)) b $ Map.toList m
--     foldl' f b (FreeModule m) = foldl' (\b (a,r) -> f b (r .* a)) b $ Map.toList m
--     
--     foldr1 f (FreeModule m) = foldr1 f $ map (\(a,r) -> r.*a) $ Map.toList m
--     foldl1 f (FreeModule m) = foldl1 f $ map (\(a,r) -> r.*a) $ Map.toList m

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

list2module :: (Num r, Ord r, Ord a) => [a] -> FreeModule r a
list2module xs = FreeModule $ Map.fromList $ go 0 (head sorted) [] sorted
    where
        sorted = sort xs
        
        go n x retL [] = (x,n):retL
        go n x retL xs = if (head xs) == x
            then go (n+1) x retL (tail xs)
            else go 1 (head xs) ((x,n):retL) (tail xs)

