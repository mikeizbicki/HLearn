{-# LANGUAGE DatatypeContexts, MultiParamTypeClasses, FlexibleInstances #-}

import Data.List
import qualified Data.Map as Map

class LeftOperator r m where
    (.*) :: r -> m -> m

newtype (Num r, Ord a) => FreeMod r a = FreeMod (Map.Map a r)
    deriving (Read,Show,Eq)

instance (Num r, Ord a) => LeftOperator r (FreeMod r a) where
    r .* (FreeMod m) = FreeMod $ Map.map (r*) m
    
list2module :: (Num r, Ord r, Ord a) => [a] -> FreeMod r a
list2module xs = FreeMod $ Map.fromList $ go 0 (head sorted) [] sorted
    where
        sorted = sort xs
        go n x retL [] = (x,n):retL
        go n x retL xs = if (head xs) == x
            then go (n+1) x retL (tail xs)
            else go 1 (head xs) ((x,n):retL) (tail xs)

lettercount = list2module "supercalifragilisticexpialidocious"