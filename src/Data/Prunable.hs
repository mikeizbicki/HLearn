module Data.Prunable
    where

import qualified Data.Foldable as F
import HLearn.Algebra

class Prunable t where
    prunefoldr :: (b -> t a -> IndexType (t a) -> Bool) -> (a -> b -> b) -> b -> t a -> b

class (F.Foldable t) => DualFoldable t where
    dualfoldr :: ((a,a) -> b -> b) -> b -> t a -> t a -> b
    dualfoldr f i t1 t2 = foldr f i [(x,y) | x <- (F.toList t1), y <- (F.toList t2)]

data TreeIndex = TreeLeft | TreeRight
    deriving (Read,Show,Eq,Ord,Bounded,Enum)

