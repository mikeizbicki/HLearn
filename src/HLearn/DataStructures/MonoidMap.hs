module HLearn.DataStructures.MonoidMap
    where

import qualified Data.Foldable as F
import HLearn.Algebra

-------------------------------------------------------------------------------
-- data types

data MonoidMap k m
    = Node { key :: k, val :: m, left :: MonoidMap k m, right :: MonoidMap k m }
    | Leaf

instance (Show k, Show m) => Show (MonoidMap k m) where
    show Leaf = "Leaf"
    show m = "Sum { key="++show (key m)++", val="++show (val m)++", ... }"

newtype Unsafe x = Unsafe { safe :: x }

-------------------------------------------------------------------------------
-- algebra

instance (Ord k, Monoid m) => Monoid (MonoidMap k m) where
    mempty = Leaf

    mappend Leaf Leaf = Leaf
    mappend Leaf m = m
    mappend m Leaf = m

-- | assumes that all keys in the right argument are >= all keys in the left argument
instance Monoid m => Monoid (Unsafe (MonoidMap k m)) where
    mempty = Unsafe Leaf

    mappend (Unsafe Leaf) (Unsafe Leaf) = Unsafe Leaf
    mappend (Unsafe Leaf) m = m
    mappend m (Unsafe Leaf) = m
    mappend (Unsafe m1) (Unsafe m2) = Unsafe $ Node
        { key = key m1
        , val = val m1 <> val m2
        , left = m1
        , right = m2
        }

---------------------------------------

instance F.Foldable (MonoidMap k) where
    foldr f b Leaf = b
    foldr f b (Node k v Leaf Leaf) = f v b 
    foldr f b m = F.foldr f (F.foldr f b (right m)) (left m)

-------------------------------------------------------------------------------
-- training

instance (Ord k, Monoid m) => HomTrainer (MonoidMap k m) where
    type Datapoint (MonoidMap k m) = (k,m)
    train1dp (k,m) = Node k m Leaf Leaf

instance Monoid m => HomTrainer (Unsafe (MonoidMap k m)) where
    type Datapoint (Unsafe (MonoidMap k m)) = (k,m)
    train1dp (k,m) = Unsafe $ Node k m Leaf Leaf
