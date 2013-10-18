module HLearn.DataStructures.MonoidTree
    ( MonoidTree
    , getval
    , takeMT
    , dropMT
    )
    where

import HLearn.Algebra

-------------------------------------------------------------------------------
-- data types

data MonoidTree m
    = Node { val :: m, numdp' :: Int, left :: MonoidTree m, right :: MonoidTree m }
    | Leaf

instance Show m => Show (MonoidTree m) where
    show Leaf = "Leaf"
    show m = "Sum { val="++show (val m)++", numdp="++show (numdp m)++", ... }"

-------------------------------------------------------------------------------
-- algebra

instance Monoid m => Monoid (MonoidTree m) where
    mempty = Leaf

    mappend Leaf Leaf = Leaf
    mappend Leaf m = m
    mappend m Leaf = m
    mappend m1 m2 = Node
        { val = val m1 <> val m2
        , numdp' = numdp' m1 + numdp' m2
        , left = m1
        , right = m2
        }

-------------------------------------------------------------------------------
-- training

instance HasRing (MonoidTree m) where
    type Ring (MonoidTree m) = Int

instance NumDP (MonoidTree m) where
    numdp Leaf = 0
    numdp m = numdp' m

instance Monoid m => HomTrainer (MonoidTree m) where
    type Datapoint (MonoidTree m) = m
    train1dp m = Node m 1 Leaf Leaf

-------------------------------------------------------------------------------
-- operations

getval :: Monoid m => MonoidTree m -> m
getval Leaf = mempty
getval m = val m

takeMT :: Monoid m => Int -> MonoidTree m -> MonoidTree m
takeMT 0 _ = Leaf
takeMT _ Leaf = Leaf
takeMT i m = if i >= numdp m
    then m
    else takeMT i (left m) <> takeMT (i - (numdp $ left m)) (right m)


dropMT :: Monoid m => Int -> MonoidTree m -> MonoidTree m
dropMT 0 m = m
dropMT _ Leaf = Leaf
dropMT i m = if i >= numdp m
    then Leaf
    else dropMT i (left m) <> dropMT (i - (numdp $ left m)) (right m)

