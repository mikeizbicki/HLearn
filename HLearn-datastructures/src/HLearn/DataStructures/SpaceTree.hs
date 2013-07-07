module HLearn.DataStructures.SpaceTree
    where

import Data.Semigroup
import HLearn.Algebra

-------------------------------------------------------------------------------
-- classes 

class SpaceTree t where
    type Node t

    root :: t -> Node t

    minDist :: Node t -> Node t -> Ring t
    maxDist :: Node t -> Node t -> Ring t
    maxChildDist :: Node t -> Ring t
    maxDescDist :: Node t -> Ring t

-------------------------------------------------------------------------------
-- data types

data KDTree' elem
    = Leaf elem
    | Node elem Splitdim (KDTree' elem) (KDTree' elem)

type KDTree = Option (KDTree' elem)

-------------------------------------------------------------------------------
-- algebra 

instance Monoid (KDTree elem splitdim) where
    mempty = KDTree Nothing 
    mappend (KDTree Nothing) (KDTree Nothing) = (KDTree Nothing)
    mappend (KDTree (Just t1)) (KDTree Nothing) = KDTree $ Just t1
    mappend (KDTree Nothing) (KDTree (Just t2)) = KDTree $ Just t2
    mappend (KDTree (Just t1)) (KDTree (Just t2)) = KDTree $ Just $
        undefined

-------------------------------------------------------------------------------
-- training 

instance HomTrainer (KDTree elem splitdim) where
    type Datapoint (KDTree elem splitdim) = elem
    train1dp elem = KDTree $ Just $ Leaf elem
