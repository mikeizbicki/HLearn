module HLearn.DataStructures.SpaceTree.Naive
    where

import qualified Data.Foldable as F

import HLearn.Algebra
import HLearn.DataStructures.SpaceTree

-------------------------------------------------------------------------------
-- data types

data NaiveST dp 
    = forall container. (F.Foldable container, Monoid (container dp)) => NaiveST { dataset :: container dp }

instance SpaceTree NaiveST dp where
    stMinDistance _ _ = 0
    stMaxDistance _ _ = infinity
    
    stMinDistanceDp _ _ = 0
    stMaxDistanceDp _ _ = infinity
    
    stChildren =  
    stMinDistance :: t dp -> t dp -> Ring dp
    stMaxDistance :: t dp -> t dp -> Ring dp
    
    stMinDistanceDp :: t dp -> dp -> Ring dp
    stMaxDistanceDp :: t dp -> dp -> Ring dp

    stChildren :: t dp -> [t dp]
    stNode :: t dp -> dp
    stHasNode :: t dp -> Bool
    stIsLeaf :: t dp -> Bool

-------------------------------------------------------------------------------
-- algebra

instance Monoid (NaiveST dp) where
    mempty = NaiveST mempty

-------------------------------------------------------------------------------
-- training


