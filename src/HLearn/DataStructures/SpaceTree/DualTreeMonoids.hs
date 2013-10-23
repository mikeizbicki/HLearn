module HLearn.DataStructures.SpaceTree.DualTreeMonoids
    ( TreeAlg (..)
    , TreeAlg2 (..)
    )
    where

import GHC.TypeLits
import Control.DeepSeq

import HLearn.Algebra
import HLearn.DataStructures.SpaceTree

-------------------------------------------------------------------------------
-- query == reference 

data TreeAlg (result:: * -> *) (tree:: * -> *) (dp:: *) = TreeAlg
    { dptree  :: !(tree dp)
    , results :: !(result dp)
    }

instance (NFData (result dp), NFData (tree dp)) => NFData (TreeAlg result tree dp) where
    rnf (TreeAlg dptree results) = deepseq dptree $ rnf results

deriving instance (Show (result dp), Show (tree dp)) => Show (TreeAlg result tree dp)

instance 
    ( Monoid (tree dp)
    , Monoid (result dp)
    , Function (result dp) (DualTree (tree dp)) (result dp)
    , SpaceTree tree dp
    ) => Monoid (TreeAlg result tree dp) 
        where
    mempty = TreeAlg mempty mempty
    mappend a b = TreeAlg
        { dptree = dptree a <> dptree b
        , results = results a 
                 <> results b 
                 <> (if stHasNode (dptree a) && stHasNode (dptree b)
                         then function (undefined::result dp) (DualTree (dptree a) (dptree b))
                           <> function (undefined::result dp) (DualTree (dptree b) (dptree a))
                         else mempty
                    )
        }

instance 
    ( HomTrainer (tree dp)
    , Monoid (result dp)
    , Function (result dp) (DualTree (tree dp)) (result dp)
    , Datapoint (tree dp) ~ dp
    , SpaceTree tree dp
    ) => HomTrainer (TreeAlg result tree dp) 
        where
    type Datapoint (TreeAlg result tree dp) = dp

    train1dp dp = TreeAlg
        { dptree = train1dp dp
        , results = mempty
        }

    train dps = TreeAlg
        { dptree = tree
        , results = function (undefined::result dp) (DualTree tree tree)
        }
        where
            tree = train dps

-------------------------------------------------------------------------------
-- query /= reference

data TreeAlg2 (result:: * -> *) (tree:: * -> *) (dp:: *) = TreeAlg2
    { tree_ref :: tree dp
    , tree_query :: tree dp
    , results2 :: result dp
    }

deriving instance (Show (result dp), Show (tree dp)) => Show (TreeAlg2 result tree dp)

instance
    ( Monoid (tree dp)
    , Monoid (result dp)
    , Function (result dp) (DualTree (tree dp)) (result dp)
    , SpaceTree tree dp
    ) => Monoid (TreeAlg2 result tree dp)
        where
    mempty = TreeAlg2 mempty mempty mempty
    mappend a b = TreeAlg2
        { tree_ref = tree_ref a <> tree_ref b
        , tree_query = tree_query a <> tree_query b
        , results2 = results2 a
                  <> results2 b
                  <> (if stHasNode (tree_ref a) && stHasNode (tree_query b)
                        then function (undefined::result dp) (DualTree (tree_ref a) (tree_query b))
                        else mempty
                     )
                  <> (if stHasNode (tree_ref b) && stHasNode (tree_query a)
                        then function (undefined::result dp) (DualTree (tree_ref b) (tree_query a))
                        else mempty
                     )
        }

instance 
    ( HomTrainer (tree dp)
    , Monoid (result dp)
    , Function (result dp) (DualTree (tree dp)) (result dp)
    , Datapoint (tree dp) ~ dp
    , SpaceTree tree dp
    ) => HomTrainer (TreeAlg2 result tree dp) 
        where
    type Datapoint (TreeAlg2 result tree dp) = Either dp dp

    train1dp (Left dp) = TreeAlg2 (train1dp dp) mempty mempty
    train1dp (Right dp) = TreeAlg2 mempty (train1dp dp) mempty
