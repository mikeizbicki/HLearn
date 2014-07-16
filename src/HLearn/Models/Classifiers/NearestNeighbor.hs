{-# LANGUAGE DataKinds #-}

module HLearn.Models.Classifiers.NearestNeighbor
    ( KNearestNeighbor (..)
    )
    where

import Control.Applicative
import qualified Data.Foldable as F
import Data.List
import Data.Maybe
import Debug.Trace

import HLearn.Algebra
import HLearn.Algebra.LinearAlgebra
import HLearn.DataStructures.SpaceTree
import HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common

import HLearn.DataStructures.CoverTree
import HLearn.Metrics.Lebesgue
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

-------------------------------------------------------------------------------
-- data structures

newtype KNearestNeighbor tree (k::Nat) (dp:: *) = KNearestNeighbor
    { gettree :: tree dp
    }
    deriving (Read,Show,Eq,Ord,Monoid,Group,Abelian)

-------------------------------------------------------------------------------
-- algebra
    
type instance Scalar (KNearestNeighbor tree k dp) = Scalar (tree dp)

-------------------------------------------------------------------------------
-- model

instance 
    ( HomTrainer (tree dp)
    , Datapoint (tree dp) ~ dp
    ) => HomTrainer (KNearestNeighbor tree k dp) 
        where
    type Datapoint (KNearestNeighbor tree k dp) = dp 

    train1dp dp = KNearestNeighbor $ train1dp dp
    train dps = KNearestNeighbor $ train dps
    add1dp m dp = KNearestNeighbor $ add1dp (gettree m) dp
    addBatch m dps = KNearestNeighbor $ addBatch (gettree m) dps
    
-------------------------------------------------------------------------------
-- classification

instance Probabilistic (KNearestNeighbor tree k dp) where
    type Probability (KNearestNeighbor tree k dp) = Scalar (KNearestNeighbor tree k dp)

{-
instance
    ( dp ~ MaybeLabeled label attr
    , IsScalar (Scalar dp)
    , SpaceTree tree dp
    , KnownNat k
    , Eq dp
    , Ord (Label dp)
    , Scalar (tree dp) ~ Scalar dp
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    , Show attr
    , Show (Scalar dp)
    , Show label
    ) => ProbabilityClassifier (KNearestNeighbor tree k dp)
        where
    type ResultDistribution (KNearestNeighbor tree k dp) = 
            Categorical (Probability (KNearestNeighbor tree k dp)) (Label dp)
    
    probabilityClassify m dp = --trace ("length res="++show (length $ getknnL res)++"; dp="++show dp++";\nres="++show res++"\n\n") $ 
        train . map (getLabel . neighbor) $ getknnL res 
--     probabilityClassify m dp = reduce . map (\dp -> (1+1/neighborDistance dp) .* train1dp (getLabel $ neighbor dp)) $ getknnL res 
        where
            res = findNeighborList (gettree m) (noLabel dp) :: NeighborList k dp

instance 
    ( ProbabilityClassifier (KNearestNeighbor tree k dp)
    , Ord (Scalar (tree dp))
    , Num (Scalar (tree dp))
    , Ord (Label dp)
    ) => Classifier (KNearestNeighbor tree k dp)
        where
    classify model dp = {-trace "KNN-classify" $-}  mean $ probabilityClassify model dp
  -}
    
-------------------------------------------------------------------------------
-- test

type DP = MaybeLabeled Char (L2 VU.Vector Double)

zs = 
    [ MaybeLabeled (Just 'x') $ L2 $ VU.fromList [2,3]
    , MaybeLabeled (Just 'x') $ L2 $ VU.fromList [2,5]
    , MaybeLabeled (Just 'x') $ L2 $ VU.fromList [3,5]
    , MaybeLabeled (Just 'y') $ L2 $ VU.fromList [3,4]
    , MaybeLabeled (Just 'y') $ L2 $ VU.fromList [2,-2]
    , MaybeLabeled (Just 'y') $ L2 $ VU.fromList [2,-1]
    ] 
    :: [DP] 

q1 = L2 $ VU.fromList [2,1.1] :: L2 VU.Vector Double
q2 = L2 $ VU.fromList [5,5] :: L2 VU.Vector Double
q3 = L2 $ VU.fromList [2,3] :: L2 VU.Vector Double

-- ct i = train $ take i zs :: CoverTree DP 
-- ct' i = train $ take i $ map getAttributes zs :: CoverTree (Attributes DP) 
-- ct = train zs :: CoverTree DP 
-- ct' = train $ map getAttributes zs :: CoverTree (Attributes DP) 

-- m = train zs :: KNearestNeighbor (AddUnit (CoverTree' (2/1) V.Vector) ()) 1 DP
