-- | Adapts any distribution into one that can handle missing data
module HLearn.Models.Distributions.Univariate.Internal.MissingData
    ( MissingData
    , MissResponse (..)
    )
    where

import HLearn.Algebra
import HLearn.Models.Distributions.Common

-------------------------------------------------------------------------------
-- data types

data MissResponse = Ignore

newtype MissingData (response :: MissResponse) (basedist:: * -> *) (prob :: *) = 
    MissingData { basedist :: basedist prob }
    deriving (Show,Read,Eq,Ord,Monoid,Group)

-------------------------------------------------------------------------------
-- Algebra

instance (Abelian (basedist prob)) => Abelian (MissingData response basedist prob)

-------------------------------------------------------------------------------
-- Training

instance 
    ( HomTrainer (basedist prob)
    ) => HomTrainer (MissingData response basedist prob) 
        where
    type Datapoint (MissingData response basedist prob) = Maybe (Datapoint (basedist prob))
    
    train1dp Nothing = MissingData mempty
    train1dp (Just dp) = MissingData $ train1dp dp

-------------------------------------------------------------------------------
-- Distribution

instance Probabilistic (MissingData response basedist prob) where
    type Probability (MissingData response basedist prob) = prob

instance 
    ( Probability (basedist prob) ~ prob
    , PDF (basedist prob)
    , Num prob
    ) => PDF (MissingData Ignore basedist prob)
        where

    {-# INLINE pdf #-}
    pdf (MissingData dist) (Just dp) = pdf (dist) dp
    pdf (MissingData dist) Nothing = 1
