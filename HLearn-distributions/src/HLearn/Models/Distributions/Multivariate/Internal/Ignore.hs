-- | Used for ignoring data
module HLearn.Models.Distributions.Multivariate.Internal.Ignore
    ( Ignore 
    , Ignore' (Ignore')
    ) where

import Control.DeepSeq

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Multivariate.Internal.Unital
import HLearn.Models.Distributions.Multivariate.Internal.Marginalization

-------------------------------------------------------------------------------
-- data types

newtype Ignore' (label:: *) (basedist:: *) (prob :: *) = Ignore' { basedist :: basedist }
    deriving (Show,Read,Eq,Ord,NFData)

type family Ignore (xs :: [*]) :: [* -> * -> *]
type instance Ignore '[] = '[]
type instance Ignore (x ': xs) = (Ignore' x) ': (Ignore xs) 

-------------------------------------------------------------------------------
-- Algebra

instance (Abelian basedist) => Abelian (Ignore' label basedist prob)
instance (Monoid basedist) => Monoid (Ignore' label basedist prob) where
    mempty = Ignore' mempty
    mappend d1 d2 = Ignore' $ mappend (basedist d1) (basedist d2)

instance (Group basedist) => Group (Ignore' label basedist prob) where
    inverse d = Ignore' $ inverse (basedist d)

instance (HasRing basedist) => HasRing (Ignore' label basedist prob) where
    type Ring (Ignore' label basedist prob) = Ring basedist

instance (Module basedist) => Module (Ignore' label basedist prob) where
    r .* d = Ignore' $ r .* basedist d

-------------------------------------------------------------------------------
-- Training

instance 
    ( HomTrainer basedist
    , Datapoint basedist ~ HList ys
    ) => HomTrainer (Ignore' label basedist prob) 
        where
    type Datapoint (Ignore' label basedist prob) = label `HCons` (Datapoint basedist)
    
    train1dp (dp:::basedp) = Ignore' $ train1dp basedp

instance (NumDP basedist) => NumDP (Ignore' label basedist prob) where
    numdp (Ignore' basedist) = numdp basedist

-------------------------------------------------------------------------------
-- Distribution

instance Probabilistic (Ignore' label basedist prob) where
    type Probability (Ignore' label basedist prob) = prob

instance 
    ( Probability basedist ~ prob
    , HomTrainer (Ignore' label basedist prob)
    , Datapoint (Ignore' label basedist prob) ~ HList dpL
    , Datapoint basedist ~ HList basedpL
    , PDF basedist
    ) => PDF (Ignore' label basedist prob)
        where

    {-# INLINE pdf #-}
    pdf dist (label:::basedp) = pdf (basedist dist) basedp

-- instance Marginalize (Nat1Box Zero) (Ignore' label basedist prob) (Unital prob) where
--     getMargin _ dist = Categorical $ Map.map numdp (pdfmap dist) 
    
instance 
    ( Marginalize' (Nat1Box n) basedist
    ) => Marginalize' (Nat1Box (Succ n)) (Ignore' label basedist prob) 
        where
    type Margin' (Nat1Box (Succ n)) (Ignore' label basedist prob) = Margin' (Nat1Box n) basedist
    getMargin' _ dist = getMargin' (undefined :: Nat1Box n) $ basedist dist
    
    type MarginalizeOut' (Nat1Box (Succ n)) (Ignore' label basedist prob) = 
        Ignore' label (MarginalizeOut' (Nat1Box n) basedist) prob
    marginalizeOut' _ dist = Ignore' $ marginalizeOut' (undefined :: Nat1Box n) $ basedist dist
    
    condition' _ dist dp = Ignore' $ condition' (undefined :: Nat1Box n) (basedist dist) dp