module HLearn.Models.Distributions.Multivariate.Internal.Container
    ( Container
    , MultiContainer
    )
    where

import Control.DeepSeq
import Debug.Trace
import GHC.TypeLits
import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Multivariate.Internal.Ignore
import HLearn.Models.Distributions.Multivariate.Internal.Marginalization

-------------------------------------------------------------------------------
-- data types

data Container (dist :: * -> a -> *) (sample:: a) basedist (prob:: * ) = Container    
    { dist :: dist prob sample
    , basedist :: basedist
    }
    deriving (Read,Show,Eq,Ord)
    
instance (NFData (dist prob sample), NFData basedist) => NFData (Container dist sample basedist prob) where
    rnf c = deepseq (dist c) $ rnf (basedist c)
    
newtype MultiContainer dist sample basedist prob = MultiContainer (Container dist sample basedist prob)
    deriving (Read,Show,Eq,Ord,Monoid,Abelian,Group,NFData)

-------------------------------------------------------------------------------
-- Algebra

instance (Abelian (dist prob sample), Abelian basedist) => Abelian (Container dist sample basedist prob) 
instance 
    ( Monoid (dist prob sample)
    , Monoid basedist
    ) => Monoid (Container dist sample basedist prob) 
        where
    mempty = Container mempty mempty
    c1 `mappend` c2 = Container
        { dist = dist c1 <> dist c2
        , basedist = basedist c1 <> basedist c2
        }

instance 
    ( Group (dist prob sample)
    , Group basedist
    ) => Group (Container dist sample basedist prob) 
        where
    inverse c = Container
        { dist = inverse $ dist c
        , basedist = inverse $ basedist c
        }

instance 
    ( HasRing (dist prob sample)
    , HasRing basedist
    , Ring (dist prob sample) ~ Ring basedist
    ) => HasRing (Container dist sample basedist prob)
        where
    type Ring (Container dist sample basedist prob) = Ring (dist prob sample)


instance 
    ( HasRing (dist prob sample)
    , HasRing basedist
    , Ring (dist prob sample) ~ Ring basedist
    ) => HasRing (MultiContainer dist sample basedist prob)
        where
    type Ring (MultiContainer dist sample basedist prob) = Ring (dist prob sample)

instance 
    ( Module (dist prob sample)
    , Module basedist
    , Ring (dist prob sample) ~ Ring basedist
    ) => Module (Container dist sample basedist prob) 
        where
    r .* c = Container
        { dist = r .* (dist c)
        , basedist = r .* (basedist c)
        }
        
deriving instance     
    ( Module (dist prob sample)
    , Module basedist
    , Ring (dist prob sample) ~ Ring basedist
    ) => Module (MultiContainer dist sample basedist prob) 


-------------------------------------------------------------------------------
-- Training

instance 
    ( HomTrainer (dist prob sample)
    , HomTrainer basedist
    , Datapoint basedist ~ HList ys
    ) =>  HomTrainer (Container dist sample basedist prob) 
        where
    type Datapoint (Container dist sample basedist prob) = 
        (Datapoint (dist prob sample)) `HCons` (Datapoint basedist)
        
    train1dp (dp:::basedp) = Container
        { dist = train1dp dp
        , basedist = train1dp basedp
        }

instance 
    ( NumDP (dist prob sample)
    , HasRing basedist
    , Ring basedist ~ Ring (dist prob sample)
    ) => NumDP (Container dist sample basedist prob) 
        where
    numdp container = numdp $ dist container

---------------------------------------

instance 
    ( HomTrainer (dist prob sample)
    , HomTrainer basedist
    , Datapoint (dist prob sample) ~ HList zs
    , Datapoint basedist ~ HList ys
    , HTake1 (Nat1Box (Length1 zs)) (HList (zs++ys)) (HList zs)
    , HDrop1 (Nat1Box (Length1 zs)) (HList (zs++ys)) (HList ys)
    ) =>  HomTrainer (MultiContainer dist sample basedist prob) 
        where
    type Datapoint (MultiContainer dist sample basedist prob) = 
        (Datapoint (dist prob sample)) `HAppend` (Datapoint basedist)

    train1dp dpL = MultiContainer $ Container 
        { dist = train1dp $ htake1 (Nat1Box :: Nat1Box (Length1 zs)) dpL
        , basedist = train1dp $ hdrop1 (Nat1Box :: Nat1Box (Length1 zs)) dpL
        }

instance 
    ( NumDP (dist prob sample)
    , HasRing basedist
    , Ring basedist ~ Ring (dist prob sample)
    ) => NumDP (MultiContainer dist sample basedist prob) 
        where
    numdp (MultiContainer container) = numdp $ dist container
    
-------------------------------------------------------------------------------
-- Distribution
    
instance Probabilistic (Container dist sample basedist prob) where
    type Probability (Container dist sample basedist prob) = prob
    
instance 
    ( PDF (dist prob sample)
    , PDF basedist
    , Probability (dist prob sample) ~ prob
    , Probability basedist ~ prob
    , Probabilistic (Container dist sample basedist prob) 
    , Datapoint basedist ~ HList ys
    , Datapoint (dist prob sample) ~ y
    , Datapoint (Container dist sample basedist prob) ~ HList (y ': ys)
    , Num prob
    ) => PDF (Container dist sample basedist prob) 
        where
    pdf container (dp:::basedp) = pdf1*pdf2
        where
            pdf1 = pdf (dist container) dp
            pdf2 = pdf (basedist container) basedp

instance Marginalize' (Nat1Box Zero) (Container dist (sample :: *) basedist prob) where
    type Margin' (Nat1Box Zero) (Container dist sample basedist prob) = dist prob sample
    getMargin' _ container = dist container
    
    type MarginalizeOut' (Nat1Box Zero) (Container dist sample basedist prob) = Ignore' sample basedist prob
    marginalizeOut' _ container = Ignore' $ basedist container
    
    condition' _ container dp = Ignore' $ basedist container --error "Container.Marginalize.condition: undefined"
    
instance 
    ( Marginalize' (Nat1Box n) basedist
    ) => Marginalize' (Nat1Box (Succ n)) (Container dist sample basedist prob)
        where
    type Margin' (Nat1Box (Succ n)) (Container dist sample basedist prob) = Margin' (Nat1Box n) basedist
    getMargin' _ container = getMargin' (undefined :: Nat1Box n) $ basedist container
    
    type MarginalizeOut' (Nat1Box (Succ n)) (Container dist sample basedist prob) = 
        Container dist sample (MarginalizeOut' (Nat1Box n) basedist) prob 
    marginalizeOut' _ container = Container 
        { dist = dist container
        , basedist = marginalizeOut' (undefined :: Nat1Box n) $ basedist container 
        }

    condition' _ container dp = Container
        { dist = dist container
        , basedist = condition' (undefined :: Nat1Box n) (basedist container) dp
        }
    
{-instance Marginalize (Nat1Box Zero) (Container dist sample basedist prob) (dist prob) where
    getMargin _ container = dist container
    
instance 
    ( Marginalize (Nat1Box n) basedist margin
    ) => Marginalize (Nat1Box (Succ n)) (Container dist sample basedist prob) margin 
        where
    getMargin _ container = getMargin (undefined :: Nat1Box n) $ basedist container
-}
---------------------------------------

instance Probabilistic (MultiContainer dist sample basedist prob) where
    type Probability (MultiContainer dist sample basedist prob) = prob
    
instance 
    ( PDF (dist prob sample)
    , PDF basedist
    , prob ~ Probability (dist prob sample)
    , prob ~ Probability basedist
    , Num prob
    , Datapoint (dist prob sample) ~ HList dpL
    , Datapoint basedist ~ HList basedpL
    , HTake1 (Nat1Box (Length1 dpL)) (HList (dpL ++ basedpL)) (HList dpL)
    , HDrop1 (Nat1Box (Length1 dpL)) (HList (dpL ++ basedpL)) (HList basedpL)
    ) => PDF (MultiContainer dist sample basedist prob) 
        where
    pdf (MultiContainer container) dp = (pdf (dist container) dp1)*(pdf (basedist container) dp2)
        where
            dp1 = htake1 (Nat1Box :: Nat1Box (Length1 dpL)) dp
            dp2 = hdrop1 (Nat1Box :: Nat1Box (Length1 dpL)) dp
