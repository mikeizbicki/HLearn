{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | 
module HLearn.Models.Distributions.Multivariate.Internal.Container
    ( Container
    , MultiContainer
    )
    where

import Debug.Trace
import GHC.TypeLits
import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Multivariate.Internal.Ignore
import HLearn.Models.Distributions.Multivariate.Internal.Marginalization

-------------------------------------------------------------------------------
-- data types

data Container dist sample basedist (prob:: * ) = Container    
    { dist :: dist prob
    , basedist :: basedist
    }
    deriving (Read,Show,Eq,Ord)
    
newtype MultiContainer dist sample basedist prob = MultiContainer (Container dist sample basedist prob)
    deriving (Read,Show,Eq,Ord,Monoid,Group)

-------------------------------------------------------------------------------
-- Algebra

instance (Abelian (dist prob), Abelian basedist) => Abelian (Container dist sample basedist prob) 
instance 
    ( Monoid (dist prob)
    , Monoid basedist
    ) => Monoid (Container dist sample basedist prob) 
        where
    mempty = Container mempty mempty
    c1 `mappend` c2 = Container
        { dist = dist c1 <> dist c2
        , basedist = basedist c1 <> basedist c2
        }

instance 
    ( Group (dist prob)
    , Group basedist
    ) => Group (Container dist sample basedist prob) 
        where
    inverse c = Container
        { dist = inverse $ dist c
        , basedist = inverse $ basedist c
        }

instance 
    ( HasRing (dist prob)
    , HasRing basedist
    , Ring (dist prob) ~ Ring basedist
    ) => HasRing (Container dist sample basedist prob)
        where
    type Ring (Container dist sample basedist prob) = Ring (dist prob)
    
instance 
    ( Module (dist prob)
    , Module basedist
    , Ring (dist prob) ~ Ring basedist
    ) => Module (Container dist sample basedist prob) 
        where
    r .* c = Container
        { dist = r .* (dist c)
        , basedist = r .* (basedist c)
        }
        
-------------------------------------------------------------------------------
-- Training

instance 
    ( HomTrainer (dist prob)
    , HomTrainer basedist
    , Datapoint basedist ~ HList ys
    ) =>  HomTrainer (Container dist sample basedist prob) 
        where
    type Datapoint (Container dist sample basedist prob) = 
        (Datapoint (dist prob)) `HCons` (Datapoint basedist)
        
    train1dp (dp:::basedp) = Container
        { dist = train1dp dp
        , basedist = train1dp basedp
        }

instance (NumDP (dist prob) ring) => NumDP (Container dist sample basedist prob) ring where
    numdp container = numdp $ dist container

---------------------------------------

instance 
    ( HomTrainer (dist prob)
    , HomTrainer basedist
    , Datapoint (dist prob) ~ HList zs
    , Datapoint basedist ~ HList ys
    , HTake1 (Nat1Box (Length1 zs)) (HList (zs++ys)) (HList zs)
    , HDrop1 (Nat1Box (Length1 zs)) (HList (zs++ys)) (HList ys)
    ) =>  HomTrainer (MultiContainer dist sample basedist prob) 
        where
    type Datapoint (MultiContainer dist sample basedist prob) = 
        (Datapoint (dist prob)) `HAppend` (Datapoint basedist)

    train1dp dpL = MultiContainer $ Container 
        { dist = train1dp $ htake1 (Nat1Box :: Nat1Box (Length1 zs)) dpL
        , basedist = train1dp $ hdrop1 (Nat1Box :: Nat1Box (Length1 zs)) dpL
        }

instance (NumDP (dist prob) ring) => NumDP (MultiContainer dist sample basedist prob) ring where
    numdp (MultiContainer container) = numdp $ dist container
    
-------------------------------------------------------------------------------
-- Distribution
    
instance Probabilistic (Container dist sample basedist prob) where
    type Probability (Container dist sample basedist prob) = prob
    
instance 
    ( PDF (dist prob)
    , PDF basedist
    , Probability (dist prob) ~ prob
    , Probability basedist ~ prob
    , Probabilistic (Container dist sample basedist prob) 
    , Datapoint basedist ~ HList ys
    , Datapoint (dist prob) ~ y
    , Datapoint (Container dist sample basedist prob) ~ HList (y ': ys)
    , Num prob
    ) => PDF (Container dist sample basedist prob) 
        where
    pdf container (dp:::basedp) = pdf1*pdf2
        where
            pdf1 = pdf (dist container) dp
            pdf2 = pdf (basedist container) basedp

instance Marginalize (Nat1Box Zero) (Container dist (sample :: *) basedist prob) where
    type Margin (Nat1Box Zero) (Container dist sample basedist prob) = dist prob
    getMargin _ container = dist container
    
    type MarginalizeOut (Nat1Box Zero) (Container dist sample basedist prob) = Ignore' sample basedist prob
    marginalizeOut _ container = Ignore' $ basedist container
    
instance 
    ( Marginalize (Nat1Box n) basedist
    ) => Marginalize (Nat1Box (Succ n)) (Container dist sample basedist prob)
        where
    type Margin (Nat1Box (Succ n)) (Container dist sample basedist prob) = Margin (Nat1Box n) basedist
    getMargin _ container = getMargin (undefined :: Nat1Box n) $ basedist container
    
    type MarginalizeOut (Nat1Box (Succ n)) (Container dist sample basedist prob) = 
        Container dist sample (MarginalizeOut (Nat1Box n) basedist) prob 
    marginalizeOut _ container = Container 
        { dist = dist container
        , basedist = marginalizeOut (undefined :: Nat1Box n) $ basedist container 
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
    ( PDF (dist prob)
    , PDF basedist
    , prob ~ Probability (dist prob)
    , prob ~ Probability basedist
    , Num prob
    , Datapoint (dist prob) ~ HList dpL
    , Datapoint basedist ~ HList basedpL
    , HTake1 (Nat1Box (Length1 dpL)) (HList (dpL ++ basedpL)) (HList dpL)
    , HDrop1 (Nat1Box (Length1 dpL)) (HList (dpL ++ basedpL)) (HList basedpL)
    ) => PDF (MultiContainer dist sample basedist prob) 
        where
    pdf (MultiContainer container) dp = (pdf (dist container) dp1)*(pdf (basedist container) dp2)
        where
            dp1 = htake1 (Nat1Box :: Nat1Box (Length1 dpL)) dp
            dp2 = hdrop1 (Nat1Box :: Nat1Box (Length1 dpL)) dp
