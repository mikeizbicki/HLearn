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

-- {-# LANGUAGE OverlappingInstances #-}

-- | All multivariate distributions should implement methods to marginalize
module HLearn.Models.Distributions.Multivariate.Internal.Marginalization
    where

import GHC.TypeLits
import HLearn.Algebra
import HLearn.Models.Distributions.Common

-------------------------------------------------------------------------------
-- type classes

class Marginalize index dist where
    type Margin index dist
    getMargin :: index -> dist -> Margin index dist
    
    type MarginalizeOut index dist 
    marginalizeOut :: index -> dist -> MarginalizeOut index dist
    
    condition :: index -> dist -> Datapoint (Margin index dist) -> MarginalizeOut index dist
    
--     conditionAllButOne :: index -> dist -> Datapoint dist -> MarginalizeOut index dist
    
instance (SingI n, Marginalize (Nat1Box (ToNat1 n)) dist) => Marginalize (Sing (n::Nat)) dist where
    type Margin (Sing n) dist = Margin (Nat1Box (ToNat1 n)) dist
    getMargin _ dist = getMargin (undefined :: Nat1Box (ToNat1 n)) dist
    
    type MarginalizeOut (Sing n) dist = MarginalizeOut (Nat1Box (ToNat1 n)) dist
    marginalizeOut _ dist = marginalizeOut (undefined :: Nat1Box (ToNat1 n)) dist
    
    condition _ dist dp = condition (undefined :: Nat1Box (ToNat1 n)) dist dp


class IndexName i where
    type IndexNameOf i

class (Marginalize (IndexNameOf index) dist, IndexName index) => Marginalize' index dist where

    getMargin' :: index -> dist -> Margin (IndexNameOf index) dist
    marginalizeOut' :: index -> dist -> MarginalizeOut (IndexNameOf index) dist
    condition' :: index -> dist -> Datapoint (Margin (IndexNameOf index) dist) -> MarginalizeOut (IndexNameOf index) dist

instance (Marginalize (IndexNameOf index) dist, IndexName index) => Marginalize' index dist where
    getMargin' _ dist = getMargin (undefined :: (IndexNameOf index)) dist
    marginalizeOut' _ dist = marginalizeOut (undefined :: (IndexNameOf index)) dist
    condition' _ dist dp = condition (undefined :: (IndexNameOf index)) dist dp


{-class NameIndex i where
    type NameIndexOf i 
    
instance 
    ( NameIndex i
    , Marginalize (Nat1Box (NameIndexOf i)) dist
    ) => Marginalize i dist 
        where
    type Margin i dist = Margin (NameIndexOf i) dist
    getMargin _ dist = getMargin (undefined :: Nat1Box (NameIndexOf i)) dist
    
    type MarginalizeOut i dist = MarginalizeOut (Nat1Box (NameIndexOf i)) dist
    marginalizeOut _ dist = marginalizeOut (undefined :: Nat1Box (NameIndexOf i)) dist
    
    condition _ dist dp = condition (undefined :: Nat1Box (NameIndexOf i)) dist dp
    -}
-- class Conditional index dist where
--     type Conditional index dist
    