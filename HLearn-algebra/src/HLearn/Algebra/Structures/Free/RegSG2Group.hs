{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | These algebraic structures have sacrificed generality in favor of being easily used with the standard Haskell Prelude.  The fact that monoids are not guaranteed to be semigroups makes this difficult.

module HLearn.Algebra.Structures.Free.RegSG2Group
    where

import HLearn.Algebra.Structures.Groups
import HLearn.Algebra.Structures.Modules
import Control.DeepSeq

-- | Convert any regular semigroup into a group (and thus also a monoid) by adding a unique identity element
data RegSG2Group sg = SGNothing | SGJust sg
    deriving (Show,Read,Ord,Eq)

instance (Semigroup sg) => Semigroup (RegSG2Group sg) where
    SGNothing <> m = m
    m <> SGNothing = m
    (SGJust sg1) <> (SGJust sg2) = SGJust $ sg1<>sg2

instance (Abelian sg) => Abelian (RegSG2Group sg)

instance (RegularSemigroup sg) => RegularSemigroup (RegSG2Group sg) where
    inverse SGNothing = SGNothing
    inverse (SGJust x) = SGJust $ inverse x

instance (Semigroup sg) => Monoid (RegSG2Group sg) where
    mempty = SGNothing
    mappend = (<>)
    
-- instance (RegularSemigroup sg) => Group (RegSG2Group sg)

instance (RegularSemigroup sg, NFData sg) => NFData (RegSG2Group sg) where
    rnf SGNothing = ()
    rnf (SGJust sg) = rnf sg
    
instance (LeftOperator r sg, RegularSemigroup sg) => LeftOperator r (RegSG2Group sg) where
    r .* (SGNothing) = SGNothing
    r .* (SGJust sg) = SGJust $ r .*  sg
    
instance (RightOperator r sg, RegularSemigroup sg) => RightOperator r (RegSG2Group sg) where
    (SGNothing) *. r = SGNothing
    (SGJust sg) *. r = SGJust $ sg *. r