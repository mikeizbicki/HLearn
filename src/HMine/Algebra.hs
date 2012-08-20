{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module HMine.Algebra
    ( Invertible (..)
    , InverseSemigroup (..)
    , CommutativeMonoid
    , module Data.Semigroup
    )
    where

import Data.Semigroup
import Prelude hiding (subtract)

-------------------------------------------------------------------------------
-- Inverse Semigroups


class Invertible i where
    inverse :: i -> i

-- class SemiGroup sg where
--     add :: sg -> sg -> sg
    
class (Invertible isg, Semigroup isg) => InverseSemigroup isg where
    sub :: isg -> isg -> isg
    sub sg1 sg2 = sg1 <> (inverse sg2)

instance (Invertible isg, Semigroup isg) => InverseSemigroup isg where

class (Monoid m) => CommutativeMonoid m where

{-class (Monoid g) => Group g where
    inverse :: g -> g
    inverse g = subtract mempty g
        
    subtract :: g -> g -> g
    subtract g1 g2 = mappend g1 $ inverse g2-}
    
instance Monoid Int where
    mempty = 0
    mappend = (+)
    
instance Invertible Int where
    inverse x = -x