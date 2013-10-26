module HLearn.Algebra.Structures.Free.AddUnit
    where

import Control.DeepSeq
import qualified Data.Semigroup as SG
import Data.Monoid

-------------------------------------------------------------------------------
-- data types

data AddUnit1 sg (dp:: *) = Unit | UnitLift { unUnit :: sg dp}
    deriving (Read,Show,Eq,Ord)

instance NFData (sg dp) => NFData (AddUnit1 sg dp) where
    {-# INLINE rnf #-}
    rnf Unit = ()
    rnf (UnitLift sg) = rnf sg

-------------------------------------------------------------------------------
-- algebra

instance SG.Semigroup (sg dp) => Monoid (AddUnit1 sg dp) where
    {-# INLINE mempty #-}
    {-# INLINE mappend #-}
    mempty = Unit
    mappend Unit Unit = Unit
    mappend Unit x = x
    mappend x Unit = x
    mappend (UnitLift x1) (UnitLift x2) = UnitLift $ x1 SG.<> x2 
