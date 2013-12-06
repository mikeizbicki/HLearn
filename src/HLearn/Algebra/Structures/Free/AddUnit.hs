module HLearn.Algebra.Structures.Free.AddUnit
    where

import Control.DeepSeq
import qualified Data.Semigroup as SG
import Data.Monoid

-------------------------------------------------------------------------------
-- data types

data AddUnit1 sg (dp:: *) = Unit1 | UnitLift1 { unUnit1 :: sg dp}
    deriving (Read,Show,Eq,Ord)

instance NFData (sg dp) => NFData (AddUnit1 sg dp) where
    {-# INLINE rnf #-}
    rnf Unit1 = ()
    rnf (UnitLift1 sg) = rnf sg

-------------------------------------------------------------------------------
-- algebra

instance SG.Semigroup (sg dp) => Monoid (AddUnit1 sg dp) where
    {-# INLINE mempty #-}
    {-# INLINE mappend #-}
    mempty = Unit1
    mappend Unit1 Unit1 = Unit1
    mappend Unit1 x = x
    mappend x Unit1 = x
    mappend (UnitLift1 x1) (UnitLift1 x2) = UnitLift1 $ x1 SG.<> x2 
