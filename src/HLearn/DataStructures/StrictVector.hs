module HLearn.DataStructures.StrictVector
    ( StrictVector
    , StrictMVector
    )
    where

import Control.Monad
import Data.Monoid
import qualified Data.Foldable as F

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

newtype StrictVector a = StrictVector (V.Vector a)
newtype StrictMVector s a = StrictMVector (VM.MVector s a)

instance VGM.MVector StrictMVector a where
    basicLength (StrictMVector v) = VGM.basicLength v
    basicUnsafeSlice i j (StrictMVector v) = StrictMVector $ VGM.basicUnsafeSlice i j v
    basicOverlaps (StrictMVector v1) (StrictMVector v2) = VGM.basicOverlaps v1 v2
    basicUnsafeNew i = liftM StrictMVector $ VGM.basicUnsafeNew i
    basicUnsafeRead (StrictMVector v) i = VGM.basicUnsafeRead v i
    basicUnsafeWrite (StrictMVector v) i a = seq a $ VGM.basicUnsafeWrite v i a

type instance VG.Mutable StrictVector = StrictMVector

instance VG.Vector StrictVector a where
    basicUnsafeFreeze (StrictMVector v) = liftM StrictVector $ VG.basicUnsafeFreeze v
    basicUnsafeThaw (StrictVector v) = liftM StrictMVector $ VG.basicUnsafeThaw v
    basicLength (StrictVector v) = VG.basicLength v
    basicUnsafeSlice s t (StrictVector v) = StrictVector $ VG.basicUnsafeSlice s t v
    basicUnsafeIndexM (StrictVector v) i = VG.basicUnsafeIndexM v i
    basicUnsafeCopy (StrictMVector vm) (StrictVector v) = VG.basicUnsafeCopy vm v
    elemseq (StrictVector v) a b = VG.elemseq v a b
    
instance Functor StrictVector where
    fmap f (StrictVector v) = StrictVector $ fmap f v
    
instance F.Foldable StrictVector where
    foldMap f (StrictVector v) = F.foldMap f v

instance Monoid (StrictVector a) where
    mempty = StrictVector $ mempty
    mappend (StrictVector v1) (StrictVector v2) = StrictVector $ mappend v1 v2
