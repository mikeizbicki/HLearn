module HLearn.Algebra.LinearAlgebra
    ( Vector
    , MVector

--     , module Numeric.LinearAlgebra
    ) where

import Control.DeepSeq
import Control.Monad
import Data.Typeable
import Foreign.Storable
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM

import qualified Numeric.LinearAlgebra as LA

import HLearn.Algebra

-------------------------------------------------------------------------------
-- Double

instance Monoid Double where
    mempty = 0
    mappend = (+)

instance Group Double where
    inverse = negate

instance Abelian Double

instance Module Double where
    a.*b = a*b
    b*.a = b*a

instance VectorSpace Double where
    (/.) = (/) 

instance InnerProduct Double where
    inner = (*)

-------------------------------------------------------------------------------
-- vectors

newtype Vector a = Vector (VS.Vector a)
    deriving (Read,Show,Eq,Ord,NFData,Typeable)

instance Storable a => VG.Vector Vector a where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE elemseq #-}
    basicUnsafeFreeze (MVector v) = liftM Vector $ VG.basicUnsafeFreeze v
    basicUnsafeThaw (Vector v) = liftM MVector $ VG.basicUnsafeThaw v
    basicLength (Vector v) = VG.basicLength v
    basicUnsafeSlice s t (Vector v) = Vector $ VG.basicUnsafeSlice s t v
    basicUnsafeIndexM (Vector v) i = VG.basicUnsafeIndexM v i
    basicUnsafeCopy (MVector vm) (Vector v) = VG.basicUnsafeCopy vm v
    elemseq (Vector v) a b = VG.elemseq v a b

newtype MVector s a = MVector (VSM.MVector s a)

instance Storable a => VGM.MVector MVector a where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength (MVector v) = VGM.basicLength v
    basicUnsafeSlice s t (MVector v) = MVector $ VGM.basicUnsafeSlice s t v
    basicOverlaps (MVector v1) (MVector v2) = VGM.basicOverlaps v1 v2
    basicUnsafeNew n = liftM MVector $ VGM.basicUnsafeNew n
    basicUnsafeReplicate i a = liftM MVector $ VGM.basicUnsafeReplicate i a
    basicUnsafeRead (MVector v) i = VGM.basicUnsafeRead v i
    basicUnsafeWrite (MVector v) i a = VGM.basicUnsafeWrite v i a

type instance VG.Mutable Vector = MVector

---------------------------------------
-- basic algebra

instance (Storable a, Monoid a) => Monoid (Vector a) where
    mempty = Vector $ VG.fromList []
    mappend (Vector a) (Vector b) = Vector $ VG.fromList $ go (VG.toList a) (VG.toList b)
        where
            go (a:as) (b:bs) = a<>b:go as bs
            go (a:as) [] = a:go as []
            go [] (b:bs) = b:go [] bs
            go [] [] = []

instance (Storable a, Group a) => Group (Vector a) where
    inverse (Vector a) = Vector $ VG.map inverse a

instance (Storable a, Abelian a) => Abelian (Vector a)

type instance Scalar (Vector a) = Scalar a

instance (Storable a, Module a) => Module (Vector a) where
    r .* (Vector a) = Vector $ VG.map (r.*) a
    (Vector a) *. r = Vector $ VG.map (*.r) a

instance (Storable a, VectorSpace a) => VectorSpace (Vector a) where
    (Vector a) /. r = Vector $ VG.map (/.r) a

instance 
    ( Storable a
    , VectorSpace a
    , Num a
    , a ~ Scalar a
    ) => InnerProduct (Vector a) 
        where
    inner (Vector a) (Vector b) = VG.sum $ VG.zipWith (*) a b 
