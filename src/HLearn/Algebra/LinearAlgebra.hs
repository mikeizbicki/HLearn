{-# LANGUAGE DataKinds #-}

module HLearn.Algebra.LinearAlgebra
    ( Vector
    , MVector
    , eye
    , outerProduct
    , outerProductV
    , extractDiagonal
    , extractBanded
    , matProduct
    , v2m
    , v2m'
    , trans
    , Mult (..)
    , ValidTensor1 (..)
    , ValidTensor (..)
    , IsScalar
    , LA.inv
    , LA.eig
    , LA.eigenvalues
    , LA.Field
    , LA.Matrix
    , LA.rows

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
import Debug.Trace

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
    , Floating a
    , a ~ Scalar a
    ) => InnerProduct (Vector a) 
        where
    inner (Vector a) (Vector b) = VG.sum $ VG.zipWith (*) a b
--     inner (Vector a) (Vector b) = kernel $ VG.sum $ VG.zipWith (*) a b 
--         where
--             kernel x = (1+x)**2

            -- 0.44

--     inner (Vector a) (Vector b) = exp $ -gamma*l2
--         where
--             gamma = 0.0001
--             l2 =  VG.sum $ VG.map (**2) $ VG.zipWith (-) a b 

-------------------------------------------------------------------------------
-- matrices

eye :: LA.Field a => Int -> LA.Matrix a
eye = LA.ident

-- class TensorProduct a b c | a b -> c
--     tensorProduct :: a -> b -> c

v2m :: LA.Field a => Vector a -> LA.Matrix a
v2m (Vector v) = LA.asColumn v

v2m' :: LA.Field a => Vector a -> LA.Matrix a
v2m' (Vector v) = LA.asRow v

trans :: LA.Matrix a -> LA.Matrix a
trans = LA.trans

-- data a +> b = (+>) a b
-- type family Tensor (order::Nat) a
-- type instance Tensor 0 Double = Double
-- type instance Tensor 1 Double = Double
-- type instance Tensor 2 Double = Double
-- type instance Tensor 0 (Vector r) = r
-- type instance Tensor 1 (Vector r) = Vector r
-- type instance Tensor 2 (Vector r) = LA.Matrix r

class Mult a b c | a b -> c, a c -> b, b c -> a where
    mul :: a -> b -> c

class 
    ( InnerProduct (Tensor 0 a) 
    , InnerProduct (Tensor 1 a)
    , Tensor 0 a ~ Scalar a
    , Scalar (Tensor 0 a) ~ Scalar a
    , Scalar (Tensor 1 a) ~ Scalar a
    , LA.Field (Scalar a)
    ) => ValidTensor1 a 
        where
    type Tensor (order::Nat) a
    mkTensor :: a -> Tensor 1 a
    

class 
    ( ValidTensor1 a
    , InnerProduct (Tensor 2 a)
    , OuterProduct (Tensor 1 a) (Tensor 2 a)
    , Mult (Tensor 1 a) (Tensor 2 a) (Tensor 1 a)
    , Mult (Tensor 2 a) (Tensor 1 a) (Tensor 1 a)
    , Mult (Tensor 2 a) (Tensor 2 a) (Tensor 2 a)
    , Scalar (Tensor 2 a) ~ Scalar a
    ) => ValidTensor a 
        where

class OuterProduct t1 t2 | t1 -> t2, t2 -> t1 where
    outerProduct :: t1 -> t1 -> t2

outerProductV :: LA.Field r => Vector r -> Vector r -> LA.Matrix r
outerProductV (Vector v1) (Vector v2) = LA.asColumn v1 LA.<> LA.asRow v2

type IsScalar a = 
    ( LA.Field a
    , a ~ Scalar a
    , a ~ Tensor 1 a
    , VectorSpace a
    , ValidTensor a
    )

---------------------------------------

instance ValidTensor1 Double where
    type Tensor 0 Double = Double
    type Tensor 1 Double = Double
    type Tensor 2 Double = Double
    mkTensor = id

instance ValidTensor Double

instance OuterProduct Double Double where
    outerProduct = (*)

instance Mult Double Double Double where
    mul a b = a*b

---------------------------------------

instance (IsScalar a, LA.Field a, VectorSpace a) => ValidTensor1 (Vector a) where
    type Tensor 0 (Vector a) = a
    type Tensor 1 (Vector a) = Vector a
    type Tensor 2 (Vector a) = LA.Matrix a
    mkTensor = id

instance (IsScalar a, ValidTensor1 (Vector a)) => ValidTensor (Vector a)

instance LA.Field a => OuterProduct (Vector a) (LA.Matrix a) where
    outerProduct (Vector v1) (Vector v2) = LA.asColumn v1 LA.<> LA.asRow v2

instance IsScalar a => InnerProduct (LA.Matrix a) where
    inner = undefined

instance LA.Field a => Mult (Vector a) (LA.Matrix a) (Vector a) where
    mul (Vector v) m = Vector $ v LA.<> m
instance LA.Field a => Mult (LA.Matrix a) (Vector a) (Vector a) where
    mul m (Vector v) = Vector $ m LA.<> v
instance LA.Field a => Mult (LA.Matrix a) (LA.Matrix a) (LA.Matrix a) where
    mul = (LA.<>)

-------------------------------------------------------------------------------

class TensorProduct a where
    tensor :: Tensor 1 a -> Tensor 1 a -> Tensor 2 a

-- instance LA.Field r => TensorProduct (Vector r) where
--     tensor (Vector v1) (Vector v2) = LA.asColumn v1 LA.<> LA.asRow v2

-- outerProduct :: LA.Field a => Vector a -> Vector a -> LA.Matrix a
-- outerProduct (Vector v1) (Vector v2) = LA.asColumn v1 LA.<> LA.asRow v2

extractDiagonal :: LA.Field a => LA.Matrix a -> LA.Matrix a
extractDiagonal = extractBanded 0

extractBanded :: LA.Field a => Int -> LA.Matrix a -> LA.Matrix a
extractBanded b m = LA.buildMatrix (LA.rows m) (LA.cols m) go
    where
        go (r,c) = if abs (r-c)-1 < b
            then m LA.@@> (r,c)
            else 0

matProduct a b = a LA.<> b

instance LA.Mul Vector LA.Matrix Vector where
    (Vector v) <> m = Vector $ m LA.<> v

instance LA.Mul LA.Matrix Vector Vector where
    m <> (Vector v) = Vector $ m LA.<> v

instance LA.Field a => Monoid (LA.Matrix a) where
    mempty = (1 LA.>< 1 $ [0])
    mappend a b = if a==mempty 
        then b
        else if b==mempty
            then a
            else a `LA.add` b
--     mempty = error "LA.Matrix mempty"
--     mappend a b = a `LA.add` b

instance LA.Field a => Group (LA.Matrix a) where
    inverse a = LA.scale (-1) a

instance LA.Field a => Abelian (LA.Matrix a)

type instance Scalar (LA.Matrix a) = a

instance LA.Field a => Module (LA.Matrix a) where
    r .* a = LA.scale r a

instance (Module a, LA.Field a) => VectorSpace (LA.Matrix a) where
    a /. r = LA.scale (1/r) a

-- newtype Matrix a = Matrix (LA.Matrix a)
-- 
-- instance (Num a, LA.Field a) => Monoid (Matrix a) where
--     mempty = undefined
--     mappend (Matrix a) (Matrix b) = Matrix $ a + b
-- 
-- instance LA.Field a => Group (Matrix a) where
--     inverse (Matrix a) = Matrix $ negate a
