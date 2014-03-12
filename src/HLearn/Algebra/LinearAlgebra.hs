module HLearn.Algebra.LinearAlgebra
    where

import Foreign.Storable
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

import qualified Numeric.LinearAlgebra as LA

import HLearn.Algebra

-------------------------------------------------------------------------------
-- vectors

newtype Vector a = Vector (LA.Vector a)


newtype VectorM a = VectorM (LA.Vector a)

---------------------------------------
-- basic algebra

instance (Storable a, Monoid a) => Monoid (Vector a) where
    mempty = Vector $ VG.fromList $ repeat mempty
    mappend (Vector a) (Vector b) = Vector $ VG.zipWith (<>) a b

instance (Storable a, Group a) => Group (Vector a) where
    inverse (Vector a) = Vector $ VG.map inverse a

instance (Storable a, Abelian a) => Abelian (Vector a)

instance HasRing a => HasRing (Vector a) where
    type Ring (Vector a) = Ring a

instance (Storable a, Module a) => Module (Vector a) where
    r .* (Vector a) = Vector $ VG.map (r.*) a
    (Vector a) *. r = Vector $ VG.map (*.r) a

instance (Storable a, VectorSpace a) => VectorSpace (Vector a) where
    (Vector a) /. r = Vector $ VG.map (/.r) a

-- instance (Storable a, Num a) => InnerProduct (Vector a) where
--     inner (Vector a) (Vector b) = VG.zipWith (+) 
