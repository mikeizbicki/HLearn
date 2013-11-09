module HLearn.Metrics.Mahalanobis
    where

import Control.DeepSeq
import qualified Data.Vector.Generic as VG

import Foreign.Storable
import Numeric.LinearAlgebra hiding ((<>),dim)
import qualified Numeric.LinearAlgebra as LA

import HLearn.Algebra

-------------------------------------------------------------------------------
-- data types

data Mahalanobis dp = Mahalanobis
    { rawdp :: !dp
    , moddp :: !dp
    }
    deriving (Read,Show,Eq,Ord)

instance NFData dp => NFData (Mahalanobis dp) where
    rnf m = deepseq (rawdp m) 
          $ rnf (moddp m)

---------------------------------------

dim :: VG.Vector dp r => Mahalanobis (dp r) -> Int
dim m = VG.length $ rawdp m 

---------------------------------------

class 
    ( Storable (Ring dp)
    , Element (Ring dp)
    , Container Vector (Ring dp)
    , LA.Product (Ring dp)
    , Field (Ring dp)
    ) => MatrixField dp

instance 
    ( Storable (Ring dp)
    , Element (Ring dp)
    , Container Vector (Ring dp)
    , LA.Product (Ring dp)
    , Field (Ring dp)
    ) => MatrixField dp

class MkMahalanobis params where
    mkMahalanobis :: params -> Datapoint params -> Mahalanobis (Datapoint params)

-------------------------------------------------------------------------------
-- algebra

instance HasRing dp => HasRing (Mahalanobis dp) where
    type Ring (Mahalanobis dp) = Ring dp

instance 
    ( RealFrac r
    , Floating r
    , Ring (dp r) ~ r
    , HasRing (dp r)
    , VG.Vector dp r
    ) => MetricSpace (Mahalanobis (dp r)) where
    {-# INLINABLE distance #-}

    distance !m1 !m2 = {-# SCC distance #-} sqrt $ go 0 (dim m1-1)
        where
            go tot (-1) = tot
            go tot i = go (tot+(((rawdp m1) `VG.unsafeIndex` i)-((rawdp m2) `VG.unsafeIndex` i))
                              *(((moddp m1) `VG.unsafeIndex` i)-((moddp m2) `VG.unsafeIndex` i))) (i-1)
