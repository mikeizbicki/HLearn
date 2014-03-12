module HLearn.Metrics.Mahalanobis
    where

import Control.DeepSeq
import qualified Data.Vector.Generic as VG

import Debug.Trace 

import qualified Data.Strict as Strict
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

mkIdentity :: dp -> Mahalanobis dp
mkIdentity dp = Mahalanobis dp dp

---------------------------------------

dim :: VG.Vector dp r => Mahalanobis (dp r) -> Int
dim m = VG.length $ rawdp m 

---------------------------------------

class 
    ( Storable (Scalar dp)
    , Element (Scalar dp)
    , Container Vector (Scalar dp)
    , LA.Product (Scalar dp)
    , Field (Scalar dp)
    , Scalar dp ~ Double
    ) => MatrixField dp

instance 
    ( Storable (Scalar dp)
    , Element (Scalar dp)
    , Container Vector (Scalar dp)
    , LA.Product (Scalar dp)
    , Field (Scalar dp)
    , Scalar dp ~ Double
    ) => MatrixField dp

class MkMahalanobis params where
    type MetricDatapoint params -- = Datapoint params
    mkMahalanobis :: params -> MetricDatapoint params -> Mahalanobis (MetricDatapoint params)


class MahalanobisMetric metric where
    getMatrix :: metric -> Matrix (Scalar metric)

applyMahalanobis :: 
    ( VG.Vector vec r
    , MahalanobisMetric metric
    , Element r
    , Field r
    , LA.Product r
    , Scalar metric ~ r
    , r~Double
    ) => metric -> vec r -> vec r
applyMahalanobis metric dp = VG.fromList $ toList $ flatten $ 
    (mapMatrix realPart $ matFunc sqrt $ mapMatrix (:+0) $ getMatrix metric) LA.<> asColumn dp'
    where
        dp' = fromList $ VG.toList dp 

-------------------------------------------------------------------------------
-- algebra

type instance Scalar (Mahalanobis dp) = Scalar dp

instance 
    ( RealFrac r
    , Floating r
    , Scalar (dp r) ~ r
    , VG.Vector dp r
    ) => MetricSpace (Mahalanobis (dp r)) where
    {-# INLINABLE distance #-}

    distance !m1 !m2 = {-# SCC distance #-} sqrt $ go 0 (dim m1-1)
        where
            go tot (-1) = tot
            go tot i = go (tot+(((rawdp m1) `VG.unsafeIndex` i)-((rawdp m2) `VG.unsafeIndex` i))
                              *(((moddp m1) `VG.unsafeIndex` i)-((moddp m2) `VG.unsafeIndex` i))) (i-1)

--     isFartherThanWithDistance !m1 !m2 !dist = {-# SCC isFartherThanWithDistance #-} 
--         go 0 (dim m1-1)
--         where
--             dist2=dist*dist
-- 
--             go tot (-1) = Strict.Just $ sqrt tot
--             go tot i = if tot'>dist2
--                 then Strict.Nothing
--                 else go tot' (i-1)
--                 where
--                     tot' = tot+(((rawdp m1) `VG.unsafeIndex` i)-((rawdp m2) `VG.unsafeIndex` i))
--                               *(((rawdp m1) `VG.unsafeIndex` i)-((rawdp m2) `VG.unsafeIndex` i))
