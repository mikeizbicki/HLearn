module HLearn.Metrics.Mahalanobis.Normal
    where

import Control.DeepSeq
import qualified Data.Vector.Generic as VG

import Foreign.Storable
import Numeric.LinearAlgebra hiding ((<>))
import qualified Numeric.LinearAlgebra as LA

import HLearn.Algebra
import HLearn.Metrics.Mahalanobis
import HLearn.Models.Distributions.Multivariate.MultiNormalFast

-------------------------------------------------------------------------------
-- data types

newtype MahalanobisParams dp = MahalanobisParams
    { normal :: MultiNormal dp
    }

deriving instance Show (MultiNormal dp) => Show (MahalanobisParams dp)
deriving instance Read (MultiNormal dp) => Read (MahalanobisParams dp)
deriving instance Eq (MultiNormal dp) => Eq (MahalanobisParams dp)
deriving instance Ord (MultiNormal dp) => Ord (MahalanobisParams dp)
deriving instance NFData (MultiNormal dp) => NFData (MahalanobisParams dp)

-------------------------------------------------------------------------------
-- algebra

deriving instance Monoid (MultiNormal dp) => Monoid (MahalanobisParams dp)

type instance Scalar (MahalanobisParams dp) = Scalar dp

instance (Field (Scalar dp)) => MahalanobisMetric (MahalanobisParams dp) where
    getMatrix m = inv $ covar $ normal m 

-------------------------------------------------------------------------------
-- training

instance HomTrainer (MultiNormal dp) => HomTrainer (MahalanobisParams dp) where
    type Datapoint (MahalanobisParams dp) = Datapoint (MultiNormal dp) 

    train1dp dp = MahalanobisParams $ train1dp dp
    train dps = MahalanobisParams $ train dps

-------------------------------------------------------------------------------
-- metric

instance
    ( VG.Vector dp r
    , Scalar (dp r) ~ r
    , LA.Product r
    , Field r
    ) =>  MkMahalanobis (MahalanobisParams (dp r)) 
        where
    type MetricDatapoint (MahalanobisParams (dp r)) = dp r
    mkMahalanobis (MahalanobisParams m) dp = Mahalanobis
        { rawdp = dp
        , moddp = VG.fromList $ toList $ flatten $ inv (covar m) LA.<> asColumn v
        }
        where
            v = fromList $ VG.toList dp

-- mkMahalanobis :: 
--     ( VG.Vector dp r
--     , Scalar (dp r) ~ r
--     , LA.Product r
--     ) => MahalanobisParams (dp r) -> dp r -> Mahalanobis (dp r)
-- mkMahalanobis (MahalanobisParams m) dp = Mahalanobis
--     { rawdp = dp
--     , moddp = VG.fromList $ toList $ flatten $ covar m LA.<> asColumn v
--     }
--     where
--         v = fromList $ VG.toList dp
