module HLearn.Models.Distributions.Multivariate.MultiNormalFast
    ( MultiNormal 
    , covar
    )
    where

import Control.DeepSeq
import qualified Data.Foldable as F
import qualified Data.Semigroup as SG
import qualified Data.Vector.Generic as VG

import Foreign.Storable
import Numeric.LinearAlgebra hiding ((<>))
import qualified Numeric.LinearAlgebra as LA

import HLearn.Algebra
import HLearn.Models.Distributions.Common

-------------------------------------------------------------------------------
-- data types 

type MultiNormal dp = AddUnit1 MultiNormal' dp

data MultiNormal' dp = MultiNormal' 
    { raw       :: !(RawMoments dp)
    , meanV     :: Vector (Scalar dp)
    , covarM    :: Matrix (Scalar dp)
    , invcovarM :: Matrix (Scalar dp)
    , pdfdenom  :: Scalar dp
    }

deriving instance (Element (Scalar dp), Read (Scalar dp)) => Read (MultiNormal' dp)
deriving instance (Element (Scalar dp), Show (Scalar dp)) => Show (MultiNormal' dp)

instance (NFData (Scalar dp), Storable (Scalar dp)) => NFData (MultiNormal' dp) where
    rnf m = deepseq (raw m) 
          $ deepseq (meanV m) 
          $ deepseq (covarM m) 
          $ deepseq (invcovarM m) 
          $ rnf (pdfdenom m)

---------------------------------------

data RawMoments dp = RawMoments
    { m0 :: !(Scalar dp)
    , m1 :: !(Vector (Scalar dp))
    , m2 :: !(Matrix (Scalar dp))
    }

deriving instance (Element (Scalar dp), Read (Scalar dp)) => Read (RawMoments dp)
deriving instance (Element (Scalar dp), Show (Scalar dp)) => Show (RawMoments dp)

instance NFData (RawMoments dp) where
    rnf rm = seq (m0 rm) 
           $ seq (m1 rm) 
           $ seq (m2 rm) ()

---------------------------------------

covar :: MultiNormal dp -> Matrix (Scalar dp)
covar (UnitLift1 m) = covarM m

mkMultiNormal' :: 
    ( Storable (Scalar dp)
    , Element (Scalar dp)
    , Field (Scalar dp)
    , Floating (Scalar dp)
    ) => RawMoments dp -> MultiNormal' dp
mkMultiNormal' raw = MultiNormal'
    { raw = raw
    , meanV = meanV' 
    , covarM = covarM'
    , invcovarM = inv covarM'
    , pdfdenom = 1/(sqrt $ det covarM' * (2*pi)^d)
    }
    where
        n = m0 raw
        d = dim $ m1 raw
        meanV' = mapVector (/n) (m1 raw)
        covarM' = mapMatrixWithIndex (\(i,j) mij -> (mij/n-(meanV' @> j)*(meanV' @> i))) (m2 raw)

-------------------------------------------------------------------------------
-- algebra

type instance Scalar (MultiNormal dp) = Scalar dp

instance 
    ( Container Matrix (Scalar dp)
    , Container Vector (Scalar dp)
    , Field (Scalar dp)
    , Floating (Scalar dp)
    ) => SG.Semigroup (MultiNormal' dp) 
        where
    d1 <> d2 = mkMultiNormal' $ RawMoments 
        { m0 = m0 (raw d1) + m0 (raw d2)
        , m1 = m1 (raw d1) `LA.add` m1 (raw d2)
        , m2 = m2 (raw d1) `LA.add` m2 (raw d2)
        }

-------------------------------------------------------------------------------
-- training 

instance 
    ( Element r 
    , Container Vector r
    , Field r
    , Floating r
    , Scalar (dp r) ~ r
    , VG.Vector dp r
    ) => HomTrainer (MultiNormal (dp r)) 
        where
    type Datapoint (MultiNormal (dp r)) = dp r

    train1dp dp = UnitLift1 . mkMultiNormal' $ RawMoments
        { m0 = 1
        , m1 = fromList $ VG.toList dp 
        , m2 = n><n $ [i*j | i <- VG.toList dp, j <- VG.toList dp]
        }
        where
            n = length $ VG.toList dp

-------------------------------------------------------------------------------
-- distribution 

instance Probabilistic (MultiNormal dp) where
    type Probability (MultiNormal dp) = Scalar dp

instance 
    ( Element r
    , Container Vector r
    , Floating r
    , LA.Product r
    , Scalar (dp r) ~ r
    , F.Foldable dp 
    ) => PDF (MultiNormal (dp r)) 
        where
    pdf (UnitLift1 dist) dp = pdfdenom dist * (exp $ (-1/2) * top) 
        where
            x = fromList $ F.toList dp
            top=minElement $ ((asRow  x) `sub` (asRow $ meanV dist)) 
                       LA.<> (invcovarM dist) 
                       LA.<> ((asColumn x) `sub` (asColumn $ meanV dist))
