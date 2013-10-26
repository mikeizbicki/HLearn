module HLearn.Metrics.Mahalanobis'
    where

import qualified Data.Foldable as F
import qualified Data.Semigroup as SG

import Foreign.Storable
import Numeric.LinearAlgebra hiding ((<>))
import qualified Numeric.LinearAlgebra as LA

import HLearn.Algebra
import HLearn.Models.Distributions.Common

-------------------------------------------------------------------------------
-- data types 

type Mahalanobis dp = AddUnit1 Mahalanobis' dp

data Mahalanobis' dp = Mahalanobis' 
    { raw      :: !(RawMoments dp)
    , meanV    :: Vector (Ring dp)
    , covar    :: Matrix (Ring dp)
    , invcovar :: Matrix (Ring dp)
    , pdfdenom :: Ring dp
    }
    deriving (Read,Show)

data RawMoments dp = RawMoments
    { m0 :: !(Ring dp)
    , m1 :: !(Vector (Ring dp))
    , m2 :: !(Matrix (Ring dp))
    }
    deriving (Read,Show)

mkMahalanobis' :: 
    ( Storable (Ring dp)
    , Element (Ring dp)
    , Field (Ring dp)
    , Floating (Ring dp)
    ) => RawMoments dp -> Mahalanobis' dp
mkMahalanobis' raw = Mahalanobis'
    { raw = raw
    , meanV = meanV' 
    , covar = covar'
    , invcovar = inv covar'
    , pdfdenom = 1/(sqrt $ det covar' * (2*pi)^d)
    }
    where
        n = m0 raw
        d = dim $ m1 raw
        meanV' = mapVector (/n) (m1 raw)
        covar' = mapMatrixWithIndex (\(i,j) mij -> (mij/n-(meanV' @> j)*(meanV' @> i))) (m2 raw)

-------------------------------------------------------------------------------
-- algebra

instance 
    ( Container Matrix (Ring dp)
    , Container Vector (Ring dp)
    , Field (Ring dp)
    , Floating (Ring dp)
    ) => SG.Semigroup (Mahalanobis' dp) 
        where
    d1 <> d2 = mkMahalanobis' $ RawMoments 
        { m0 = m0 (raw d1) + m0 (raw d2)
        , m1 = m1 (raw d1) `LA.add` m1 (raw d2)
        , m2 = m2 (raw d1) `LA.add` m2 (raw d2)
        }

-------------------------------------------------------------------------------
-- training 

instance Num a => HasRing [a] where 
    type Ring [a] = a

instance 
    ( Element r 
    , Container Vector r
    , Field r
    , Floating r
    , Ring (dp r) ~ r
    , F.Foldable dp
    ) => HomTrainer (Mahalanobis (dp r)) 
        where
    type Datapoint (Mahalanobis (dp r)) = dp r

    train1dp dp = UnitLift . mkMahalanobis' $ RawMoments
        { m0 = 1
        , m1 = fromList $ F.toList dp 
        , m2 = n><n $ [i*j | i <- F.toList dp, j <- F.toList dp]
        }
        where
            n = length $ F.toList dp

-------------------------------------------------------------------------------
-- distribution 

instance Probabilistic (Mahalanobis dp) where
    type Probability (Mahalanobis dp) = Ring dp

instance 
    ( Element r
    , Container Vector r
    , Floating r
    , LA.Product r
    , Ring (dp r) ~ r
    , F.Foldable dp 
    ) => PDF (Mahalanobis (dp r)) 
        where
    pdf (UnitLift dist) dp = pdfdenom dist * (exp $ (-1/2) * top) 
        where
            x = fromList $ F.toList dp
            top=minElement $ ((asRow  x) `sub` (asRow $ meanV dist)) 
                       LA.<> (invcovar dist) 
                       LA.<> ((asColumn x) `sub` (asColumn $ meanV dist))
