{-# LANGUAGE DataKinds,EmptyDataDecls #-}

module HLearn.Metrics.Mahalanobis.Lego
    where

import Control.DeepSeq
import Data.List
import Data.Proxy
import qualified Data.Semigroup as SG
import qualified Data.Foldable as F
import qualified Data.Vector.Generic as VG

import Foreign.Storable
import Numeric.LinearAlgebra hiding ((<>))
import qualified Numeric.LinearAlgebra as LA

import HLearn.Algebra
import HLearn.Metrics.Mahalanobis
import HLearn.Models.Distributions.Multivariate.MultiNormalFast

-------------------------------------------------------------------------------
-- data types

type instance Scalar (AddUnit1 (Lego' reg eta) dp) = Scalar (Lego' reg eta dp)

instance Field (Scalar dp) => MahalanobisMetric (AddUnit1 (Lego' reg eta) dp) where
    getMatrix (UnitLift1 m) = inv $ covar $ multinorm m

type Lego reg (eta::Frac) dp = AddUnit1 (Lego' reg eta) dp

data Lego' reg (eta::Frac) dp = Lego'
    { b :: !(Matrix (Scalar dp))
    , c :: !(Matrix (Scalar dp))
    , x :: Matrix (Scalar dp)
    , multinorm :: MultiNormal dp
    }
    
deriving instance (Element (Scalar dp), Show (Scalar dp)) => Show (Lego' reg eta dp)

instance NFData dp => NFData (Lego' reg eta dp) where
    rnf lego = deepseq b
             $ deepseq c
             $ rnf x

---------------------------------------

data RegMatrix_ = Identity_ | InvCovar_

class RegMatrix m where
    regmatrix :: m -> RegMatrix_

data Identity
instance RegMatrix Identity where
    regmatrix _ = Identity_

data InvCovar
instance RegMatrix InvCovar where
    regmatrix _ = InvCovar_

---------------------------------------

mkLego :: forall reg eta dp. 
    ( KnownFrac eta
    , MatrixField dp
    , RegMatrix reg
    ) => Matrix (Scalar dp) -> Matrix (Scalar dp) -> MultiNormal dp -> Lego' reg eta dp
mkLego b c multinorm = Lego'
    { b = b
    , c = c
    , multinorm = multinorm
    , x = inv $ completeTheSquare (scale (-1) b') (scale (-eta) c)
    }
    where
        m = case (regmatrix (undefined::reg)) of
            Identity_ -> ident (rows b)
            InvCovar_ -> inv $ covar multinorm
        b' = scale (1/2) (m `add` scale eta b)
        eta = fromRational $ fracVal (Proxy :: Proxy eta)

-- | given an equation Y^2 + BY + YB + C, solve for Y
completeTheSquare :: 
    ( Storable r
    , Element r
    , Container Vector r
    , LA.Product r
    , Field r
    , r ~ Double
    ) => Matrix r -> Matrix r -> Matrix r
-- completeTheSquare b c = (sqrtm' ((b LA.<> b) `sub` c)) `sub` b
completeTheSquare b c = (sqrtm' ((scale (1/4) $ b LA.<> b) `sub` c)) `sub` b
    where
        sqrtm' m = cmap realPart $ matFunc sqrt $ cmap (:+0) m

-------------------------------------------------------------------------------
-- algebra

instance (KnownFrac eta, RegMatrix reg, MatrixField dp) => SG.Semigroup (Lego' reg eta dp) where
    lego1 <> lego2 = mkLego  b' c' mn'
        where
            b' = b lego1 `add` b lego2
            c' = c lego1 `add` c lego2
            mn' = multinorm lego1 <> multinorm lego2

type instance Scalar (Lego' reg eta dp) = Scalar dp

type instance Scalar (Vector r) = r

-------------------------------------------------------------------------------
-- training

instance 
    ( KnownFrac eta
    , RegMatrix reg
    , MatrixField (Vector r)
    , r ~ Scalar (dp r)
    , VG.Vector dp r 
    ) => HomTrainer (Lego reg eta (dp r)) 
        where
    type Datapoint (Lego reg eta (dp r)) = (r,dp r)

    train1dp (y,dp) = UnitLift1 $ mkLego (scale y zzT) (zzT LA.<> zzT) (train1dp dp)
        where 
            zzT = asColumn dp' LA.<> asRow dp'
            dp' = fromList $ VG.toList dp

-------------------------------------------------------------------------------
-- metric

instance
    ( VG.Vector dp r
    , Scalar (dp r) ~ r
    , RegMatrix reg
    , LA.Product r
    ) =>  MkMahalanobis (Lego reg eta (dp r)) 
        where
    type MetricDatapoint (Lego reg eta (dp r)) = dp r
    mkMahalanobis (UnitLift1 lego) dp = Mahalanobis
        { rawdp = dp
        , moddp = VG.fromList $ toList $ flatten $ (x lego) LA.<> asColumn v
        }
        where
            v = fromList $ VG.toList dp

-------------------------------------------------------------------------------
-- test

xs = 
    [ [1,2]
    , [2,3]
    , [3,4]
    ]
    :: [[Double]]

mb = (2><2)[-6.5,-10,-10,-14]      :: Matrix Double
mc = (2><2)[282,388,388,537] :: Matrix Double
