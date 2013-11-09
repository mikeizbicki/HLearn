module HLearn.Metrics.Mahalanobis.Lego
    where

import Control.DeepSeq
import Data.List
import Data.Semigroup as SG
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

type Lego dp = AddUnit1 Lego' dp

data Lego' dp = Lego'
    { b :: !(Matrix (Ring dp))
    , c :: !(Matrix (Ring dp))
    , x :: Matrix (Ring dp)
    }
    deriving (Read,Show)

instance NFData dp => NFData (Lego' dp) where
    rnf lego = deepseq b
             $ deepseq c
             $ rnf x

---------------------------------------

mkLego :: MatrixField dp => Matrix (Ring dp) -> Matrix (Ring dp) -> Lego' dp
mkLego b c = Lego'
    { b = b
    , c = c
    , x = inv $ completeTheSquare (scale (-1) b') (scale (-1) c)
    }
    where
        b' = scale (1/2) (ident (rows b) `sub` b)

-- | given an equation Y^2 + BY + YB + C, solve for Y
completeTheSquare :: 
    ( Storable r
    , Element r
    , Container Vector r
    , LA.Product r
    , Field r
    ) => Matrix r -> Matrix r -> Matrix r
completeTheSquare b c = (sqrtm ((b LA.<> b) `sub` c)) `sub` b

-------------------------------------------------------------------------------
-- algebra

instance MatrixField dp => SG.Semigroup (Lego' dp) where
    lego1 <> lego2 = mkLego b' c'
        where
            b' = b lego1 `add` b lego2
            c' = c lego1 `add` c lego2

instance HasRing dp => HasRing (Lego' dp) where
    type Ring (Lego' dp) = Ring dp

instance Num r => HasRing (Vector r) where
    type Ring (Vector r) = r

-------------------------------------------------------------------------------
-- training

instance (MatrixField (Vector r), r ~ Ring (dp r), F.Foldable dp) => HomTrainer (Lego (dp r)) where
    type Datapoint (Lego (dp r)) = dp r

    train1dp dp = UnitLift1 $ mkLego zzT (zzT LA.<> zzT)
        where 
            zzT = asColumn dp' LA.<> asRow dp'
            dp' = fromList $ F.toList dp

-------------------------------------------------------------------------------
-- metric

instance
    ( VG.Vector dp r
    , Ring (dp r) ~ r
    , LA.Product r
    ) =>  MkMahalanobis (Lego (dp r)) 
        where
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
