{-# LANGUAGE DataKinds,EmptyDataDecls #-}

module HLearn.Metrics.Mahalanobis.Mega
    where

import Control.DeepSeq
import Data.List
import qualified Data.Semigroup as SG
import qualified Data.Foldable as F
import qualified Data.Vector.Generic as VG
import Debug.Trace

import Foreign.Storable
import Numeric.LinearAlgebra hiding ((<>))
import qualified Numeric.LinearAlgebra as LA

import HLearn.Algebra
import HLearn.Metrics.Mahalanobis
import HLearn.Metrics.Mahalanobis.LegoPaper
import HLearn.Models.Distributions.Multivariate.MultiNormalFast

-------------------------------------------------------------------------------
-- data types

newtype Mega (eta::Frac) dp = Mega
    { _x :: Matrix (Ring dp)
    }
    
deriving instance (Element (Ring dp), Show (Ring dp)) => Show (Mega eta dp)

instance (Storable (Ring dp), NFData (Ring dp), NFData dp) => NFData (Mega eta dp) where
    rnf mega =  rnf $ _x mega

instance HasRing dp => MahalanobisMetric (Mega eta dp) where
    getMatrix mega =  _x mega

-------------------------------------------------------------------------------
-- algebra

instance HasRing dp => HasRing (Mega eta dp) where
    type Ring (Mega eta dp) = Ring dp

-------------------------------------------------------------------------------
-- training

instance Num r => HasRing (Vector r) where
    type Ring (Vector r) = r

mkMega :: forall container eta. 
    ( Ring (container Double) ~ Double
    , VG.Vector container Double
    , KnownFrac eta
    ) => Double
      -> [(Double,container Double)] 
      -> Mega eta (container Double)
mkMega etaraw !xs = Mega $ _x $ (mkMega' etaraw xs'::Mega eta (Vector Double))
    where
        xs' = map (\(y,x) -> (y, fromList $ VG.toList x)) xs

mkMega' :: forall eta. KnownFrac eta => Double -> [(Double,Vector Double)] -> Mega eta (Vector Double)
-- mkMega' !xs = Mega $ lego
mkMega' etaraw !xs = Mega $ findzero a b lego
    where
        (LegoPaper lego) = train_LegoPaper eta xs
        identity = ident (LA.dim $ snd $ head xs)
        xxt x = asColumn x LA.<> asRow x
        -- TODO: remove kronecker
        a = foldl1' LA.add $ map (\(_,x) -> eta `scale` kronecker (xxt x) (xxt x)) xs
        b = foldl' LA.add (vec identity) $ map (\(y,x) -> vec $ (eta*y) `scale` xxt x) xs

        eta = etaraw -- /fromIntegral (length xs)

--         eta = fromSing (sing :: Sing eta)

findzero :: Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double
findzero !a !b !x0 =go x0 x0' 20
    where
        x0' = x0 `LA.sub` (((rows x0)><(cols x0)) $ repeat 1)

        go !x !lastx !itr = if itr==0
            then trace ("WARNING: findzero probably failed to converge; diff="++show diff++"; ratio="++show ratio) x
            else if diff <= 1e-6 || ratio <= 1e-6
                then x 
                else go x' x (itr-1)
            where
                diff = (maxElement $ cmap abs $ x `LA.sub` lastx) 
                ratio = (maxElement $ cmap abs $ x `LA.sub` lastx) / maxElement (cmap abs x)
                xinv = inv x
                f = (vec $ (-1) `scale` xinv) `LA.add` (a LA.<> vec x) `LA.sub` b
                df = kronecker xinv xinv `LA.add` a
                x' = x `LA.sub` reshape (rows x) (flatten $ inv df LA.<> f)

vec :: Matrix Double -> Matrix Double
vec = asColumn . flatten

-------------------------------------------------------------------------------
-- metric

instance
    ( VG.Vector dp r
    , Ring (dp r) ~ r
    , LA.Product r
    ) =>  MkMahalanobis (Mega eta (dp r)) 
        where
    type MetricDatapoint (Mega eta (dp r)) = dp r
    mkMahalanobis mega dp = Mahalanobis
        { rawdp = dp
        , moddp = VG.fromList $ toList $ flatten $ (_x mega) LA.<> asColumn v
        }
        where
            v = fromList $ VG.toList dp

-------------------------------------------------------------------------------
-- test

-- xs = 
--     [ [1,2]
--     , [2,3]
--     , [3,4]
--     ]
--     :: [[Double]]

xs = 
    [ (1, fromList [2,3])
    , (1, fromList [3,3])
    , (1, fromList [3,-2])
    ] :: [(Double,Vector Double)]

mb = (2><2)[-6.5,-10,-10,-14]      :: Matrix Double
mc = (2><2)[282,388,388,537] :: Matrix Double

