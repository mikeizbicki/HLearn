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
import HLearn.Models.Distributions.Multivariate.MultiNormalFast

-------------------------------------------------------------------------------
-- data types

data Mega (eta::Frac) dp = Mega
    { _x :: Matrix (Ring dp)
    }
    
deriving instance (Element (Ring dp), Show (Ring dp)) => Show (Mega eta dp)

instance (Storable (Ring dp), NFData (Ring dp), NFData dp) => NFData (Mega eta dp) where
    rnf mega =  rnf $ _x mega

-------------------------------------------------------------------------------
-- algebra

-------------------------------------------------------------------------------
-- training

instance Num r => HasRing (Vector r) where
    type Ring (Vector r) = r

mkMega :: forall container eta. 
    ( F.Foldable container
    , Ring (container Double) ~ Double
    , SingI eta
    ) => [(Double,container Double)] 
      -> Mega eta (container Double)
mkMega xs = Mega $ _x $ (mkMega' xs'::Mega eta (Vector Double))
    where
        xs' = map (\(y,x) -> (y, fromList $ F.toList x)) xs

mkMega' :: forall eta. SingI eta => [(Double,Vector Double)] -> Mega eta (Vector Double)
mkMega' xs = Mega $ findzero a b identity
    where
        identity = ident (LA.dim $ snd $ head xs)
        xxt x = asColumn x LA.<> asRow x
        a = foldl1' LA.add $ map (\(_,x) -> eta `scale` kronecker (xxt x) (xxt x)) xs
        b = foldl' LA.add (vec identity) $ map (\(y,x) -> vec $ (eta*y) `scale` xxt x) xs

        eta = fromSing (sing :: Sing eta)

findzero :: Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double
findzero a b x0 = go x0 x0'
    where
        x0' = x0 `LA.sub` (((rows x0)><(cols x0)) $ repeat 1)
        go x lastx = 
          if (maxElement $ cmap abs $ x `LA.sub` lastx) <= 1e-6
            then x 
            else go x' x 
            where
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

