{-# LANGUAGE DataKinds,EmptyDataDecls #-}

module HLearn.Metrics.Mahalanobis.Mega
    where

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.List
import qualified Data.Semigroup as SG
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Algorithms.Intro as Intro

import Debug.Trace
import Unsafe.Coerce

import Foreign.Storable
import Numeric.LinearAlgebra hiding ((<>))
import qualified Numeric.LinearAlgebra as LA

import HLearn.Algebra
import HLearn.Metrics.Mahalanobis
import HLearn.Metrics.Mahalanobis.LegoPaper
import HLearn.Metrics.Mahalanobis.ITML hiding (_x)
import HLearn.Models.Distributions.Multivariate.MultiNormalFast
import qualified HLearn.Numeric.Recipes as Recipe

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

-- mkMega' :: forall eta. KnownFrac eta => Double -> [(Double,Vector Double)] -> Mega eta (Vector Double)
-- -- mkMega' etaraw !xs = Mega $ findMinAmoeba f init
-- mkMega' etaraw !xs = Mega $ findzero a b init
--     where
-- --         (ITML init) = train_ITML 10 xs
-- --         (LegoPaper init) = train_LegoPaper eta xs
--         init = identity
--         
--         identity = ident (LA.dim $ snd $ head xs)
--         xxt x = asColumn x LA.<> asRow x
--         a = foldl1' LA.add $ map (\(_,x) -> eta `scale` kronecker (xxt x) (xxt x)) xs
--         b = foldl' LA.add (vec identity) $ map (\(y,x) -> vec $ (eta*y) `scale` xxt x) xs
-- 
--         eta = etaraw 


mkMega' :: forall eta. KnownFrac eta => Double -> [(Double,Vector Double)] -> Mega eta (Vector Double)
mkMega' etaraw !xs = {-trace ("megamatrix="++show res) $ -} Mega $ res 
    where
        res = Recipe.conjugateGradientDescent f f' init
        x0 = identity
        (ITML init) = train_ITML 10 xs
        
        identity = ident (LA.dim $ snd $ head xs)
        xxt x = asColumn x LA.<> asRow x
        a = foldl1' LA.add $ map (\(_,x) -> eta `scale` kronecker (xxt x) (xxt x)) xs
        b = foldl' LA.add (vec identity) $ map (\(y,x) -> vec $ (eta*y) `scale` xxt x) xs

--         q_zz = foldl1' LA.add $ map (\(y,x) -> let z=asColumn x in (vec z)*(trans $ vec z) * (trans $ (vec z)*(trans $ vec z))) xs
--         q_yz = foldl1' LA.add $ map (\(y,x) -> let z=asColumn x in scale ((-2)*y) (z `LA.mul` trans z)) xs
        q_zz = foldl1' LA.add $ map (\(y,x) -> (asColumn $ flatten $ xxt x) LA.<> (asRow$flatten$xxt x)) xs
        q_yz = foldl1' LA.add $ map (\(y,x) -> scale (-2*y) (xxt x)) xs
        q_yy = sum $ map fst xs

        eta = etaraw 

        tr = LA.sumElements . LA.takeDiag
        f x   = -- trace ("x="++show x++"\n\n\nvec x="++show (vec x)++"\n\n\nq_zz="++show q_zz) $ 
--                 trace ("det="++show (LA.det $ x LA.<> LA.inv x0))
                (tr $ x LA.<> x0)
              + if det < 0
                    then 1e100
                    else log det
              + (maxElement $ (trans $ vec x) LA.<> q_zz LA.<> (vec x))
              + (sumElements $ q_yz * x)
              + q_yy
            where det = LA.det $ x LA.<> LA.inv x0 
        f' x  = reshape (rows x) $ flatten $ (scale (-1) $ vec (inv x)) `LA.add` (a LA.<> vec x) `LA.sub` b
--         f' x  = (scale (-1) $ vec (inv x)) `LA.add` (a LA.<> vec x) `LA.sub` b
        f'' x = kronecker (inv x) (inv x) `LA.add` a 

megaZero a b x0 = Recipe.conjugateGradientDescent f f' x0 
    where
        tr = LA.sumElements . LA.takeDiag
        f x   = (tr $ x `LA.mul` x0) + (log $ LA.det $ x `LA.mul` LA.inv x0) 
        f' x  = (scale (-1) $ vec (inv x)) `LA.add` (a LA.<> vec x) `LA.sub` b
        f'' x = kronecker (inv x) (inv x) `LA.add` a 

-- stepLineMin f f' lower mid upper



-- g :: Matrix Double -> Matrix Double
g :: LA.Vector Double -> Double
g x = VG.sum $ VG.map abs $ x `LA.add` (LA.fromList [2,3])

y0 = LA.fromList [1,2::Double]

m1 = 2 >< 2 $ [1,2,2,4::Double]
m2 = 2 >< 2 $ [3,1,1,4::Double]


findzero :: Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double
findzero !a !b !x0 = newtonRaphson x0 x0' 1000 --0000
    where
        x0' = x0 `LA.sub` (((rows x0)><(cols x0)) $ repeat 1)

--         gradientDescent !x 0 = x
--         gradientDescent x i = gradientDescent x' (i-1)
--             where
--                 x' = 
--                 xinv = inv x
--                 df = kronecker xinv xinv `LA.add` a

        newtonRaphson !x !lastx !itr = --trace ("x="++show x++"\nx'="++show x'++"\n\n") $
          if itr==0
            then trace ("WARNING: findzero probably failed to converge; diff="++show diff++"; ratio="++show ratio) x
            else if diff <= 1e-8 || ratio <= 1e-8
                then x 
                else newtonRaphson x' x (itr-1)
            where
                diff = (maxElement $ cmap abs $ x `LA.sub` lastx) 
                ratio = (maxElement $ cmap abs $ x `LA.sub` lastx) / maxElement (cmap abs x)
                xinv = inv x
                f = (vec $ (-1) `scale` xinv) `LA.add` (a LA.<> vec x) `LA.sub` b
                df = kronecker xinv xinv `LA.add` a
--                 x' = if itr > 20
--                     then x `LA.sub` reshape (rows x) (flatten $ scale (0.1) df)
--                     else x `LA.sub` reshape (rows x) (flatten $ inv df LA.<> f)

                epsilon = 1e-13 -- 0.000000000000001 -- *(maxElement f/ fromIntegral (rows x*rows x))
--                 x' = x `LA.sub` reshape (rows x) (flatten $ scale epsilon f)
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

