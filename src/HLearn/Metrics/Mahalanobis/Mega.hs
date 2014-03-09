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
import HLearn.Metrics.Mahalanobis.Normal
import HLearn.Metrics.Mahalanobis.LegoPaper
import HLearn.Metrics.Mahalanobis.ITML hiding (_x)
import HLearn.Models.Distributions.Multivariate.MultiNormalFast
import qualified HLearn.Numeric.Conic as Recipe
import qualified HLearn.Numeric.Recipes as Recipe
import qualified HLearn.Numeric.Recipes.GradientDescent as Recipe
import qualified HLearn.Numeric.Recipes.Amoeba as Recipe

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

mkMega :: forall container eta. 
    ( Ring (container Double) ~ Double
    , VG.Vector container Double
    , KnownFrac eta
    ) => Double
      -> Double
      -> [(Double,container Double)] 
      -> Mega eta (container Double)
mkMega etaraw eta2 !xs = Mega $ _x $ (mkMega' etaraw eta2 xs'::Mega eta (Vector Double))
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


mkMega' :: forall eta. KnownFrac eta => Double -> Double -> [(Double,Vector Double)] -> Mega eta (Vector Double)
mkMega' etaraw eta2 !xs = {-trace ("megamatrix="++show res) $ -} 
    deepseq lego $ deepseq itml $ deepseq res $
    trace ("rank q_zz="++show (LA.rank (q_zz :: Matrix Double))++"; rank a="++show (LA.rank a)) $
    trace ("lego score="++show (f lego)) $
    trace ("itml score="++show (f itml)) $
    trace ("mega score="++show (f res)) $
    trace ("trace  = "++show (tr $ res)) $ 
    trace ("logdet = "++show (log $ det res)) $
    trace ("zAzzAz = "++show (0.5*eta*(sumElements $ (trans $ vec res) LA.<> q_zz LA.<> (vec res)))) $
    trace ("yzAz   = "++show (0.5*eta*(sumElements $ scale (-2) $ q_yz * res))) $
    trace ("yy     = "++show (0.5*eta*q_yy)) $
    trace ("sum    = "++show (0.5*eta*((sumElements $ (trans $ vec res) LA.<> q_zz LA.<> (vec res))
                                      +(sumElements $ scale (-2) $ q_yz * res)
                                      +q_yy))) $
--     trace ("eigenvalues q_zz="++show (eigenvalues q_zz)) $
--     deepseq res $ deepseq res2 $ deepseq res3 $
--     trace ("res="++show res++"\n\nres2="++show res2++"\n\nres3="++show res3) $
    Mega $ res 
    where
        magicJumble2 0 x = x
        magicJumble2 i x = magicJumble2 (i-1) 
--                        $ trace "---------------\nunderscore" $ Recipe.newtonRaphson_constrained f_ f'_ f''_
--                        $ trace "---------------\nplain" $ Recipe.newtonRaphson_constrained f f'c f''
                       $ Recipe.conjugateGradientDescent f f'
                       $ Recipe.newtonRaphson_constrained f f'c f''
                       $ x
        
        magicJumble 0 x = x
        magicJumble i x = magicJumble (i-1) x'
            where
                x_proj = Recipe.conicProjection f
                       $ Recipe.newtonRaphson_constrained f f'c f''
                       $ x
                
                fx = f x
                fx_proj = f x_proj

                x' = Recipe.randomConicPersuit f $ if fx_proj < fx
                    then x_proj
                    else x

        res = 
            Recipe.newtonRaphson_constrained f f'c f''
            $ Recipe.conjugateGradientDescent f f'
            $ x0

        res1 = findzero a b x0
        res2 = findzero a b $ Recipe.conjugateGradientDescent f f' x0
        res3 = findzero a b itml

--         x0 = identity
        x0 = mahalanobis
        mahalanobis = getMatrix $ (train $ map snd xs :: MahalanobisParams (Vector Double))
        (ITML itml) = train_ITML 10 xs -- :: ITML (Double,Vector Double)
        (LegoPaper lego _ _) = train_LegoPaper 1 0.01 xs -- :: LegoPaper (Double,Vector Double)
        
        identity = ident (LA.dim $ snd $ head xs)
        xxt x = asColumn x LA.<> asRow x
--         a = foldl1' LA.add $ map (\(_,x) -> eta `scale` kronecker (xxt x) (xxt x)) xs
        a = scale eta q_zz
        b = foldl' LA.add (vec identity) $ map (\(y,x) -> vec $ (eta*y) `scale` xxt x) xs

--         q_zz = foldl1' LA.add $ map (\(y,x) -> (asColumn$flatten$xxt x) LA.<> (asRow$flatten$xxt x)) xs
--         q_yz = foldl1' LA.add $ map (\(y,x) -> scale (-2*y) (xxt x)) xs
        q_z  = sum $ map (\(y,x) -> xxt x) xs
        q_zz = q_zz_mock
        q_zz_lowrank = (head $ head $ toBlocks [size] [rank] u) 
                 LA.<> (diagRect 0 (VG.slice 0 rank sigma) rank rank)
                 LA.<> (head $ head $ toBlocks [rank] [size] $ trans v) :: Matrix Double
--         rank=rows x0
        rank=floor  $ (fromIntegral size::Double)/2
        size=rows q_zz_full
        (u,sigma,v) = svd q_zz_full
        q_zz_full = sum $ map (\(y,x) -> kronecker (xxt x) (xxt x)) xs
        q_zz_mock = kronecker zz zz
            where
                zz = sum $ (map (\(y,x) -> xxt x)) xs
        q_yz = sum $ map (\(y,x) -> scale y (xxt x)) xs
        q_yy = sum $ map (\(y,x) -> y^2) xs
        q_zzk= sum $ map (\(y,x) -> kronecker (xxt x) (xxt x)) xs

        eta = etaraw 
--         eta = etaraw / fromIntegral (length xs)

        tr = LA.sumElements . LA.takeDiag

        f :: Matrix Double -> Double
        f x = tr prod
            + if det < 0
                then infinity
                else (-1) * log det
            + 0.5*eta*
                ( (sumElements $ (trans $ vec x) LA.<> q_zz LA.<> (vec x))
                + (sumElements $ scale (-2) $ q_yz * x)
                + q_yy
                )
            + eta2 * (pnorm Frobenius x)
            where 
                det = LA.det prod
                prod = x LA.<> LA.inv x0

        f_ x = tr (x LA.<> LA.inv x0)
            + 0.5*eta*
                ( (sumElements $ (trans $ vec x) LA.<> q_zz LA.<> (vec x))
                + (sumElements $ scale (-2) $ q_yz * x)
                + q_yy
                )
        f'_ x = (a LA.<> vec x) `LA.sub` b
        f''_ x = a

        f' x = f'a x + scale (eta2*2) x
        f'a x = reshape (rows x) $ flatten $ (vec $ (-1) `scale` inv x) `LA.add` (a LA.<> vec x) `LA.sub` b
        f'b x = inv x0 - inv x + scale eta ((reshape (rows x) $ flatten $ q_zzk LA.<> vec x) - q_yz)
        f'c x = (vec $ (-1) `scale` inv x) `LA.add` (a LA.<> vec x) `LA.sub` b

        f'' x = kronecker xinv xinv `LA.add` a
              + scale eta2 (ident $ (rows xinv)^2)
--         f'' x = reshape (rows x) $ flatten $ kronecker xinv xinv `LA.add` a
            where
                xinv = inv x

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

