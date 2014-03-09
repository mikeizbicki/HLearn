module HLearn.Metrics.Mahalanobis.LegoPaper
    where

import Control.Monad
import Control.Monad.Random hiding (fromList)
import Control.Monad.ST
import Data.Array.ST
import GHC.Arr
import qualified Data.Foldable as F
import qualified Data.Vector.Generic as VG
import Debug.Trace

import Foreign.Storable
import Numeric.LinearAlgebra hiding ((<>))
import qualified Numeric.LinearAlgebra as LA

import HLearn.Algebra
import HLearn.Metrics.Mahalanobis

-------------------------------------------------------------------------------
-- data types

data LegoPaper dp = LegoPaper 
    { _matrix :: Matrix (Ring dp)
    , _xs :: [(Ring dp,dp)]
    , _numdp :: Ring dp
    }

deriving instance (Element (Ring dp), Show(Ring dp), Show dp) => Show (LegoPaper dp)

instance HasRing dp => HasRing (LegoPaper dp) where
    type Ring (LegoPaper dp) = Ring dp

instance MahalanobisMetric (LegoPaper dp) where
    getMatrix (LegoPaper x _ _) = x

fromSingleton :: (Element r, Container Vector r, Storable r) => Matrix r -> r
fromSingleton m = if rows m /= 1 || cols m /= 1
    then error "fromSingleton on not 1x1 matrix"
    else atIndex m $ minIndex m

-- mappend1 :: (Field (Ring dp)) => LegoPaper dp -> LegoPaper dp -> LegoPaper dp
mappend1 m1 m2 = LegoPaper 
    { _matrix = (scale (_numdp m1/totdp) $ _matrix m1) `LA.add` (scale (_numdp m2/totdp) $ _matrix m2)
    , _numdp = _numdp m1+_numdp m2
    , _xs = _xs m1++_xs m2
    }
    where
        totdp = _numdp m1+_numdp m2

mappend2 eta m1 m2 = mappend1 m1' m2'
    where
        m1' = m1 { _matrix = _matrix m1'' }
        m2' = m1 { _matrix = _matrix m2'' }
--         m1'' = F.foldl' (add1dp_LegoPaper eta) m1 $ take 100 xs1'
--         m2'' = F.foldl' (add1dp_LegoPaper eta) m2 $ take 100 xs2'
        m1'' = F.foldl' (add1dp_LegoPaper eta) m1 xs1'
        m2'' = F.foldl' (add1dp_LegoPaper eta) m2 xs2'

        xs1' = evalRand (shuffle $ _xs m1) (mkStdGen 10)
        xs2' = evalRand (shuffle $ _xs m2) (mkStdGen 10)

-- train_LegoMonoid :: 
--     ( F.Foldable container
--     , Functor container
--     , VG.Vector vec r
--     , Container Vector r
--     , LA.Product r
--     , Floating r
--     , r ~ Ring (vec r)
--     , Show r
--     , Ord r
--     ) => Int -> r -> container (r,vec r) -> LegoPaper (vec r) 
train_LegoMonoid monoid k1 k2 eta dps = F.foldl1 monoid ms
    where
        ms = map (train_LegoPaper 0 eta) dps'
        dps' = take k2 [takeEvery k1 $ drop j dps | j<-[0..k1-1]]

        takeEvery n [] = []
        takeEvery n xs = head xs : (takeEvery n $ drop n xs)

train_LegoPaper :: 
    ( F.Foldable container
    , Functor container
    , VG.Vector vec r
    , Container Vector r
    , LA.Product r
    , Floating r
    , r ~ Ring (vec r)
    , Show r
    , Ord r
    ) => Int -> r -> container (r,vec r) -> LegoPaper (vec r) 
train_LegoPaper rep eta dps = 
    F.foldl' (add1dp_LegoPaper eta) reg dps'
    where
--         dps' = evalRand (shuffle $ concat $ replicate rep $ F.toList dps) $ mkStdGen 10
        dps' = dps 
        reg = LegoPaper 
            { _matrix = ident $ VG.length $ snd $ head $ F.toList dps
            , _numdp = 0 -- fromIntegral $ length $ F.toList dps
            , _xs = []
            }

shuffle :: RandomGen g => [a] -> Rand g [a]
shuffle xs = do
    let l = length xs
    rands <- take l `fmap` getRandomRs (0, l-1)
    let ar = runSTArray $ do
            ar <- thawSTArray $ listArray (0, l-1) xs
            forM_ (zip [0..(l-1)] rands) $ \(i, j) -> do
                vi <- readSTArray ar i
                vj <- readSTArray ar j
                writeSTArray ar j vi
                writeSTArray ar i vj
            return ar
    return (elems ar)

add1dp_LegoPaper :: 
    ( Storable r
    , LA.Product r
    , Floating r
    , Container Vector r
    , VG.Vector vec r
    , r ~ Ring (vec r)
    , Show r
    , Ord r
    ) => r -> LegoPaper (vec r) -> (r,vec r) -> LegoPaper (vec r)
add1dp_LegoPaper eta lp (y,vec) = LegoPaper
    { _matrix = mat
    , _numdp = _numdp lp+1
    , _xs = (y,vec):(_xs lp)
    }
    -- trace ("yhat="++show yhat++"; ybar="++show ybar++"; ybar_det="++show ybar_det++"; y="++show y++"; vec="++show vec++"; a="++show a) $
    where
        a = _matrix lp
        mat = if yhat<0.00001
            then a 
            else a `LA.sub` (scale (eta*(ybar-y)/(1+eta*(ybar-y)*yhat)) $ a LA.<> z LA.<> zT LA.<> a)

        vec' = VG.convert vec
        zT = asRow vec'
        z = asColumn vec'
        yhat = fromSingleton $ zT LA.<> a LA.<> z
        ybar =  ybar_top / ybar_bot
        ybar_bot = 2*eta*yhat
        ybar_top = (eta*y*yhat - 1 + (sqrt ybar_det))
        ybar_det = (eta*y*yhat-1)**2 + 4*eta*yhat**2
    

instance
    ( VG.Vector vec r
    , Ring (vec r) ~ r
    , LA.Product r
    ) =>  MkMahalanobis (LegoPaper (vec r))
        where
    type MetricDatapoint (LegoPaper (vec r)) = vec r
    mkMahalanobis (LegoPaper a _ _) vec = Mahalanobis
        { rawdp = vec
        , moddp = VG.fromList $ toList $ flatten $ a LA.<> asColumn v
        }
        where
            v = fromList $ VG.toList vec
