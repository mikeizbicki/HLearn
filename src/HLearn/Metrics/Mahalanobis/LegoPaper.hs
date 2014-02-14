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

newtype LegoPaper dp = LegoPaper ( Matrix (Ring dp) )

deriving instance (Element (Ring dp), Show(Ring dp)) => Show (LegoPaper dp)

instance HasRing dp => HasRing (LegoPaper dp) where
    type Ring (LegoPaper dp) = Ring dp

instance MahalanobisMetric (LegoPaper dp) where
    getMatrix (LegoPaper x) = x

fromSingleton :: (Element r, Container Vector r, Storable r) => Matrix r -> r
fromSingleton m = if rows m /= 1 || cols m /= 1
    then error "fromSingleton on not 1x1 matrix"
    else atIndex m $ minIndex m

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
    F.foldl' (add1dp_LegoPaper eta) reg $ fmap (\(y,v) -> (y,fromList $ VG.toList v)) $ dps'
    where
        dps' = evalRand (shuffle $ concat $ replicate rep $ F.toList dps) $ mkStdGen 10
        reg = LegoPaper $ ident $ VG.length $ snd $ head $ F.toList dps

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
    , r ~ Ring (vec r)
    , Show r
    , Ord r
    ) => r -> LegoPaper (vec r) -> (r,Vector r) -> LegoPaper (vec r)
add1dp_LegoPaper eta (LegoPaper a) (y,vec) = 
    -- trace ("yhat="++show yhat++"; ybar="++show ybar++"; ybar_det="++show ybar_det++"; y="++show y++"; vec="++show vec++"; a="++show a) $
    if yhat<0.00001
        then LegoPaper a
        else LegoPaper $ a `LA.sub` (scale (eta*(ybar-y)/(1+eta*(ybar-y)*yhat)) $ a LA.<> z LA.<> zT LA.<> a) 
    where
        zT = asRow vec
        z = asColumn vec
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
    mkMahalanobis (LegoPaper a) vec = Mahalanobis
        { rawdp = vec
        , moddp = VG.fromList $ toList $ flatten $ a LA.<> asColumn v
        }
        where
            v = fromList $ VG.toList vec
