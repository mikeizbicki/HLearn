module HLearn.Metrics.Mahalanobis.ITML
    where

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.List
import Data.STRef
import qualified Data.Semigroup as SG
import qualified Data.Foldable as F
import qualified Data.Vector.Generic as VG
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
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

newtype ITML dp = ITML
    { _x :: Matrix (Ring dp)
    }
    
deriving instance (Element (Ring dp), Show (Ring dp)) => Show (ITML dp)

instance (Storable (Ring dp), NFData (Ring dp), NFData dp) => NFData (ITML dp) where
    rnf mega =  rnf $ _x mega

instance HasRing dp => MahalanobisMetric (ITML dp) where
    getMatrix mega =  _x mega

-------------------------------------------------------------------------------
-- algebra

instance HasRing dp => HasRing (ITML dp) where
    type Ring (ITML dp) = Ring dp

instance HasRing (Vector Double) where 
    type Ring (Vector Double) = Double

-------------------------------------------------------------------------------
-- training

train_ITML :: 
    ( VG.Vector container Double
    , Ring (container Double) ~ Double
    ) => Double -> [(Double,container Double)] -> ITML (container Double)   
train_ITML gamma xs = ITML . _x $ train_ITML' xs' gamma u l 
    where
        xsL = map (\(dist,x) -> (dist<avedist,fromList $ VG.toList x)) xs
        xs' = V.fromList xsL 
        avedist = (sum $ map fst xs) / (fromIntegral $ length xs) :: Double
        u = minimum $ map fst xs 
        l = maximum $ map fst xs

train_ITML' :: 
    ( VG.Vector v (Bool,Vector Double)
    ) => v (Bool,Vector Double) -> Double -> Double -> Double -> ITML (Vector Double)
train_ITML' vec gamma u l = ITML $ runST $ do

    -- initialize mutabele variables
    aref <- newSTRef $ ident dim
    lambda <- VUM.replicate (VG.length vec) (0::Double)
    
    xsi :: VUM.MVector s Double <- VUM.new (VG.length vec)
    forM [0..VG.length vec-1] $ \i -> do
        if fst $ vec VG.! i
            then VUM.write xsi i u
            else VUM.write xsi i l

    -- perform training
    forM [0..10] $ \j -> do
        forM [0..VG.length vec-1] $ \i -> do
            let (similar,x) = vec VG.! i
                delta = if similar then 1 else -1

            a <- readSTRef aref
            let p = minElement $ asRow x LA.<> a LA.<> asColumn x
                
            lambda_i <- VUM.read lambda i
            xsi_i <- VUM.read xsi i
             
            let alpha = min lambda_i (delta/2*(1/p - gamma/xsi_i))
                beta = delta*alpha/(1-delta*alpha*p)

            VUM.write xsi i $ gamma*xsi_i / (gamma+delta*alpha*xsi_i)
            VUM.write lambda i $ lambda_i - alpha
            
            writeSTRef aref $ a + scale beta (a LA.<> asColumn x LA.<> asRow x LA.<> a)

    readSTRef aref

    where
        dim = VG.length $ snd $ VG.head vec 

-------------------------------------------------------------------------------
-- metric

instance
    ( VG.Vector dp r
    , Ring (dp r) ~ r
    , LA.Product r
    ) =>  MkMahalanobis (ITML (dp r)) 
        where
    type MetricDatapoint (ITML (dp r)) = dp r
    mkMahalanobis mega dp = Mahalanobis
        { rawdp = dp
        , moddp = VG.fromList $ toList $ flatten $ (_x mega) LA.<> asColumn v
        }
        where
            v = fromList $ VG.toList dp

