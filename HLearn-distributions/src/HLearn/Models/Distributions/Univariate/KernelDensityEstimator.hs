-- | Kernel Density Estimation (KDE) is a generic and powerful method for estimating a probability distribution.  See wikipedia for more information: <http://en.wikipedia.org/wiki/Kernel_density_estimation>
module HLearn.Models.Distributions.Univariate.KernelDensityEstimator
    where

import Control.DeepSeq
import qualified Data.Map as Map
import GHC.TypeLits

import qualified Control.ConstraintKinds as CK

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Kernels

-------------------------------------------------------------------------------
-- data types

-- | The KDE type is implemented as an isomorphism with the FreeModule
newtype KDE kernel (h::Nat) prob = KDE
    { freemod :: FreeModule Int prob
    }
    deriving (Read,Show,Eq,Ord,NFData,Monoid,Group,Abelian,Module)

-------------------------------------------------------------------------------
-- Training
    
instance (Num prob) => HasRing (KDE kernel h prob) where
    type Ring (KDE kernel h prob) = prob
    
instance (Num prob) => NumDP (KDE kernel h prob) where
    numdp kde = fromIntegral $ Map.foldr (+) 0 $ getMap $ freemod kde
    
instance (Num prob, Ord prob) => HomTrainer (KDE kernel h prob) where
    type Datapoint (KDE kernel h prob) = prob
    train1dp dp = KDE $ FreeModule $ Map.singleton dp 1

---------------------------------------

instance CK.Functor (KDE kernel h) where
    type FunctorConstraint (KDE kernel h) prob = Ord prob
    fmap f = KDE . CK.fmap f . freemod

-------------------------------------------------------------------------------
-- Distribution
    
instance Probabilistic (KDE kernel h prob) where
    type Probability (KDE kernel h prob) = prob
    
instance (Kernel kernel prob, SingI h, Fractional prob) => PDF (KDE kernel h prob) where
    pdf kde dp = (1/(n*h))*(foldr (+) 0 $ map (\x -> f $ (dp-x)/h) dpList)
        where 
            f = evalKernel (undefined::kernel)
            n = numdp kde
            h = fromIntegral $ fromSing (sing :: Sing h)
            dpList = Map.keys (getMap $ freemod kde)
