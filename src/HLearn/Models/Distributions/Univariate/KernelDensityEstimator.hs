{-# LANGUAGE DataKinds #-}
-- | Kernel Density Estimation (KDE) is a generic and powerful method for estimating a probability distribution.  See wikipedia for more information: <http://en.wikipedia.org/wiki/Kernel_density_estimation>
module HLearn.Models.Distributions.Univariate.KernelDensityEstimator
    where

import Control.DeepSeq
import qualified Data.Map as Map
import GHC.TypeLits
import qualified Data.Foldable as F
import qualified Control.ConstraintKinds as CK

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Kernels
import HLearn.Models.Distributions.Visualization.Gnuplot
import HLearn.DataStructures.SortedVector

-------------------------------------------------------------------------------
-- data types

-- | The KDE type is implemented as an isomorphism with the FreeModule
newtype KDE kernel (h::Frac) (prob:: *) (dp:: *) = KDE
    { freemod :: SortedVector dp 
    }
    deriving (Read,Show,Eq,Ord,NFData,Monoid,Group,Abelian{-,Module-})

-------------------------------------------------------------------------------
-- Training
    
instance (Num (Ring (SortedVector dp))) => HasRing (KDE kernel h prob dp) where
    type Ring (KDE kernel h prob dp) = Ring (SortedVector dp) 
    
instance (Num prob, NumDP (SortedVector dp)) => NumDP (KDE kernel h prob dp) where
    numdp (KDE v) = numdp v 
    
instance (Num prob, Ord prob) => HomTrainer (KDE kernel h prob prob) where
    type Datapoint (KDE kernel h prob prob) = prob
    train1dp dp = KDE $ train1dp dp 

---------------------------------------

instance CK.Functor (KDE kernel h prob) where
    type FunctorConstraint (KDE kernel h prob) dp = Ord dp 
    fmap f = KDE . CK.fmap f . freemod

-------------------------------------------------------------------------------
-- Distribution
    
instance Probabilistic (KDE kernel h prob dp) where
    type Probability (KDE kernel h prob dp) = prob
    
instance 
    ( Function kernel prob prob
    , SingI h
    , Fractional prob
    , prob ~ Ring (SortedVector prob)
    , NumDP (SortedVector prob)
    ) => PDF (KDE kernel h prob prob) 
        where
    pdf kde dp = (1/(n*h))*(foldr (+) 0 $ map (\x -> f $ (dp-x)/h) dpList)
        where 
            f = function (undefined::kernel)
            n = numdp kde
            h = fromSing (sing :: Sing h)
--             dpList = Map.keys (getMap $ freemod kde)
            dpList = CK.toList (freemod kde) 

-- instance 
--     ( Floating prob
--     , Enum prob
--     , Show prob
--     , Ord prob
--     ) => PlottableDistribution (kDE kernel h prob prob) where
--     
--     plotType _ = Continuous
-- 
--     samplePoints dist = samplesFromMinMax min max
--         where
--             min = (mean dist)-5*(sqrt $ variance dist)
--             max = (mean dist)+5*(sqrt $ variance dist)
instance 
    ( PDF (KDE kernel h prob prob)
    , Function kernel prob prob
    , SingI h
    , Fractional prob
    , Show prob
    , Enum prob
    , Ord prob
    ) => PlottableDistribution (KDE kernel h prob prob) 
        where
    plotType _ = Continuous 
    samplePoints kde = map (/100) [-1000..3000]
        where
--             start = (freemod $ vector kde)
-- class 
--     ( Plottable (Datapoint dist)
--     , Plottable (Probability dist), Num (Probability dist)
--     , PDF dist
--     , MaybeShow (Datapoint dist)
--     ) => PlottableDistribution dist where
-- 
--     samplePoints :: dist -> [Datapoint dist]
--     plotType :: dist -> PlotType
--     
--     pdfL :: dist -> [Probability dist]
--     pdfL dist = map (pdf dist) $ samplePoints dist
-- 
--     plotdata :: dist -> String
--     plotdata dist = mconcat [maybeshow (x::Datapoint dist) ++ " " ++ show (pdf dist x::Probability dist) ++"\n" | x <- plotPoints]
--         where
--             plotPoints = samplePoints dist
