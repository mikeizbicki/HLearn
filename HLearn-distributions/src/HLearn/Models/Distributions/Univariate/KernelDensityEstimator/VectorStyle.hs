{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE DataKinds #-}

-- | Kernel Density Estimation (KDE) is a generic and powerful method for estimating a probability distribution.  See wikipedia for more information: <http://en.wikipedia.org/wiki/Kernel_density_estimation>
module HLearn.Models.Distributions.KernelDensityEstimator
    ( 
    -- * Parameters
    KDEParams (..)
    , KDEBandwidth (..)
    
    -- * Data types
    , KDE
--     , KDE'
    
    -- * Easy creation of parameters
    , genSamplePoints
    
--     , module HLearn.Models.Distributions.KernelDensityEstimator.Kernels
    )
    where
          
import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Categorical
import HLearn.Models.Distributions.KernelDensityEstimator.Kernels

import qualified Control.ConstraintKinds as CK
import Control.DeepSeq
import qualified Data.Vector.Unboxed as VU

-------------------------------------------------------------------------------
-- Parameters

-- | The bandwidth is a \"free parameter\" that has a large influence on the resulting PDF.  The simplest way to set the bandwidth is using a constant.  However, we can also have a bandwidth that varies over the domain of the distribution.  This is done using the Variable constructor.  For more, see Wikipedia's entry on Variable KDE: <https://en.wikipedia.org/wiki/Variable_kernel_density_estimation>.

data KDEBandwidth prob = Constant prob | Variable (prob -> prob)

instance (Show prob) => Show (KDEBandwidth prob) where
    show (Constant x) = "Constant " ++ show x

instance (Eq prob) => Eq (KDEBandwidth prob) where
    Constant x1 == Constant x2 = x1==x2
    Variable f1 == Variable f2 = True
    _ == _ = False

instance (Ord prob) => Ord (KDEBandwidth prob) where
    Variable _ `compare` Variable _ = EQ
    Constant _ `compare` Variable _ = LT
    Variable _ `compare` Constant _ = GT
    Constant x `compare` Constant y = x `compare` y

instance (NFData prob) => NFData (KDEBandwidth prob) where
    rnf (Constant x) = rnf x
    rnf (Variable f) = seq f ()
    
{-# INLINE calcBandwidth #-}
calcBandwidth :: (KDEBandwidth prob) -> prob -> prob
calcBandwidth (Constant h) _ = h
calcBandwidth (Variable f) x = f x

data KDEParams prob = KDEParams
    { bandwidth :: KDEBandwidth prob
    , samplePoints :: VU.Vector prob -- ^ These data points must be sorted from smallest to largest
    , kernel :: KernelBox prob
    }
    deriving (Show,Eq,Ord)

instance (NFData prob) => NFData (KDEParams prob) where
    rnf kdeparams = deepseq (bandwidth kdeparams)
                  $ deepseq (kernel kdeparams)
                  $ seq (samplePoints kdeparams)
                  $ ()


-- | Generates a vector of the positions to sample our distribution at to generate the PDF.  It is intended for use with KDEParams only.
genSamplePoints :: 
    ( Fractional prob
    , VU.Unbox prob
    ) => Int -- minimum point
      -> Int -- maximum point
      -> Int -- number of samples
      -> VU.Vector prob
genSamplePoints min max samples = VU.fromList $ map (\i -> (fromIntegral min) + (fromIntegral i)*step) [0..samples]
    where
        step = (fromIntegral $ max-min)/(fromIntegral $ samples)

-- paramsFromData :: (Floating prob, Ord prob, Enum prob, VU.Unbox prob) => [prob] -> KDEParams prob
-- paramsFromData xs = KDEParams
--     { bandwidth = ((4*sigma^^5)/(3*n))**(1/5)
--     , samplePoints = VU.fromList samplePointsL
--     , kernel = KernelBox Epanechnikov
--     }
--     where
--         size = 100
--         margin = 10
--         step = ((maximum xs)-(minimum xs))/size
--         samplePointsL = map (\i -> (minimum xs) + i*step) [(-margin)..(size+margin)]
--         n = fromIntegral $ length xs
--         sigma = 1

-------------------------------------------------------------------------------
-- KDE

-- | This is an intermediate data structure used by the KDE data type.  You should NEVER use it directly because it doesn't have an empty element.  It is exported because some other modules in the HLearn library need direct access to it.
data KDE' prob = KDE'
    { params :: KDEParams prob
    , n :: prob
    , sampleVals :: VU.Vector prob
    }
    deriving (Show,Eq,Ord)

instance (NFData prob) => NFData (KDE' prob) where
    rnf kde = deepseq (params kde)
            $ deepseq (n kde)
            $ seq (sampleVals kde)
            $ ()

-- | The data structure that stores our Kernel Density Estimate.  Because KDE' doesn't have an empty element, we use the RegSG2Group free structure to provide one for us.  KDE inherits all the instances of KDE', plus Monoid and Group instances.
type KDE prob = RegSG2Group (KDE' prob)

-------------------------------------------------------------------------------
-- Algebra

instance (Eq prob, Num prob, VU.Unbox prob) => Semigroup (KDE' prob) where
    kde1 <> kde2 = if (params kde1) /= (params kde2)
        then error "KDE.(<>): different params"
        else kde1
            { n = (n kde1) + (n kde2)
            , sampleVals = VU.zipWith (+) (sampleVals kde1) (sampleVals kde2)
            }

instance (Eq prob, Num prob, VU.Unbox prob) => RegularSemigroup (KDE' prob) where
    inverse kde = kde
        { n = negate $ n kde
        , sampleVals = VU.map negate $ sampleVals kde
        }

instance (Num prob, VU.Unbox prob) => LeftOperator prob (KDE' prob) where
    p .* kde = kde
        { n = p * (n kde)
        , sampleVals = VU.map (*p) (sampleVals kde)
        }

instance (Num prob, VU.Unbox prob) => RightOperator prob (KDE' prob) where
    (*.) = flip (.*)
    
-------------------------------------------------------------------------------
-- Training
    
instance (Eq prob, Num prob, VU.Unbox prob) => Model (KDEParams prob) (KDE prob) where
    getparams (SGJust kde) = params kde

-- instance HomTrainer (KDEParams Double) Int (KDE Double) where
--     train1dp' params dp = train1dp' params (fromIntegral dp :: Double)

instance (Eq prob, Fractional prob, VU.Unbox prob) => HomTrainer (KDEParams prob) prob (KDE prob) where
    train1dp' params dp = SGJust $ KDE'
        { params = params
        , n = 1
        , sampleVals = VU.map (\x -> k ((x-dp)/(h x))) (samplePoints params)
        }
        where
            k u = (evalkernel (kernel params) u)/(h u)
            h   = calcBandwidth (bandwidth params)

-------------------------------------------------------------------------------
-- Distribution
    
instance (Ord prob, Fractional prob, VU.Unbox prob) => Distribution (KDE prob) prob prob where
    pdf (SGJust kde) dp 
        | dp <= (samplePoints $ params kde) VU.! 0 = 0 -- (sampleVals kde) VU.! 0
        | dp >= (samplePoints $ params kde) VU.! l = 0 -- (sampleVals kde) VU.! l
        | otherwise = (y2-y1)/(x2-x1)*(dp-x1)+y1
        where
            index = binsearch (samplePoints $ params kde) dp
            x1 = (samplePoints $ params kde) VU.! (index-1)
            x2 = (samplePoints $ params kde) VU.! (index)
            y1 = ((sampleVals kde) VU.! (index-1)) / (n kde)
            y2 = ((sampleVals kde) VU.! (index  )) / (n kde)
            l = (VU.length $ samplePoints $ params kde)-1

binsearch :: (Ord a, VU.Unbox a) => VU.Vector a -> a -> Int
binsearch vec dp = go 0 (VU.length vec-1)
    where 
        go low high
            | low==high = low
            | dp <= (vec VU.! mid) = go low mid
            | dp >  (vec VU.! mid) = go (mid+1) high
            where 
                mid = floor $ (fromIntegral $ low+high)/2

-------------------------------------------------------------------------------
-- Morphisms

instance Morphism (Categorical Int Double) (KDEParams Double) (KDE Double) where
    cat $> kdeparams = train' kdeparams $ CK.fmap (fromIntegral :: Int -> Double) (cat $> FreeModParams)

