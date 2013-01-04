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

module HLearn.Models.Distributions.KernelDensityEstimator
    ( KDEParams (..)
    , KDEBandwidth (..)
    , KDE (..)
    , KDE' (..)
    , genSamplePoints
    
    , Uniform (..)
    , Triangular (..)
    , Epanechnikov (..)
    , Quartic (..)
    , Triweight (..)
    , Tricube (..)
    , Gaussian (..)
    , Cosine (..)
    , KernelBox (..)
    )
    where
          
import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Categorical

import qualified Control.ConstraintKinds as CK
import Control.DeepSeq
import qualified Data.Vector.Unboxed as VU

-------------------------------------------------------------------------------
--

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

genSamplePoints :: (Fractional prob, VU.Unbox prob) => Int -> Int -> Int -> VU.Vector prob
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

instance HomTrainer (KDEParams Double) Int (KDE Double) where
    train1dp' params dp = train1dp' params (fromIntegral dp :: Double)

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

-------------------------------------------------------------------------------
-- Kernels

-- | This list of kernels is take from wikipedia's: https://en.wikipedia.org/wiki/Uniform_kernel#Kernel_functions_in_common_use
class Kernel kernel num where
    evalkernel :: kernel -> num -> num

data KernelBox num where KernelBox :: (Kernel kernel num, Show kernel) => kernel -> KernelBox num

instance Kernel (KernelBox num) num where
    evalkernel (KernelBox k) p = evalkernel k p
instance Show (KernelBox num) where
    show (KernelBox k) = "KB "++show k
instance Eq (KernelBox num) where
    KernelBox k1 == KernelBox k2 = (show k1) == (show k2)
instance Ord (KernelBox num) where
    _ `compare` _ = EQ
instance NFData (KernelBox num) where
    rnf (KernelBox num)= seq num ()
    
data Uniform = Uniform deriving (Read,Show)
instance (Fractional num, Ord num) => Kernel Uniform num where
    evalkernel Uniform u = if abs u < 1
        then 1/2
        else 0

data Triangular = Triangular deriving (Read,Show)
instance (Fractional num, Ord num) => Kernel Triangular num where
    evalkernel Triangular u = if abs u<1
        then 1-abs u
        else 0
        
data Epanechnikov = Epanechnikov deriving (Read,Show)
instance (Fractional num, Ord num) => Kernel Epanechnikov num where
    evalkernel Epanechnikov u = if abs u<1
        then (3/4)*(1-u^^2)
        else 0

data Quartic = Quartic deriving (Read,Show)
instance (Fractional num, Ord num) => Kernel Quartic num where
    evalkernel Quartic u = if abs u<1
        then (15/16)*(1-u^^2)^^2
        else 0
        
data Triweight = Triweight deriving (Read,Show)
instance (Fractional num, Ord num) => Kernel Triweight num where
    evalkernel Triweight u = if abs u<1
        then (35/32)*(1-u^^2)^^3
        else 0

data Tricube = Tricube deriving (Read,Show)
instance (Fractional num, Ord num) => Kernel Tricube num where
    evalkernel Tricube u = if abs u<1
        then (70/81)*(1-u^^3)^^3
        else 0
        
data Cosine = Cosine deriving (Read,Show)
instance (Floating num, Ord num) => Kernel Cosine num where
    evalkernel Cosine u = if abs u<1
        then (pi/4)*(cos $ (pi/2)*u)
        else 0
        
data Gaussian = Gaussian deriving (Read,Show)
instance (Floating num, Ord num) => Kernel Gaussian num where
    evalkernel Gaussian u = (1/(2*pi))*(exp $ (-1/2)*u^^2)
