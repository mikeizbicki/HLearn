{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module HLearn.Models.Distributions.Gaussian
    where

import Control.Monad
import Control.Monad.Random
import Control.DeepSeq
import Data.Binary
import Data.Hashable
import Data.Random.Normal
import Data.Semigroup
import Data.Number.LogFloat
import Debug.Trace
import Numeric.SpecFunctions (logFactorial)
import Test.QuickCheck

import Prelude hiding (log)

import HLearn.DataContainers
import HLearn.Math.Algebra
import HLearn.Math.TypeClasses
import HLearn.Models.Distributions.Common

-------------------------------------------------------------------------------
-- GaussianParams
          
data GaussianParams = GaussianParams

-------------------------------------------------------------------------------
-- Gaussian

data Gaussian datatype = Gaussian
        { m1 :: {-# UNPACK #-} !datatype
        , m2 :: {-# UNPACK #-} !datatype
        , n :: {-# UNPACK #-} !Int
        } 
    deriving (Show,Read)

instance (Binary datatype) => Binary (Gaussian datatype) where
    put (Gaussian m1 m2 n) = put m1 >> put m2 >> put n
    get = liftM3 Gaussian get get get
    
instance (NFData datatype) => NFData (Gaussian datatype) where
    rnf (Gaussian m1 m2 n) = seq (rnf m1) $ seq (rnf m2) (rnf n)

-------------------------------------------------------------------------------
-- Testing

instance (Fractional datatype, Ord datatype) => Eq (Gaussian datatype) where
    (==) (Gaussian m1a m2a na) (Gaussian m1b m2b nb) = 
        ((m1a==0 && m1b==0) || (abs $ m1a-m1b)/(m1a+m1b) < 1e-10) &&
        ((m2a==0 && m2b==0) || (abs $ m2a-m2b)/(m2a+m2b) < 1e-10) &&
        na==nb

instance (Random datatype, Fractional datatype, Arbitrary datatype) => Arbitrary (Gaussian datatype) where
    arbitrary = do
        m1 <- choose (-10,10)
        m2 <- choose (0.001,10)
        n <- choose (5,10)
        return $ Gaussian m1 m2 n

-------------------------------------------------------------------------------
-- Distribution

instance (Fractional datatype) => DistributionEstimator (Gaussian datatype) datatype where
    add1sample (Gaussian m1 m2 n) x = Gaussian m1' m2' n'
        where
            m1'=m1+(x-m1)/(fromIntegral n')
            m2'=m2+(x-m1)*(x-m1')
            n' =n+1

instance (Floating datatype, Ord datatype, Random datatype, Real datatype, Transfinite datatype) => 
    Distribution (Gaussian datatype) datatype 
        where            
    pdf g x = (logFloat $ 1/(sqrt $ 2*pi*v)) * (logToLogFloat $ -(x-m)^2/(2*v))
        where
            m = mean g
            v = varianceSample g
            
    cdf = error "Gaussian.cdf: undefined"
    cdfInverse dist prob = error "Gaussian.cdfInverse: undefined"
    
    drawSample g = do
        seed <- getRandom
        let (ret,_) = normal' (mean g,stddev g) (mkStdGen seed)
        return ret

--     mean :: (Floating  datatype) => (Gaussian datatype) -> datatype
    mean (Gaussian m1 m2 n) = {-float2Double-} m1

varianceSample :: (Floating  datatype, Ord datatype) => (Gaussian datatype) -> datatype
varianceSample (Gaussian m1 m2 n) = {-trace ("n="++show n) $-} {-float2Double $-} 
    if m2==0
       then abs $ (max m1 1)/(fromIntegral n)
       else m2/(fromIntegral $ n-1)

stddev :: (Floating  datatype, Ord datatype) => (Gaussian datatype) -> datatype
stddev = sqrt . varianceSample

-- instance (Label datatype) => IntersectableDistribution (Gaussian datatype) datatype where
--     intersectionScaled (wa,ga) (wb,gb) = 
-- --         trace ("a="++show a++", b="++show b++", c="++show c++", det="++show det) $ 
-- --         trace ("ma="++show ma++", va="++show va) $ 
-- --         trace ("mb="++show mb++", vb="++show vb) $ 
-- --         trace ("dbg="++show (a*q^2+b*q+c)) $ 
--         if det < 0
--            then []
--            else if a/=0
--                 then [(-b+(sqrt det))/(2*a),(-b-(sqrt det))/(2*a)]
--                 else if b/=0
--                     then [-c/b]
--                     else []
--         where
--             -- Quadratic equation variables
--             det = b^2 - 4*a*c
--             
--             a = (1/2)*(1/va - 1/vb)
--             b = (-ma/va + mb/vb)
--             c = (log $ (wb/wa)*(sqrt $ va/vb)) + (ma^2)/(2*va) - (mb^2)/(2*vb)
--             
--             -- Gaussian variables
--             ma = mean ga
--             mb = mean gb
--             va = varianceSample ga
--             vb = varianceSample gb


-------------------------------------------------------------------------------
-- Training
    
instance (Fractional datatype, Label datatype) => EmptyTrainer GaussianParams (Gaussian datatype) (Maybe datatype) where
    emptyModel desc params = mempty

instance (Fractional datatype, Label datatype) => OnlineTrainer GaussianParams (Gaussian datatype) () (Maybe datatype) where
--    add1dp :: DataDesc label -> modelparams -> model -> Labeled datatype label -> HLearn model
    add1dp desc modelparams model dp = case fst dp of
        Nothing -> return model
        Just x  -> return $ Gaussian m1' m2' n'
            where
                (Gaussian m1 m2 n) = model
                
                m1'=m1+(x-m1)/(fromIntegral n')
                m2'=m2+(x-m1)*(x-m1')
                n' =n+1
            
-------------------------------------------------------------------------------
-- Classification

instance (Random datatype, Floating datatype, Real datatype, Transfinite datatype) => Classifier (Gaussian datatype) (Maybe datatype) LogFloat where
    classify g Nothing  = 1
    classify g (Just x) = (logFloat $ 1/(sqrt $ 2*pi*v)) * (logToLogFloat $ -(x-m)^2/(2*v))
        where
            m = mean g
            v = varianceSample g

-------------------------------------------------------------------------------
-- Algebra

instance (Num datatype) => Invertible (Gaussian datatype) where
    inverse (Gaussian m1 m2 n) = Gaussian m1 (-m2) (-n)
--     inverse g = subG mempty g
--         where
--             subG (Gaussian m1' m2' n') (Gaussian m1a m2a na) = Gaussian m1b m2b nb
--                 where
--                     m1b=(m1'*(fromIntegral n'))/(fromIntegral nb) - (m1a*(fromIntegral na))/(fromIntegral nb)
--                     m2b=(-1)*(m2a-m2'+(fromIntegral $ na*nb)/(fromIntegral n')*(m1a-m1b)^2)
--                     nb=n'-na

instance (Fractional datatype) => Semigroup (Gaussian datatype) where
    (<>) = mappend

instance (Fractional datatype) => Monoid (Gaussian datatype) where
    mempty = Gaussian 0 0 0
    mappend g1@(Gaussian m1a m2a na) g2@(Gaussian m1b m2b nb) = 
        if n'==0
           then mempty
           else Gaussian m1' m2' n'
        where
            m1' = m1a*(fromIntegral na/fromIntegral n')+m1b*(fromIntegral nb/fromIntegral n')
            m2' = m2a+m2b+(fromIntegral $ na*nb)/(fromIntegral n')*(m1a-m1b)^2
            n'  = na+nb
                                                
