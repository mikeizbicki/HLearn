{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module HLearn.Models.Distributions.Gaussian
    where

import Control.Monad
import Control.Monad.Random
import Data.Random.Normal
import Debug.Trace
import Numeric.SpecFunctions (logFactorial)
import Test.QuickCheck

import qualified Statistics.Distribution as D
import qualified Statistics.Distribution.Normal as N

import HLearn.Algebra
import HLearn.Models.Distributions.Common


import Data.Vector.Generic.Base
import Data.Vector.Generic.Mutable
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Deriving
   
   -- newtype Score = Score Double deriving (Vector U.Vector, MVector U.MVector, U.Unbox)

-------------------------------------------------------------------------------
-- GaussianParams
          
data GaussianParams = GaussianParams

instance ModelParams GaussianParams

instance NFData GaussianParams where
    rnf params = ()

-------------------------------------------------------------------------------
-- Gaussian

data Gaussian datapoint = Gaussian
--         { m1 :: {-# UNPACK #-} !Double
--         , m2 :: {-# UNPACK #-} !Double
--         , n :: {-# UNPACK #-} !Int
--         } 
        { m1 :: !datapoint
        , m2 :: !datapoint
        , n :: {-# UNPACK #-} !Int
        } 
    deriving (Show,Read)

derivingUnbox "Gaussian"
    [d| instance (U.Unbox a) => Unbox' (Gaussian a) (a, a, Int) |]
    [| \ (Gaussian m1 m2 n) -> (m1,m2,n) |]
    [| \ (m1,m2,n) -> (Gaussian m1 m2 n) |]

instance (NFData datapoint) => Model GaussianParams (Gaussian datapoint) where
    params model = GaussianParams

instance (Binary datapoint) => Binary (Gaussian datapoint) where
    put (Gaussian m1 m2 n) = put m1 >> put m2 >> put n
    get = liftM3 Gaussian get get get
    
instance (NFData datapoint) => NFData (Gaussian datapoint) where
    rnf (Gaussian m1 m2 n) = seq (rnf m1) $ seq (rnf m2) (rnf n)

-------------------------------------------------------------------------------
-- Testing

instance (Fractional datapoint, Ord datapoint) => Eq (Gaussian datapoint) where
    (==) (Gaussian m1a m2a na) (Gaussian m1b m2b nb) = 
        ((m1a==0 && m1b==0) || (abs $ m1a-m1b)/(m1a+m1b) < 1e-10) &&
        ((m2a==0 && m2b==0) || (abs $ m2a-m2b)/(m2a+m2b) < 1e-10) &&
        na==nb

instance (Random datapoint, Fractional datapoint, Arbitrary datapoint) => Arbitrary (Gaussian datapoint) where
    arbitrary = do
        m1 <- choose (-10,10)
        m2 <- choose (0.001,10)
        n <- choose (5,10)
        return $ Gaussian m1 m2 n

-------------------------------------------------------------------------------
-- Distribution

convdistr :: Gaussian Double -> N.NormalDistribution
convdistr g = N.normalDistr (mean g) (stddev g)

instance Distribution (Gaussian Double) Double Double where
    pdf g x = D.density (convdistr g) x
    cdf g x = D.cumulative (convdistr g) x
    cdfInverse g x = D.quantile (convdistr g) x
    mean g = m1 g
    drawSample g = do
        seed <- getRandom
        let (ret,_) = normal' (mean g,stddev g) (mkStdGen seed)
        return ret

varianceSample :: (Floating  datapoint, Ord datapoint) => (Gaussian datapoint) -> datapoint
varianceSample (Gaussian m1 m2 n) = {-trace ("n="++show n) $-} {-float2Double $-} 
    if m2==0
       then abs $ (max m1 1)/(fromIntegral n)
       else m2/(fromIntegral $ n-1)

stddev :: (Floating  datapoint, Ord datapoint) => (Gaussian datapoint) -> datapoint
stddev = sqrt . varianceSample


-------------------------------------------------------------------------------
-- Training
    
-- instance (Fractional datapoint, Label datapoint) => HasIdentity GaussianParams (Gaussian datapoint) (Maybe datapoint) where
--     emptyModel desc params = mempty
-- 
-- instance (Fractional datapoint, Label datapoint) => OnlineTrainer GaussianParams (Gaussian datapoint) () (Maybe datapoint) where
-- --    add1dp :: DataDesc label -> modelparams -> model -> Labeled datapoint label -> HLearn model
--     add1dp desc modelparams model dp = case fst dp of
--         Nothing -> return model
--         Just x  -> return $ Gaussian m1' m2' n'
--             where
--                 (Gaussian m1 m2 n) = model
--                 
--                 m1'=m1+(x-m1)/(fromIntegral n')
--                 m2'=m2+(x-m1)*(x-m1')
--                 n' =n+1
            
-------------------------------------------------------------------------------
-- Classification

-- instance (Random datapoint, Floating datapoint, Real datapoint, Transfinite datapoint) => Classifier (Gaussian datapoint) (Maybe datapoint) LogFloat where
--     classify g Nothing  = 1
--     classify g (Just x) = (logFloat $ 1/(sqrt $ 2*pi*v)) * (logToLogFloat $ -(x-m)^2/(2*v))
--         where
--             m = mean g
--             v = varianceSample g

-------------------------------------------------------------------------------
-- Algebra

instance (Fractional datapoint) => SingletonTrainer GaussianParams datapoint (Gaussian datapoint) where
    {-# INLINE train #-}
    train GaussianParams x = Gaussian x 0 1

instance (Num datapoint) => Invertible (Gaussian datapoint) where
    {-# INLINE inverse #-}
    inverse (Gaussian m1 m2 n) = Gaussian m1 (-m2) (-n)

instance (Fractional datapoint) => Semigroup (Gaussian datapoint) where
    {-# INLINE (<>) #-}
    (<>) g1@(Gaussian m1a m2a na) g2@(Gaussian m1b m2b nb) = 
        if n'==0
           then Gaussian 0 0 0
           else Gaussian m1' m2' n'
        where
            m1' = m1a*(fromIntegral na/fromIntegral n')+m1b*(fromIntegral nb/fromIntegral n')
            m2' = m2a+m2b+(fromIntegral $ na*nb)/(fromIntegral n')*(m1a-m1b)^2
            n'  = na+nb

instance (Fractional datapoint) => HasIdentity (Gaussian datapoint) where
    {-# INLINE identity #-}
    identity = Gaussian 0 0 0

instance (Fractional datapoint) => Monoid (Gaussian datapoint) where
    {-# INLINE mempty #-}
    mempty = identity
    {-# INLINE mappend #-}
    mappend = (<>)
