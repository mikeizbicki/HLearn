{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module HLearn.Models.Distributions.Gaussian
    where

import Control.Monad.Random
import Data.Random.Normal
import Debug.Trace
import Numeric.SpecFunctions (logFactorial)
import Test.QuickCheck

import qualified Statistics.Distribution as D
import qualified Statistics.Distribution.Normal as N

import HLearn.Algebra
import HLearn.Models.Distributions.Common

-------------------------------------------------------------------------------
-- GaussianParams
          
data GaussianParams = GaussianParams

instance ModelParams GaussianParams

instance NFData GaussianParams where
    rnf params = ()

-------------------------------------------------------------------------------
-- Gaussian

data Gaussian datatype = Gaussian
--         { m1 :: {-# UNPACK #-} !datatype
--         , m2 :: {-# UNPACK #-} !datatype
        { m1 :: !datatype
        , m2 :: !datatype
        , n :: {-# UNPACK #-} !Int
        } 
    deriving (Show,Read)

instance (NFData datatype) => Model GaussianParams (Gaussian datatype) where
    params model = GaussianParams

{-instance (Binary datatype) => Binary (Gaussian datatype) where
    put (Gaussian m1 m2 n) = put m1 >> put m2 >> put n
    get = liftM3 Gaussian get get get-}
    
instance (NFData datatype) => NFData (Gaussian datatype) where
    rnf (Gaussian m1 m2 n) = seq (rnf m1) $ seq (rnf m2) (rnf n)

instance (Fractional datatype) => Trainer (Gaussian datatype) datatype (Gaussian datatype) where
    train (Gaussian m1 m2 n) x = Gaussian m1' m2' n'
        where
            m1'=m1+(x-m1)/(fromIntegral n')
            m2'=m2+(x-m1)*(x-m1')
            n' =n+1

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

varianceSample :: (Floating  datatype, Ord datatype) => (Gaussian datatype) -> datatype
varianceSample (Gaussian m1 m2 n) = {-trace ("n="++show n) $-} {-float2Double $-} 
    if m2==0
       then abs $ (max m1 1)/(fromIntegral n)
       else m2/(fromIntegral $ n-1)

stddev :: (Floating  datatype, Ord datatype) => (Gaussian datatype) -> datatype
stddev = sqrt . varianceSample


-------------------------------------------------------------------------------
-- Training
    
-- instance (Fractional datatype, Label datatype) => HasIdentity GaussianParams (Gaussian datatype) (Maybe datatype) where
--     emptyModel desc params = mempty
-- 
-- instance (Fractional datatype, Label datatype) => OnlineTrainer GaussianParams (Gaussian datatype) () (Maybe datatype) where
-- --    add1dp :: DataDesc label -> modelparams -> model -> Labeled datatype label -> HLearn model
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

-- instance (Random datatype, Floating datatype, Real datatype, Transfinite datatype) => Classifier (Gaussian datatype) (Maybe datatype) LogFloat where
--     classify g Nothing  = 1
--     classify g (Just x) = (logFloat $ 1/(sqrt $ 2*pi*v)) * (logToLogFloat $ -(x-m)^2/(2*v))
--         where
--             m = mean g
--             v = varianceSample g

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
    (<>) g1@(Gaussian m1a m2a na) g2@(Gaussian m1b m2b nb) = 
        if n'==0
           then Gaussian 0 0 0
           else Gaussian m1' m2' n'
        where
            m1' = m1a*(fromIntegral na/fromIntegral n')+m1b*(fromIntegral nb/fromIntegral n')
            m2' = m2a+m2b+(fromIntegral $ na*nb)/(fromIntegral n')*(m1a-m1b)^2
            n'  = na+nb

instance (Fractional datatype) => HasIdentity (Gaussian datatype) where
    identity = Gaussian 0 0 0

instance (Fractional datatype) => Monoid (Gaussian datatype) where
    mempty = identity
    mappend = (<>)

-- instance (Fractional datatype) => Semigroup (Gaussian datatype) where
--     (<>) = mappend
-- 
-- instance (Fractional datatype) => Monoid (Gaussian datatype) where
--     mempty = Gaussian 0 0 0
--     mappend g1@(Gaussian m1a m2a na) g2@(Gaussian m1b m2b nb) = 
--         if n'==0
--            then mempty
--            else Gaussian m1' m2' n'
--         where
--             m1' = m1a*(fromIntegral na/fromIntegral n')+m1b*(fromIntegral nb/fromIntegral n')
--             m2' = m2a+m2b+(fromIntegral $ na*nb)/(fromIntegral n')*(m1a-m1b)^2
--             n'  = na+nb
--                                                 
