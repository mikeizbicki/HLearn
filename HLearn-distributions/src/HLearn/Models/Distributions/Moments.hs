{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module HLearn.Models.Distributions.Moments
    ( MomentsParams (..)
    , Moments (..)
    )
    where
          
import GHC.TypeLits
import qualified Data.Vector.Unboxed as VU

import HLearn.Algebra
import HLearn.Models.Distributions.Common

-------------------------------------------------------------------------------
--

data MomentsParams = MomentsParams

newtype Moments prob (n::Nat) = Moments (VU.Vector prob)
    deriving (Read,Show)

-------------------------------------------------------------------------------
-- Algebra

instance (Num prob, VU.Unbox prob) => Semigroup (Moments prob n) where
    (Moments ma) <> (Moments mb) = Moments $ VU.zipWith (+) ma mb
    
instance (Num prob, VU.Unbox prob, SingI n) => Monoid (Moments prob n) where
    mappend = (<>)
    mempty = Moments $ VU.replicate (n+1) 0
        where n=fromIntegral $ fromSing (sing :: Sing n)
    
instance (Num prob, VU.Unbox prob) => RegularSemigroup (Moments prob n) where
    inverse (Moments m) = Moments $ VU.map negate m
    
-------------------------------------------------------------------------------
-- Training
    
instance Model MomentsParams (Moments prob n) where
    getparams _ = MomentsParams

instance DefaultModel MomentsParams (Moments prob n) where
    defparams = MomentsParams

instance (VU.Unbox prob, Fractional prob, SingI n) => HomTrainer MomentsParams prob (Moments prob n) where
    train1dp' _ dp = Moments $ VU.fromList [dp^^i | i <- [0..n]]
        where n=fromIntegral $ fromSing (sing :: Sing n)
              
              
-------------------------------------------------------------------------------
--

data BetaParams = BetaParams

data Beta prob = Beta
    { alpha :: prob
    , beta :: prob
    }
    deriving (Read,Show)

instance (VU.Unbox prob, Fractional prob) => Morphism (Moments prob 2) (BetaParams) (Beta prob) where
    (Moments v) `morph` BetaParams = Beta
        { alpha = alpha
        , beta  = beta
        }
        where
            alpha = (k-1)/(l*(1+k))
            beta  = k*alpha
            
            k = (mean-1)/mean
            l = ((k+1)^^2)*var
            
            mean = 1
            var = 1

-------------------------------------------------------------------------------
--


data NormalParams = NormalParams
              
data Normal prob = Normal
    { n :: prob
    , mean :: prob
    , stddev :: prob
    }
    deriving (Read,Show)
    
instance Semigroup (Normal prob)
instance Monoid (Normal prob)
    
instance Model NormalParams (Normal prob) where
    getparams normal = NormalParams
    
instance DefaultModel NormalParams (Normal prob) where
    defparams = NormalParams
    
instance (VU.Unbox prob, Fractional prob) => Morphism (Moments prob 2) NormalParams (Normal prob) where
-- instance Morphism (Moments prob 2) NormalParams (Normal prob) where
    (Moments v) `morph` NormalParams = Normal
        { n         = m0
        , mean      = m1 / m0
        , stddev    = (1/(m0-1))*m2-(m0/(m0-1))*(m1/m0)^^2
        }
        where
            m0 = v VU.! 0
            m1 = v VU.! 1
            m2 = v VU.! 2
            
instance (VU.Unbox prob, Fractional prob) => Morphism (Normal prob) MomentsParams (Moments prob 2) where
    (Normal n mean stddev) `morph` MomentsParams = Moments $ VU.fromList
        [ n
        , mean * n
        , (stddev+(n/(n-1))*(mean)^^2)*(n-1)
        ]

foo = ((train' MomentsParams [1,2,3::Double] :: Moments Double 2)
    $> NormalParams :: Normal Double)
    $> MomentsParams

{-foo' = train' 
    ( (MomentsParams :: MomentsParams)
    ) [1,2,3]
    $> NormalParams :: Normal Double-}
    
foo2 = (train' 
    ( ((NormalParams :: NormalParams)
    :. (MomentsParams :: MomentsParams))
        :: (MorphismComposition
                          [Double]
                          (MomentsParams)
                          (Moments Double 2)
                          (NormalParams)
                          (Normal Double))
    ) [1,2,3::Double]) :: Normal Double