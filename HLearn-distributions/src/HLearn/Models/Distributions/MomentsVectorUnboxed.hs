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

-- | The method of moments can be used to estimate a number of commonly used distributions.  This module is still under construction as I work out the best way to handle morphisms from the Moments type to types of other distributions.  For more information, see the wikipedia entry: <https://en.wikipedia.org/wiki/Method_of_moments_(statistics)>

module HLearn.Models.Distributions.Moments
    ( MomentsParams (..)
    , Moments
    , Normal
    )
    where
          
import GHC.TypeLits
import qualified Data.Vector.Unboxed as VU

import HLearn.Algebra
import HLearn.Models.Distributions.Common

-------------------------------------------------------------------------------
-- Moments

data MomentsParams prob (n::Nat) = MomentsParams
    deriving (Read,Show,Eq,Ord)

data Moments prob (n::Nat) = Moments !(VU.Vector prob)
    deriving (Read,Show)

instance (Eq prob, VU.Unbox prob) => Eq (Moments prob n) where
    (Moments v1) == (Moments v2) = v1 == v2

instance (Ord prob, VU.Unbox prob) => Ord (Moments prob n) where
    compare (Moments v1) (Moments v2) = compare v1 v2

-------------------------------------------------------------------------------
-- Algebra


instance (Num prob, VU.Unbox prob) => Abelian (Moments prob n)
instance (Num prob, VU.Unbox prob) => Semigroup (Moments prob n) where
    (<>) !(Moments ma) !(Moments mb) = Moments $ VU.zipWith (+) ma mb
    
instance (Num prob, VU.Unbox prob, SingI n) => Monoid (Moments prob n) where
    mappend = (<>)
    mempty = Moments $ VU.replicate (n+1) 0
        where n=fromIntegral $ fromSing (sing :: Sing n)
    
instance (Num prob, VU.Unbox prob) => RegularSemigroup (Moments prob n) where
    inverse !(Moments m) = Moments $ VU.map negate m

instance (Fractional prob, VU.Unbox prob, SingI n) => LeftModule prob (Moments prob n)
instance (Fractional prob, VU.Unbox prob) => LeftOperator prob (Moments prob n) where
    (.*) !p !(Moments vec) = Moments $ VU.map (*p) vec

instance (Fractional prob, VU.Unbox prob, SingI n) => RightModule prob (Moments prob n)
instance (Fractional prob, VU.Unbox prob) => RightOperator prob (Moments prob n) where
    (*.) = flip (.*)
    
-------------------------------------------------------------------------------
-- Training
    
instance Model (MomentsParams prob n) (Moments prob n) where
    getparams _ = MomentsParams

instance DefaultModel (MomentsParams prob n) (Moments prob n) where
    defparams = MomentsParams

instance (VU.Unbox prob, Fractional prob, SingI n) => HomTrainer (MomentsParams prob n) prob (Moments prob n) where
    train1dp' _ dp = Moments $ VU.constructN n (\v -> let len = VU.length v in
                if len == 0
                    then 1
                    else dp*(v VU.! (len-1)))
        where 
            n = fromIntegral $ fromSing (sing :: Sing n)
              
-------------------------------------------------------------------------------
--

data MomentsConverterParams params = MomentsConverterParams params

data MomentsConverter prob (n::Nat) dist = MomentsConverter
    { moments       :: !(Moments prob n)
    , dist          :: dist
    }
    deriving (Read,Show)

instance 
    ( DefaultMorphism (Moments prob n) params dist
    , Num prob, VU.Unbox prob
    ) => Semigroup (MomentsConverter prob n dist) where
    mc1 <> mc2 = MomentsConverter m (morph m)
        where
            m = (moments mc1) <> (moments mc2)

-------------------------------------------------------------------------------
--

data BetaParams = BetaParams

data Beta prob = Beta
    { alpha :: prob
    , beta :: prob
    }
    deriving (Read,Show)

instance (VU.Unbox prob, Fractional prob) => Morphism (Moments prob 2) (BetaParams) (Beta prob) where
    (Moments v) $> BetaParams = Beta
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

type Normal prob = Moments prob 2

instance (Floating prob, VU.Unbox prob) => Distribution (Moments prob 2) prob prob where
    pdf (Moments v) x = 1/(sigma*(sqrt $ 2*pi))*(exp $ -(x-mu)*(x-mu)/(2*sigma*sigma))
        where
            m0 = v VU.! 0
            m1 = v VU.! 1
            m2 = v VU.! 2
            mu = m1 / m0
            sigma = (1/(m0-1))*m2-(m0/(m0-1))*(m1/m0)^^2

data NormalParams = NormalParams
              
-- data Normal prob = Normal
--     { n :: prob
--     , mean :: prob
--     , stddev :: prob
--     }
--     deriving (Read,Show)
--     
-- {-instance Semigroup (Normal prob)
-- instance Monoid (Normal prob)
--     
-- instance Model NormalParams (Normal prob) where
--     getparams normal = NormalParams
--     
-- instance DefaultModel NormalParams (Normal prob) where
--     defparams = NormalParams-}
--     
-- instance (VU.Unbox prob, Fractional prob) => Morphism (Moments prob 2) NormalParams (Normal prob) where
-- -- instance Morphism (Moments prob 2) NormalParams (Normal prob) where
--     (Moments v) $> NormalParams = Normal
--         { n         = m0
--         , mean      = m1 / m0
--         , stddev    = (1/(m0-1))*m2-(m0/(m0-1))*(m1/m0)^^2
--         }
--         where
--             m0 = v VU.! 0
--             m1 = v VU.! 1
--             m2 = v VU.! 2
--             
-- instance (VU.Unbox prob, Fractional prob) => Morphism (Normal prob) MomentsParams (Moments prob 2) where
--     (Normal n mean stddev) $> MomentsParams = Moments $ VU.fromList
--         [ n
--         , mean * n
--         , (stddev+(n/(n-1))*(mean)^^2)*(n-1)
--         ]
-- 
-- foo = ((train' MomentsParams [1,2,3::Double] :: Moments Double 2)
--     $> NormalParams :: Normal Double)

-- instance Distribution (Moments Double 2) Double Double where
--     pdf dist dp = 

{-foo' = train' 
    ( (MomentsParams :: MomentsParams)
    ) [1,2,3]
    $> NormalParams :: Normal Double-}
    
-- foo2 = (train' 
--     ( ((NormalParams :: NormalParams)
--     :. (MomentsParams :: MomentsParams))
--         :: (MorphismComposition
--                           [Double]
--                           (MomentsParams)
--                           (Moments Double 2)
--                           (NormalParams)
--                           (Normal Double))
--     ) [1,2,3::Double]) :: Normal Double