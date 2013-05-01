{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module HLearn.Models.Distributions.Multivariate.MultiNormal
    ( MultiNormal (..)
    )
    where

-- import qualified Control.ConstraintKinds as CK
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed.Deriving

import Control.DeepSeq
import GHC.ST
import GHC.TypeLits

import Foreign.Storable
import Numeric.LinearAlgebra hiding ((<>))
import qualified Numeric.LinearAlgebra as LA

import HLearn.Algebra
import HLearn.Models.Distributions.Common

-------------------------------------------------------------------------------
-- data types
    
data MultiNormalVec (n::Nat) prob = MultiNormalVec
    { q0 :: !prob
    , q1 :: !(VU.Vector prob)
    , q2 :: !(V.Vector (VU.Vector prob))
    }
    deriving (Read,Show,Eq,Ord)

newtype MultiNormal (xs::[*]) prob = MultiNormal (MultiNormalVec (Length xs) prob)
    deriving (Read,Show,Eq,Ord)

deriving instance (Monoid (MultiNormalVec (Length xs) prob)) => Monoid (MultiNormal xs prob)
deriving instance (Group (MultiNormalVec (Length xs) prob)) => Group (MultiNormal xs prob)

-------------------------------------------------------------------------------
-- algebra

instance (Num prob, VU.Unbox prob, SingI n) => Monoid (MultiNormalVec n prob) where
    mempty = MultiNormalVec
        { q0 = 0
        , q1 = VU.replicate n 0
        , q2 = V.replicate n (VU.replicate n 0)
        }
        where
            n = fromIntegral $ fromSing (sing :: Sing n)
    mn1 `mappend` mn2 = MultiNormalVec
        { q0 = (q0 mn1) + (q0 mn2)
        , q1 = VU.zipWith (+) (q1 mn1) (q1 mn2)
        , q2 = V.zipWith (VU.zipWith (+)) (q2 mn1) (q2 mn2)
        }

instance (Num prob) => HasRing (MultiNormalVec n prob) where
    type Ring (MultiNormalVec n prob) = prob
    
---------------------------------------

instance (Num prob) => HasRing (MultiNormal xs prob) where
    type Ring (MultiNormal xs prob) = prob
    
-------------------------------------------------------------------------------
-- training

instance (SingI n, Num prob, VU.Unbox prob) => HomTrainer (MultiNormalVec n prob) where
    type Datapoint (MultiNormalVec n prob) = (VU.Vector prob) 
    train1dp dp = MultiNormalVec
        { q0 = 1
        , q1 = dp
        , q2 = V.generate n (\i -> VU.generate n (\j -> (dp VU.! i)*(dp VU.! j)))
        }
        where
            n = fromIntegral $ fromSing (sing :: Sing n)

instance 
    ( SingI (Length xs)
    , Num prob
    , VU.Unbox prob
    , HList2List (Datapoint (MultiNormal xs prob)) prob
    ) => HomTrainer (MultiNormal xs prob) 
        where
    type Datapoint (MultiNormal xs prob) = HList xs
    train1dp dp = MultiNormal $ train1dp $ VU.fromList $ hlist2list dp

instance (Num prob) => NumDP (MultiNormal xs prob) where
    numdp (MultiNormal mn) = q0 mn

-------------------------------------------------------------------------------
-- distribution

class (Probabilistic dist) => Covariance dist where
    covar :: dist -> Matrix (Probability dist)

instance 
    ( VU.Unbox prob
    , SingI k
    , Num prob
    ) => Probabilistic (MultiNormalVec k prob) 
        where
    type Probability (MultiNormalVec k prob) = prob

instance 
    ( VU.Unbox prob
    , SingI k
    , Fractional prob
    , Enum prob
    , Storable prob
    ) => Covariance (MultiNormalVec k prob) 
        where
    covar mn = (k><k) $ 
            [ (1/(n-1))*( mij - ((q1 mn VU.! j)*((q1 mn) VU.! i))/n ) 
            | ((i,j),mij) <- assocs
            ]
        where
            assocs = zip [(i,j) | i<-[0..k-1],j<-[0..k-1]] (concat $ V.toList $ fmap VU.toList $ q2 mn)
            mean i = ((q1 mn) VU.! i)/n
            n = q0 mn
            k = fromIntegral $ fromSing (sing :: Sing k)

instance 
    ( HList2List (HList dpL) prob
    , VU.Unbox prob
    , Num prob
    , SingI (FromNat1 (Length1 dpL))
    ) => Probabilistic (MultiNormal dpL prob) 
        where
    type Probability (MultiNormal dpL prob) = prob

instance
    ( HList2List (HList dpL) prob
    , VU.Unbox prob
    , Floating prob
    , Field prob
    , Enum prob
    , SingI (FromNat1 (Length1 dpL))
--     , Covariance (MultiNormal dpL prob)
    , Storable prob
    ) => PDF (MultiNormal dpL prob) 
        where
    pdf (MultiNormal dist) dpL = 1/(sqrt $ (2*pi)^(k)*(det sigma))*(exp $ (-1/2)*(top) )
        where
            top=minElement $ ((trans $ x `sub` mu) LA.<> (inv sigma) LA.<> (x `sub` mu))
              
            k = fromIntegral $ fromSing (sing :: Sing (Length dpL)) :: Int
            x = k><1 $ hlist2list dpL
            n = q0 dist
            sigma = covar dist
            mu = k><1 $ map (/n) $ VU.toList (q1 $ dist)
--             covarM = (k><k) $ elems $ covar dist

-------------------------------------------------------------------------------
-- tests

-- ds = map (listArray (0,2)) 
--     [[1,2,4]
--     ,[2,5,6]
--     ,[3,1,1]
--     ]

-- test = train ds :: MultiNormalArray Double 3

ds2 = map VU.fromList
    [[1,2,4]
    ,[2,5,6]
    ,[3,1,1]
    ]

test2 = train ds2 :: MultiNormalVec 3 Double

ds = 
    [ 1:::2:::3:::HNil
    , 2:::5:::6:::HNil
    , 3:::1:::1:::HNil
    , 3:::2:::1:::HNil
    ]
test = train ds :: MultiNormal '[Double,Double,Double] Double
        