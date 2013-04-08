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

module HLearn.Models.Distributions.MultiNormal
    where

-- import qualified Control.ConstraintKinds as CK
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed.Deriving

import Control.DeepSeq
import Data.Array.Unboxed
import Data.Array.ST
import GHC.ST
import GHC.TypeLits

import HLearn.Algebra

-------------------------------------------------------------------------------
-- data types
    
data MultiNormalVec (n::Nat) prob = MultiNormalVec
    { q0 :: !prob
    , q1 :: !(VU.Vector prob)
    , q2 :: !(V.Vector (VU.Vector prob))
    }
    deriving (Read,Show,Eq,Ord)

newtype MultiNormal (n::Nat) prob = MultiNormal (MultiNormalVec n prob)
    deriving (Read,Show,Eq,Ord,Semigroup,Monoid{-,RegularSemigroup-})

data MultiNormalArray prob (n::Nat) = MultiNormalArray
    { m0 :: !prob
    , m1 :: !(UArray Int prob)
    , m2 :: !(UArray (Int,Int) prob)
    }
--     deriving (Show,Eq,Ord)

instance (Show prob, IArray UArray prob) => Show (MultiNormalArray prob (n::Nat)) where
    show mn = "MultiNormalArray { m0="++(show $ m0 mn)++", m1="++(show $ m1 mn)++", m2="++(show $ m2 mn)++" }"

instance NFData (MultiNormalArray prob n) where
    rnf mn = seq m0 $ seq m1 $ seq m2 $ ()

-------------------------------------------------------------------------------
-- algebra

instance (Num prob, VU.Unbox prob) => Semigroup (MultiNormalVec n prob) where
    mn1 <> mn2 = MultiNormalVec
        { q0 = (q0 mn1) + (q0 mn2)
        , q1 = VU.zipWith (+) (q1 mn1) (q1 mn2)
        , q2 = V.zipWith (VU.zipWith (+)) (q2 mn1) (q2 mn2)
        }
        
instance (Num prob, VU.Unbox prob, SingI n) => Monoid (MultiNormalVec n prob) where
    mappend = (<>)
    mempty = MultiNormalVec
        { q0 = 0
        , q1 = VU.replicate n 0
        , q2 = V.replicate n (VU.replicate n 0)
        }
        where
            n = fromIntegral $ fromSing (sing :: Sing n)

instance (Num prob, SingI n, IArray UArray prob) => Semigroup (MultiNormalArray prob n) where
    mn1 <> mn2 = MultiNormalArray
        { m0 = (m0 mn1) + (m0 mn2)
        , m1 = listArray (0,k-1) $ zipWith (+) (elems $ m1 mn1) (elems $ m1 mn2)
        , m2 = listArray ((0,0),(k-1,k-1)) $ zipWith (+) (elems $ m2 mn1) (elems $ m2 mn2)
        }
        where
            k = fromIntegral $ fromSing (sing :: Sing n)
    
instance (Num prob, SingI n, IArray UArray prob) => Monoid (MultiNormalArray prob n) where
    mempty = MultiNormalArray
        { m0 = 0
        , m1 = listArray (0,k-1) $ repeat 0
        , m2 = listArray ((0,0),(k-1,k-1)) $ repeat 0
        }
        where
            k = fromIntegral $ fromSing (sing :: Sing n)
    mappend = (<>)

-------------------------------------------------------------------------------
-- training

instance ModelParams (MultiNormalVec n prob) where
    type Params (MultiNormalVec n prob) = NoParams
    getparams _ = NoParams

instance (SingI n, Num prob, VU.Unbox prob) => HomTrainer (MultiNormalVec n prob) where
    type Datapoint (MultiNormalVec n prob) = (VU.Vector prob) 
    train1dp' _ dp = MultiNormalVec
        { q0 = 1
        , q1 = dp
        , q2 = V.generate n (\i -> VU.generate n (\j -> (dp VU.! i)*(dp VU.! j)))
        }
        where
            n = fromIntegral $ fromSing (sing :: Sing n)

instance ModelParams (MultiNormal n prob) where
    type Params (MultiNormal n prob) = NoParams
    getparams _ = NoParams
    
class HList2List xs a | xs -> a where
    hlist2list :: xs -> [a]
instance HList2List (HList '[]) a where
    hlist2list xs = []
instance (HList2List (HList xs) a) => HList2List (HList (a ':xs)) a where
    hlist2list (x:::xs) = x:(hlist2list xs)
    
instance 
    ( SingI n
    , Num prob
    , VU.Unbox prob
    , HList2List (Datapoint (MultiNormal n prob)) prob
    ) => HomTrainer (MultiNormal n prob) 
        where
    type Datapoint (MultiNormal n prob) = HList (Replicate prob n)
    train1dp' _ dp = MultiNormal $ train1dp $ VU.fromList $ hlist2list dp

instance ModelParams (MultiNormalArray prob n) where
    type Params (MultiNormalArray prob n) = NoParams
    getparams _ = NoParams

instance 
    ( Num prob
    , SingI n
    , IArray UArray prob
--     , MArray (STUArray s) prob (ST s)
    ) => HomTrainer (MultiNormalArray prob n) 
        where
    type Datapoint (MultiNormalArray prob n) = (UArray Int prob) 
              
    train1dp' _ dp = MultiNormalArray
        { m0 = 1
        , m1 = dp
        , m2 = listArray ((0,0),(k-1,k-1)) [(dp ! i)*(dp ! j) | j<-[0..k-1], i<-[0..k-1]]
        }
        where
            k = fromIntegral $ fromSing (sing :: Sing n)

-- trainST' _ dps = MultiNormal
--     { m0 = F.foldl (+) 0 $ fmap (\x->1) dps
--     , m1 = runSTUArray $ do
--         arr <- newArray (0,k-1) 0
--         return arr -- (arr::STUArray s Int prob)
-- --             listArray (0,k-1) $ repeat 0
--     , m2 = listArray ((0,0),(k-1,k-1)) $ repeat 0
--     }
--     where
--         k = 1--fromIntegral $ fromSing (sing :: Sing n)

-------------------------------------------------------------------------------
-- distribution

class Covariance dist prob | dist -> prob where
    covar :: dist -> UArray (Int,Int) prob

instance (Show prob, Fractional prob, SingI k, IArray UArray prob) => Covariance (MultiNormalArray prob k) prob where
    covar mn = listArray ((0,0),(k-1,k-1)) $ 
            [ (1/(n-1))*( mij - ((m1 mn ! j)*(m1 mn ! i))/n ) 
            | ((i,j),mij) <- assocs (m2 mn) 
            ]
        where
            mean i = ((m1 mn) ! i)/n
            n = m0 mn
            k = fromIntegral $ fromSing (sing :: Sing k)

-------------------------------------------------------------------------------
-- tests

ds = map (listArray (0,2)) 
    [[1,2,4]
    ,[2,5,6]
    ,[3,1,1]
    ]

test = train ds :: MultiNormalArray Double 3

ds2 = map VU.fromList
    [[1,2,4]
    ,[2,5,6]
    ,[3,1,1]
    ]

test2 = train ds2 :: MultiNormalVec 3 Double