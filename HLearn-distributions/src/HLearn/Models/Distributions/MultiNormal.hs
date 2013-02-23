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

module HLearn.Models.Distributions.MultiNormal
    where

import Control.DeepSeq
import Data.Array.Unboxed
import GHC.TypeLits

import HLearn.Algebra

-------------------------------------------------------------------------------
-- data types

data MultiNormal prob (n::Nat) = MultiNormal
    { m0 :: !prob
    , m1 :: !(UArray Int prob)
    , m2 :: !(UArray (Int,Int) prob)
    }
--     deriving (Show,Eq,Ord)

instance (Show prob, IArray UArray prob) => Show (MultiNormal prob (n::Nat)) where
    show mn = "MultiNormal { m0="++(show $ m0 mn)++", m1="++(show $ m1 mn)++", m2="++(show $ m2 mn)++" }"

instance NFData (MultiNormal prob n) where
    rnf mn = seq m0 $ seq m1 $ seq m2 $ ()

-------------------------------------------------------------------------------
-- algebra

instance (Num prob, SingI n, IArray UArray prob) => Semigroup (MultiNormal prob n) where
    mn1 <> mn2 = MultiNormal
        { m0 = (m0 mn1) + (m0 mn2)
        , m1 = listArray (0,k-1) $ zipWith (+) (elems $ m1 mn1) (elems $ m1 mn2)
        , m2 = listArray ((0,0),(k-1,k-1)) $ zipWith (+) (elems $ m2 mn1) (elems $ m2 mn2)
        }
        where
            k = fromIntegral $ fromSing (sing :: Sing n)
    
instance (Num prob, SingI n, IArray UArray prob) => Monoid (MultiNormal prob n) where
    mempty = MultiNormal
        { m0 = 0
        , m1 = listArray (0,k-1) $ repeat 0
        , m2 = listArray ((0,0),(k-1,k-1)) $ repeat 0
        }
        where
            k = fromIntegral $ fromSing (sing :: Sing n)
    mappend = (<>)

-------------------------------------------------------------------------------
-- training

instance Model (NoParams (MultiNormal prob n)) (MultiNormal prob n) where
    getparams _ = NoParams
    
instance DefaultModel  (NoParams (MultiNormal prob n)) (MultiNormal prob n) where
    defparams = NoParams

instance (Num prob, SingI n, IArray UArray prob) => HomTrainer (NoParams (MultiNormal prob n)) (UArray Int prob) (MultiNormal prob n) where
    train1dp' _ dp = MultiNormal
        { m0 = 1
        , m1 = dp
        , m2 = listArray ((0,0),(k-1,k-1)) [(dp ! i)*(dp ! j) | j<-[0..k-1], i<-[0..k-1]]
        }
        where
            k = fromIntegral $ fromSing (sing :: Sing n)

-------------------------------------------------------------------------------
-- distribution

class Covariance dist prob | dist -> prob where
    covar :: dist -> UArray (Int,Int) prob

instance (Show prob, Fractional prob, SingI k, IArray UArray prob) => Covariance (MultiNormal prob k) prob where
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
    
test = train ds :: MultiNormal Double 3