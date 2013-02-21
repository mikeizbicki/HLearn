{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}


-- | Used for multivariate distributions
module HLearn.Models.Distributions.MultiDist'
    where

import qualified Data.Array as Arr

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Unital

-------------------------------------------------------------------------------
-- data types

data MultiDistParams dp baseparams prob = MultiDistParams 
    { bounds :: (dp,dp)
    , baseparams :: baseparams
    }
    deriving (Read,Show,Eq,Ord)

data MultiDist' dp basedist baseparams prob = MultiDist'
    { params :: MultiDistParams dp baseparams prob
    , arr :: Arr.Array dp basedist
    , count :: prob
    }
    deriving (Read,Show,Eq,Ord)

type MultiDist dp basedist baseparams prob = RegSG2Group (MultiDist' dp basedist baseparams prob)

type MultiCategorical dp prob = MultiDist dp (Unital prob) (NoParams (Unital prob)) prob

-------------------------------------------------------------------------------
-- algebra

instance (Num prob, Eq baseparams, Arr.Ix dp, Semigroup basedist) => Semigroup (MultiDist' dp basedist baseparams prob) where
    mc1 <> mc2 = if params mc1 /= params mc2
            then error "MultiDist'.(<>): farting"
            else mc1 
                { arr = Arr.array (Arr.bounds $ arr mc1) $ zipWith go (Arr.assocs $ arr mc1) (Arr.assocs $ arr mc2)
                , count = (count mc1)+(count mc2)
                }
        where
            go (i1,a1) (i2,a2) = if i1/=i2
                then error "MultiDist'.(<>).go: farting"
                else (i1,a1<>a2)

-------------------------------------------------------------------------------
-- training

instance (Eq baseparams, Arr.Ix dp) => Model (MultiDistParams dp baseparams prob) (MultiDist dp basedist baseparams prob) where
    getparams (SGJust md) = params md

instance 
    ( Arr.Ix dp
    , Num prob
    , HomTrainer baseparams basedp basedist
    ) =>  HomTrainer (MultiDistParams dp baseparams prob) (dp,basedp) (MultiDist dp basedist baseparams prob) 
        where
    train1dp' params (dp,basedp) = SGJust $ MultiDist' 
        { params = params
        , count = 1
        , arr = Arr.array (bounds params)
            [ if i/=dp
                then (i,mempty) 
                else (i,train1dp' (baseparams params) basedp)
            | i <- Arr.range (bounds params)
            ]
        }

-------------------------------------------------------------------------------
-- distributions

instance
    ( Distribution basedist basedp prob
    , Arr.Ix dp
    , Fractional prob
    ) => Distribution (MultiDist dp basedist baseparams prob) (dp,basedp) prob 
        where
    pdf (SGJust dist) (dp,basedp) = (pdf ((arr dist) Arr.! dp) basedp)/(count dist)

-------------------------------------------------------------------------------
-- testing

-- instance HomTrainer (MultiDistParams dp (NoParams (Unital prob))) dp (MultiDist dp (Unital prob) (NoParams (Unital prob))) where
--     train1dp' params dp = train1dp' params (dp,())
        
ds= [ ((1,1),())
    , ((1,2),())
    , ((2,2),())
    ] :: [((Int,Int),())]
    
multicatParams :: (dp,dp) -> MultiDistParams dp (NoParams (Unital Double)) Double
multicatParams bounds = MultiDistParams 
    { bounds = bounds
    , baseparams = NoParams :: NoParams (Unital Double)
    }
    
-- test = train' (MultiDistParams ((1,1),(2,2)) (NoParams :: NoParams (Unital Double))) ds
test = train' (multicatParams ((1,1),(2,2))) ds

