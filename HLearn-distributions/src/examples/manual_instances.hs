{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Distributions.Multivariate.Internal.CatContainer
import HLearn.Models.Distributions.Multivariate.Internal.Container

data Person = Person
    { name :: String
    , city :: String
    , job :: Job
    , age :: Double
    , salary :: Double
    }
    deriving (Read,Show,Eq,Ord)

data Job = Programmer | Manager | Sales
    deriving (Read,Show,Eq,Ord)

instance Trainable Person where
    type GetHList Person = HList '[String,Job,Double,Double]
    getHList p = city p:::job p:::age p:::salary p:::HNil

instance MultivariateLabels Person where
    getLabels _ = ["city","job","age","salary"]

people = 
    [ Person "Sally"     "San Francisco" Programmer 22 80000
    ]

dist1a = train [] :: Multivariate Person
   '[ '[CatContainer String]
    , '[CatContainer Job]
    , Independent Normal '[Double,Double]
    ]
    Double

dist1b = train [] :: Multivariate Person
   '[ MultiCategorical '[String,Job]
    , Independent Normal '[Double,Double]
    ]
    Double

data Sample = Sample

instance Trainable Sample where
    type GetHList Sample = HList ('[String,String] ++ (Replicate 5 Double) ++ (Replicate 4 Double))
    getHList _
        = "test":::"test":::
          1:::1:::1:::1:::1:::
          2:::2:::2:::2:::HNil
        
instance MultivariateLabels Sample where
    getLabels _ = ["cat1","cat2","a1","a2","a3","a4","a5","b1","b2","b3","b4"]

dist2 = train [] :: Multivariate Sample
   '[ MultiCategorical '[String,String]
    , Dependent MultiNormal '[Double,Double,Double,Double,Double]
    , Independent Exponential '[Double,Double]
    , Independent Normal '[Double,Double]
    ]
    Double

