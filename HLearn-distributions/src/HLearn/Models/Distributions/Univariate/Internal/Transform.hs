{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HLearn.Models.Distributions.Univariate.Internal.Transform
    where

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Univariate.Normal
import HLearn.Models.Distributions.Visualization.Gnuplot

-------------------------------------------------------------------------------
-- data types

newtype Transform func dist prob = Transform (dist prob)
    deriving (Read,Show,Eq,Ord,Monoid,Group)

class Function f domain range | f -> domain range where
    function :: f -> domain -> range

data Exp = Exp
instance (Floating a) => Function Exp a a where
    function _ = exp

data Log = Log
instance (Floating a) => Function Log a a where
    function _ = log

data Logit = Logit
instance (Floating a) => Function Logit a a where
    function _ a = log $ (1-a)/a

-------------------------------------------------------------------------------
-- training

instance 
    ( Function func (Datapoint (dist prob)) (Datapoint (dist prob))
    , Num (Datapoint (dist prob))
    , HomTrainer (dist prob)
    ) => HomTrainer (Transform func dist prob) 
        where
    type Datapoint (Transform function dist prob) = Datapoint (dist prob)
    train1dp dp = Transform $ train1dp $ function (undefined :: func) dp

-------------------------------------------------------------------------------
-- distribution

instance Probabilistic (Transform func dist prob) where
    type Probability (Transform func dist prob) = Probability (dist prob)

instance 
    ( Function func prob prob
    , prob ~ Datapoint (dist prob)
    , PDF (dist prob)
    ) => PDF (Transform func dist prob) 
        where
    pdf (Transform dist) dp = pdf dist $ function (undefined::func) dp

-- instance (Floating prob) => Mean (LogNormal prob) where
--     mean (LogNormal dist) = exp $ (mean dist)+(variance dist)/2
-- 
-- instance (Floating prob) => Variance (LogNormal prob) where
--     variance (LogNormal dist) = ((exp $ variance dist) -1)*(exp $ 2*(mean dist)+(variance dist))

instance 
    ( Floating prob
    , Enum prob
    , Show prob
    , Ord prob
    , prob ~ Datapoint (dist prob)
    , prob ~ Probability (dist prob)
    , Function func prob prob
    , PDF (dist prob)
    , Mean (dist prob)
    , Variance (dist prob)
    ) => PlottableDistribution (Transform func dist prob) where
    
    plotType _ = Continuous

    samplePoints (Transform dist) = samplesFromMinMax min max
        where
            min = (mean dist)-5*(sqrt $ variance dist)
            max = (mean dist)+5*(sqrt $ variance dist)
