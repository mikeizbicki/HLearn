module HLearn.Optimization.StepSize
    ( 
    -- * fancy step sizes
    -- ** Almeida Langlois
    lrAlmeidaLanglois


    -- * simple step sizes
    -- ** linear decrease
    , lrLinear
    , eta
--     , gamma

    -- ** constant step size
    , lrConst
--     , step
    )
    where

import HLearn.Optimization.StepSize.Linear 
import HLearn.Optimization.StepSize.Const 
import HLearn.Optimization.StepSize.AlmeidaLanglois
