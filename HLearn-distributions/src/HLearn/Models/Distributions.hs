{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This file exports the most commonly used modules within HLearn-distributions.  Most likely this is the only file you will have to import.

module HLearn.Models.Distributions
    ( module HLearn.Models.Distributions.Common
    , module HLearn.Models.Distributions.Categorical
--     , module HLearn.Models.Distributions.Gaussian
    , module HLearn.Models.Distributions.KernelDensityEstimator
    , module HLearn.Models.Distributions.Moments
    )
    where

import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Categorical
-- import HLearn.Models.Distributions.Gaussian
import HLearn.Models.Distributions.KernelDensityEstimator
import HLearn.Models.Distributions.Moments
