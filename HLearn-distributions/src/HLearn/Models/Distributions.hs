{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This file exports the most commonly used modules within HLearn-distributions.  Most likely this is the only file you will have to import.

module HLearn.Models.Distributions
    ( module HLearn.Models.Distributions.Common
    , module HLearn.Models.Distributions.Univariate.Categorical
    , module HLearn.Models.Distributions.Univariate.Moments
    , module HLearn.Models.Distributions.Univariate.Uniform
--     , module HLearn.Models.Distributions.KernelDensityEstimator
    , module HLearn.Models.Distributions.Multivariate.MultiNormal
    , module HLearn.Models.Distributions.Multivariate.Multivariate
    )
    where

import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Univariate.Categorical
import HLearn.Models.Distributions.Univariate.Gaussian
import HLearn.Models.Distributions.Univariate.Moments
import HLearn.Models.Distributions.Univariate.Uniform
import HLearn.Models.Distributions.Multivariate.Multivariate
import HLearn.Models.Distributions.Multivariate.MultiNormal