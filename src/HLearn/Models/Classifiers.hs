-- | This file exports the most commonly used modules within HLearn-classifiers.  Most likely this is the only file you will have to import.

module HLearn.Models.Classifiers
    ( module HLearn.Models.Classifiers.Common
    , module HLearn.Models.Classifiers.Bayes
    , module HLearn.Models.Classifiers.NearestNeighbor
    , module HLearn.Models.Classifiers.Perceptron
--     , module HLearn.Models.Classifiers.Experimental.Boosting.FiniteBoost
--     , module HLearn.Models.Classifiers.Experimental.Boosting.MonoidBoost
--     , module HLearn.Models.Regression.PowerLaw
--     , module HLearn.Models.Regression.Common
--     , module HLearn.Models.Regression.Logarithmic
--     , module HLearn.Models.Regression.Exponential
--     , module HLearn.Models.Regression.ModifiedExponential
    , module HLearn.Evaluation.CrossValidation
    , module HLearn.Evaluation.RSquared
    )
    where

import HLearn.Models.Classifiers.Common
import HLearn.Models.Classifiers.Bayes
import HLearn.Models.Classifiers.NearestNeighbor
import HLearn.Models.Classifiers.Perceptron
-- import HLearn.Models.Classifiers.Experimental.Boosting.FiniteBoost
-- import HLearn.Models.Classifiers.Experimental.Boosting.MonoidBoost
-- import HLearn.Models.Regression.Common
-- import HLearn.Models.Regression.Logarithmic
-- import HLearn.Models.Regression.Exponential
-- import HLearn.Models.Regression.ModifiedExponential
import HLearn.Models.Regression.PowerLaw
import HLearn.Evaluation.CrossValidation
import HLearn.Evaluation.RSquared
