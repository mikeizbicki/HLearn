{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This file exports the most commonly used modules within HLearn-classifiers.  Most likely this is the only file you will have to import.

module HLearn.Models.Classifiers
    ( module HLearn.Models.Classifiers.Common
    , module HLearn.Models.Classifiers.Bayes
    , module HLearn.Models.Classifiers.NearestNeighbor
    , module HLearn.Models.Classifiers.Perceptron
    , module HLearn.Models.Classifiers.Experimental.Boosting.MonoidBoost
    , module HLearn.Evaluation.CrossValidation
    )
    where

import HLearn.Models.Classifiers.Common
import HLearn.Models.Classifiers.Bayes
import HLearn.Models.Classifiers.NearestNeighbor
import HLearn.Models.Classifiers.Perceptron
import HLearn.Models.Classifiers.Experimental.Boosting.MonoidBoost
import HLearn.Evaluation.CrossValidation
