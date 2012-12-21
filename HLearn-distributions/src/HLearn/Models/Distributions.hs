{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module HLearn.Models.Distributions
    ( module HLearn.Models.Distributions.Common
    , module HLearn.Models.Distributions.Categorical
    , module HLearn.Models.Distributions.KernelDensityEstimator
--     , module HLearn.Models.Distributions.Gaussian
    )
    where

import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Categorical
import HLearn.Models.Distributions.KernelDensityEstimator
-- import HLearn.Models.Distributions.Gaussian
-- import HLearn.DataContainers
-- import HLearn.Math.TypeClasses

import qualified Data.Map as Map
import Debug.Trace
import Data.List.Extras

-------------------------------------------------------------------------------
-- Training

-- instance (OnlineTrainer CategoricalParams (Categorical label) datatype label) => 
--     BatchTrainer CategoricalParams (Categorical label) datatype label 
--         where
--               
--     trainBatch = trainOnline
-- 
-- instance (Label label) => EmptyTrainer CategoricalParams (Categorical label) label where
--     emptyModel desc modelparams = Categorical Map.empty (Just desc)
-- 
-- instance (Label label) => OnlineTrainer CategoricalParams (Categorical label) datatype label where
--     add1dp desc modelparams model dps = return $ add1sample model $ fst dps
-- 
-- -------------------------------------------------------------------------------
-- -- Classification
-- 
-- instance (Label label) => Classifier (Categorical label) datatype label where
--     classify model dp = mean model --fst $ argmaxBy compare snd $ probabilityClassify model dp
-- 
-- instance (Label label) => ProbabilityClassifier (Categorical label) datatype label where
--     probabilityClassify model dp = model--trace "CategoricalPC" $
-- --         case Map.keys $ pdfmap model of
-- --             [] -> trace "WARNING: ProbabilityClassifier: empty Categorical" $ 
-- --                 case desc model of
-- --                     Nothing -> error "probabilityClassify: empty Categorical and empty DataDesc"
-- --                     Just desc -> map (\label -> (label,1/(fromIntegral $ numLabels desc))) $ labelL desc
-- --             xs -> map (\k -> (k,pdf model k)) xs
