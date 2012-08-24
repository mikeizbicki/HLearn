{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module HMine.Classifiers.Dirichlet
    where

import Control.DeepSeq
import Data.List.Extras
import Data.Number.LogFloat
import qualified Data.Map as Map
import qualified Data.Foldable as F

import HMine.Classifiers.TypeClasses
import HMine.DataContainers

-------------------------------------------------------------------------------
-- DirichletParams

data DirichletParams = DirichletParams
    deriving (Read,Show,Eq)

instance NFData DirichletParams where
    rnf x = ()

-------------------------------------------------------------------------------
-- Dirichlet

data Dirichlet label = Dirichlet
    { desc :: DataDesc label
    , dist :: Map.Map label Int
    }
    deriving (Read,Show)
    
instance (NFData label) => NFData (Dirichlet label) where
    rnf = rnf . dist

-------------------------------------------------------------------------------
-- Training

instance (OnlineTrainer DirichletParams (Dirichlet label) label) => BatchTrainer DirichletParams (Dirichlet label) label where
    trainBatch = trainOnline

instance (Label label) => OnlineTrainer DirichletParams (Dirichlet label) label where
    
--     emptyModel :: DataDesc -> modelparams -> model
    emptyModel desc modelparams = Dirichlet desc Map.empty

--     add1dp :: DataDesc -> modelparams -> model -> LDPS label -> HMine model
    add1dp desc modelparams model dps = return $ model
        { dist = Map.insertWith (+) (fst dps) 1 (dist model)
        }

-------------------------------------------------------------------------------
-- Classification

instance (Label label) => Classifier (Dirichlet label) label where
    classify model dp = fst $ argmaxBy compare snd $ probabilityClassify model dp

instance (Label label) => ProbabilityClassifier (Dirichlet label) label where
--     probabilityClassify :: model -> DPS -> [(label,Probability)]
    probabilityClassify model dp = 
        case (Map.toList $ dist model) of
             [] -> map (\label -> (label,1/(fromIntegral $ numLabels $ desc model))) $ labelL $ desc model
             xs -> map (\(label,count) -> (label,logFloat $ (fromIntegral count/total))) xs
        where
            total = fromIntegral $ F.foldl' (+) 0 (dist model)::Double

