{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module HMine.Models.Distributions.Dirichlet
    where

import Control.DeepSeq
import Data.List.Extras
import Data.Number.LogFloat
import Data.Semigroup

import qualified Data.Map as Map
import qualified Data.Foldable as F

import HMine.Base
import HMine.Math.Algebra
import HMine.Math.TypeClasses
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
        { pdfmap :: Map.Map label Int
        } 
    deriving (Show,Read,Eq)

instance (NFData label) => NFData (Dirichlet label) where
    rnf d = rnf $ pdfmap d

-------------------------------------------------------------------------------
-- Distribution

instance (Ord label) => Distribution (Dirichlet label) label where
    
    {-# INLINE add1sample #-}
    add1sample dist label = Dirichlet $ Map.insertWith (+) label 1 (pdfmap dist)

    {-# INLINE pdf #-}
    pdf dist label = logFloat $ 0.0001+((fi val)/(fi tot)::Double)
        where
            val = case Map.lookup label (pdfmap dist) of
                Nothing -> 0
                Just x  -> x
            tot = F.foldl' (+) 0 $ pdfmap dist

    {-# INLINE cdf #-}
    cdf dist label = (fi $ Map.foldl' (+) 0 $ Map.filterWithKey (\k a -> k<=label) $ pdfmap dist) 
                   / (fi $ Map.foldl' (+) 0 $ pdfmap dist)
                   
    cdfInverse dist prob = argmax (cdf dist) $ Map.keys $ pdfmap dist
    
-------------------------------------------------------------------------------
-- Algebra

instance (Label label) => Semigroup (Dirichlet label) where
    (<>) d1 d2 = Dirichlet $ Map.unionWith (+) (pdfmap d1) (pdfmap d2)
    
instance (Label label) => Monoid (Dirichlet label) where
    mempty = Dirichlet mempty
    mappend = (<>)

instance (Label label) => Invertible (Dirichlet label) where
    inverse d1 = Dirichlet $ Map.map (0-) (pdfmap d1)


-- data Dirichlet label = Dirichlet
--     { desc :: DataDesc label
--     , dist :: Map.Map label Int
--     }
--     deriving (Read,Show)
--     
-- instance (NFData label) => NFData (Dirichlet label) where
--     rnf = rnf . dist
-- 
-- -------------------------------------------------------------------------------
-- -- Training
-- 
-- instance (OnlineTrainer DirichletParams (Dirichlet label) datatype label) => 
--     BatchTrainer DirichletParams (Dirichlet label) datatype label 
--         where
--               
--     trainBatch = trainOnline
-- 
-- instance (Label label) => EmptyTrainer DirichletParams (Dirichlet label) label where
--     emptyModel desc modelparams = Dirichlet desc Map.empty
-- 
-- instance (Label label) => OnlineTrainer DirichletParams (Dirichlet label) datatype label where
--     add1dp desc modelparams model dps = return $ model
--         { dist = Map.insertWith (+) (fst dps) 1 (dist model)
--         }
-- 
-- -------------------------------------------------------------------------------
-- -- Classification
-- 
-- instance (Label label) => Classifier (Dirichlet label) datatype label where
--     classify model dp = fst $ argmaxBy compare snd $ probabilityClassify model dp
-- 
-- instance (Label label) => ProbabilityClassifier (Dirichlet label) datatype label where
-- --     probabilityClassify :: model -> DPS -> [(label,Probability)]
--     probabilityClassify model dp = 
--         case (Map.toList $ dist model) of
--              [] -> map (\label -> (label,1/(fromIntegral $ numLabels $ desc model))) $ labelL $ desc model
--              xs -> map (\(label,count) -> (label,logFloat $ (fromIntegral count/total))) xs
--         where
--             total = fromIntegral $ F.foldl' (+) 0 (dist model)::Double
-- 
