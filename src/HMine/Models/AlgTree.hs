{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module HMine.Models.AlgTree
    where

import Data.List.Extras
import Data.Semigroup

import HMine.Base
import HMine.DataContainers
import HMine.Math.Algebra
import HMine.Math.TypeClasses
import HMine.Models.Distributions.Dirichlet
import HMine.Models.DTree
import HMine.Models.NBayes

-------------------------------------------------------------------------------
-- AlgTreeParams

data AlgTreeParams = AlgTreeParams
    {
    }
    
-------------------------------------------------------------------------------
-- AlgTree

data AlgTree label = AlgTree
    { nbayes :: NBayes label
    , dtree :: DTree (Dirichlet label) label
    }

-------------------------------------------------------------------------------
-- Functors

nbayes2algtree :: (Label label) => NBayes label -> AlgTree label
nbayes2algtree nbayes = AlgTree
    { nbayes = nbayes
    , dtree = error "nbayes2algtree: not implemented"
    }
    
nbayes2dstump :: Int -> NBayes Int -> AlgTree Int
nbayes2dstump attrI nb = runHMine 0 $ do
    {-replicateM 1000 $-} 
--     x <- sequence $ replicate [(drawSample $ getDist nb attrI label) :: HMine DataItem | label <- labelL $ datadesc nb]
    return undefined
-- firstLabel :: Int -> NBayes label -> label
-- firstLabel attrI nbayes = fst $ argmax snd $ map mapper $ zip [] (labelL $ datadesc nbayes)
--     where
--         mapper (dist,l) = (l,sampleProb dist (0::Double))
-------------------------------------------------------------------------------
-- Algebra

instance (Label label, Semigroup (NBayes label)) => Semigroup (AlgTree label) where
    (<>) algtree1 algtree2 = nbayes2algtree $ (nbayes algtree1) <> (nbayes algtree2)

instance (Label label, Invertible (NBayes label)) => Invertible (AlgTree label) where
    inverse algtree = nbayes2algtree $ inverse (nbayes algtree)

-------------------------------------------------------------------------------
-- Training

instance BatchTrainer AlgTreeParams (AlgTree Int) DPS Int where
    trainBatch = trainOnline

instance EmptyTrainer AlgTreeParams (AlgTree Int) Int where
    emptyModel desc modelparams = AlgTree (emptyModel desc defNBayesParams) undefined

instance OnlineTrainer AlgTreeParams (AlgTree Int) DPS Int where
    add1dp desc modelparams model dp = do
        nbayes' <- add1dp desc defNBayesParams (nbayes model) dp
        return $ nbayes2algtree nbayes'
        
-------------------------------------------------------------------------------
-- Classification

instance (ProbabilityClassifier (DTree (Dirichlet label) label) datatype label) => 
    ProbabilityClassifier (AlgTree label) datatype label 
        where
    probabilityClassify model dp = probabilityClassify (dtree model) dp