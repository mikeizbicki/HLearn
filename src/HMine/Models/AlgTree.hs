{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module HMine.Models.AlgTree
    where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Data.List
import Data.List.Extras
import Data.Number.LogFloat
import Data.Semigroup
import Debug.Trace

import HMine.Base
import HMine.DataContainers
import HMine.Math.Algebra
import HMine.Math.TypeClasses
import HMine.MiscUtils
import HMine.Models.Distributions.Dirichlet
import HMine.Models.DTree
import HMine.Models.NBayes

-------------------------------------------------------------------------------
-- AlgTreeParams

data AlgTreeParams = AlgTreeParams
    {
    }
    deriving (Show, Read, Eq)
    
defAlgTreeParams = AlgTreeParams

instance NFData AlgTreeParams where
    rnf params = ()
    
-------------------------------------------------------------------------------
-- AlgTree

data AlgTree label = AlgTree
    { nbayes :: NBayes label
    , splitAttr :: Int
    }

instance (NFData (NBayes label)) => NFData (AlgTree label) where
    rnf (AlgTree nbayes splitAttr) = deepseq nbayes $ rnf splitAttr

-------------------------------------------------------------------------------
-- Functors

nbayes2algtree :: NBayes Int -> AlgTree Int
nbayes2algtree nbayes = AlgTree
    { nbayes = nbayes
    , splitAttr = argmax (runHMine 10 . attrEntropy nbayes) $ attrL $ datadesc nbayes
--     , splitAttr = error "nbayes2algtree: not implemented"
    }
    
-- attrEntropy :: (ProbabilityClassifier (NBayes label) DPS label) => NBayes label -> Int -> HMine Double
attrEntropy :: NBayes Int -> Int -> HMine Double
attrEntropy nb attrI = do
--     let labelL = [0,1,0,1]
    labelL <- liftM (map (cdfInverse (labelDist nb) . logFloat)) $ replicateM 1000 $ getRandomR (0,1::Double)
    sampleL <- mapM (drawSample . getDist nb attrI) labelL
    let pcL = map (\sample -> probabilityClassify nb [(attrI,sample)]) sampleL
    let pcLg = groupBy (\pc1 pc2 -> (classificationLabel pc1)==(classificationLabel pc2)) pcL
    let weightL = normalizeL $ map (fi . length) pcLg
    let entropyL = map (mean . map classificationEntropy) pcLg
    let entropy = sum $ map (\(w,e) -> w*e) $ zip weightL entropyL
    return entropy

-------------------------------------------------------------------------------
-- Algebra

-- instance (Label label, Semigroup (NBayes label)) => Semigroup (AlgTree label) where
instance Semigroup (AlgTree Int) where
    (<>) algtree1 algtree2 = nbayes2algtree $ (nbayes algtree1) <> (nbayes algtree2)

-- instance (Label label, Invertible (NBayes label)) => Invertible (AlgTree label) where
instance Invertible (AlgTree Int) where
    inverse algtree = nbayes2algtree $ inverse (nbayes algtree)

-------------------------------------------------------------------------------
-- Training

instance BatchTrainer AlgTreeParams (AlgTree Int) DPS Int where
    trainBatch = trainOnline

instance EmptyTrainer AlgTreeParams (AlgTree Int) Int where
    emptyModel desc modelparams = AlgTree 
        { nbayes = emptyModel desc defNBayesParams
        , splitAttr = trace "WARNING: AlgTree.EmptyTrainer: no splitAttr yet" $ 0
        }

instance OnlineTrainer AlgTreeParams (AlgTree Int) DPS Int where
    add1dp desc modelparams model dp = do
        nbayes' <- add1dp desc defNBayesParams (nbayes model) dp
        return $ nbayes2algtree nbayes'
        
-------------------------------------------------------------------------------
-- Classification

instance Classifier (AlgTree Int) DPS Int where
    classify model dp = classificationLabel $ probabilityClassify model dp

instance ProbabilityClassifier (AlgTree Int) DPS Int where
              
    probabilityClassify model dp = probabilityClassify (nbayes model) dp'
        where dp' = filter (\(attrI,val) -> attrI==splitAttr model) dp