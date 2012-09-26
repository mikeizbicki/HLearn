{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module HMine.Models.Ensemble
    where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Data.Binary
import Data.Hashable
import Data.List
import Data.List.Extras
import Data.Semigroup
import Data.Number.LogFloat hiding (log)
import Debug.Trace
      
import HMine.Base
import HMine.DataContainers
import HMine.Math.Algebra
import HMine.Math.TypeClasses
import HMine.MiscUtils

-------------------------------------------------------------------------------
-- Ensemble

data Ensemble ensparams basemodel label = Ensemble 
    { ensembleL :: [(Double,basemodel)]
    , ensembleDataDesc :: DataDesc label
    , ensembleParams :: ensparams
    }
    deriving (Show,Read,Eq)

pushClassifier :: (Double,model) -> Ensemble modelparams model label -> Ensemble modelparams model label
pushClassifier wm ens = ens { ensembleL = wm:(ensembleL ens) }

pushClassifierNorm :: (Double,model) -> Ensemble modelparams model label -> Ensemble modelparams model label
pushClassifierNorm wm ens = ens { ensembleL = map (\(w,model) -> (w/tot,model)) $ wm:(ensembleL ens) }
    where
        tot = sum $ map fst $ wm:(ensembleL ens)

emptyEnsemble :: DataDesc label -> modelparams -> Ensemble modelparams model label
emptyEnsemble desc modelparams = Ensemble
    { ensembleL = []
    , ensembleDataDesc = desc
    , ensembleParams = modelparams
    }

instance (NFData modelparams, NFData model, NFData label) => NFData (Ensemble modelparams model label) where
    rnf ens = deepseq (rnf $ ensembleL ens) (rnf $ ensembleParams ens)

instance (Invertible model) => Invertible (Ensemble modelparams model label) where
    inverse ens = ens { ensembleL = map (\(w,m) -> (w,inverse m)) $ ensembleL ens }

instance (Label label, Eq modelparams, Semigroup model) => Semigroup (Ensemble modelparams model label) where
    
    (<>) (Ensemble ens1 desc1 params1) (Ensemble ens2 desc2 params2) = Ensemble ens' desc' params'
        where
            ens' = ens1 <> ens2
--             ens' = map merge $ zip (sort' ens1) (sort' ens2)
--             sort' = sortBy (\(w1,_) (w2,_) -> compare w1 w2)
--             merge ((w1,m1),(w2,m2)) = (e2w $ ((w2e w1)+(w2e w2))/2,m1<>m2)
--                 where e2w e = (1/2)*(log $ (1-e)/e)
--                       w2e w = 1/((exp $ 2*w)+1)
--             merge ((w1,m1),(w2,m2)) = ((w1+w2)/2,m1<>m2)
            params' = if params1/=params2
                         then error "Ensemble.semigroup <>: different modelparams"
                         else params1
            desc' = if desc1 /= desc2
                       then error "Ensemble.semigroup <>: different DataDesc"
                       else desc1

-------------------------------------------------------------------------------
-- EnsembleAppender

newtype EnsembleAppenderParams ensparams = EnsembleAppenderParams ensparams
    deriving (Read,Show,Eq)

instance 
    ( BatchTrainer baseparams basemodel datatype label 
    , BatchTrainer ensparams (Ensemble baseparams basemodel label) datatype label 
    ) =>
    BatchTrainer (EnsembleAppenderParams ensparams) (Ensemble{-Appender-} baseparams basemodel label) datatype label 
        where
        
    trainBatch (EnsembleAppenderParams modelparams) datatype = {-liftM EnsembleAppender $-} trainBatch modelparams datatype

-- instance 
--     ( BatchTrainerSS baseparams basemodel datatype label 
--     , BatchTrainerSS ensparams (Ensemble baseparams basemodel label) datatype label 
--     ) =>
--     BatchTrainerSS (EnsembleAppenderParams ensparams) (EnsembleAppender baseparams basemodel label) datatype label 
--         where
--         
--     trainBatchSS (EnsembleAppenderParams modelparams) lds uds = liftM EnsembleAppender $ trainBatchSS modelparams lds uds

newtype EnsembleAppender ensparams basemodel label = EnsembleAppender (Ensemble ensparams basemodel label)
    deriving (Read,Show,Eq)

instance (NFData (Ensemble ensparams basemodel label)) => NFData (EnsembleAppender ensparams basemodel label) where
    rnf (EnsembleAppender ens) = rnf ens

instance (Label label, Eq ensparams) => Semigroup (EnsembleAppender ensparams basemodel label) where
    (<>) (EnsembleAppender (Ensemble ens1 desc1 params1)) (EnsembleAppender (Ensemble ens2 desc2 params2)) = EnsembleAppender $ Ensemble ens' desc' params'
        where 
            ens' = ens1 <> ens2
            params' = if params1/=params2
                         then error "EnsembleAppender.semigroup <>: different modelparams"
                         else params1
            desc' = if desc1 /= desc2
                       then error "EnsembleAppender.semigroup <>: different DataDesc"
                       else desc1

instance 
    ( Classifier (Ensemble ensparams basemodel label) datatype label
    ) => 
    Classifier (EnsembleAppender ensparams basemodel label) datatype label
        where
    
    classify (EnsembleAppender ens) = classify ens

-------------------------------------------------------------------------------
-- Classification

weightedClassify :: (Classifier model datatype label) =>
    Ensemble modelparams model label -> datatype -> (label, Double)
weightedClassify ens dp = argmaxWithMax labelScore (labelL $ ensembleDataDesc ens)
    where 
        labelScore label = sum $ map (\(w,model) -> w*(indicator $ label==classify model dp)) $ ensembleL ens
--         classifyL = map (classify . snd) $ ensembleL ens

instance (Classifier model datatype label) => Classifier (Ensemble modelparams model label) datatype label where
    classify ens dp = argmax labelScore (labelL $ ensembleDataDesc ens)
        where 
            labelScore label = sum $ [w*(indicator $ label==classify model dp) | (w,model) <- ensembleL ens]
--             classifyL = map (classify . snd) $ ensembleL ens

-- instance ( ProbabilityClassifier model label, Eq label) => 
--     ProbabilityClassifier (Ensemble modelparams model label) label where
--         
--     probabilityClassify (Ensemble xs desc params) dp = foldl1' combiner weightedModelL
--         where
-- --             combiner :: (Eq label) => [(label,Probability)] -> [(label,Probability)] -> [(label,Probability)]
--             combiner xs ys = map (\((l1,p1),(l2,p2))->if l1==l2
--                                                          then (l1,p1+p2)
--                                                          else error "Ensemble.probabilityClassify: models output different labels"
--                                                          ) 
--                            $ zip xs ys
--                            
-- --             weightedModelL :: [[(label,Probability)]]
--             weightedModelL = map (\(w,xs) -> map (\(l,p)->(l,(logFloat w)*p)) xs) $ zip weightL' modelL'
--             
--             weightL' = normalizeL weightL
--             modelL' = map (flip probabilityClassify dp) modelL
--             (weightL,modelL)=unzip xs