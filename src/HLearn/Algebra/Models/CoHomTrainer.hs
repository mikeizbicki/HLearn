module HLearn.Algebra.Models.CoHomTrainer
    where

import Control.Monad
import Control.Monad.Random

import HLearn.Algebra.Models.HomTrainer
import HLearn.Algebra.Structures.Groups

-- | A "CoHomTrainer" is a formal way of describing a generative model.
class CoHomTrainer model where
    type CoDatapoint model

    -- | sample with replacement a single datapoint
    cotrain1dp :: model -> Rand g (CoDatapoint model)
    cotrain1dp model = fmap fst (coadd1dp model)

    -- | samples infinitely many datapoints with replacement
    cotrain :: model -> Rand g ([CoDatapoint model])
    cotrain = sequence . repeat . cotrain1dp 
        
    -- | sample without replacement a single datapoint
    coadd1dp :: model -> Rand g ((CoDatapoint model, model))

    -- | sample without replacement as many datapoints as possible
    coaddBatch :: model -> Rand g ([(CoDatapoint model, model)])
    coaddBatch = sequence . repeat . coadd1dp
