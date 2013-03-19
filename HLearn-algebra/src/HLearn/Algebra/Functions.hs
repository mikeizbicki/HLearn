{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module contains \"low-level higher order functions\" for manipulating algebraic homomorphisms.  You probably want to use the 'HomTrainer' type-class rather than using these functions directly.

module HLearn.Algebra.Functions
    ( 
    -- * Parallelism
    parallel
    -- * Manipulating homomorphisms
    , online, offline
    , batch, batchCK, unbatch
    , semigroup
    -- * Helper functions
    , reduce
    )
    where

import qualified Control.ConstraintKinds as CK
-- import Control.Applicative
import Control.Concurrent
import Control.Parallel.Strategies
-- import Data.Traversable
import qualified Data.Foldable as F
import GHC.Exts (Constraint)
import Prelude hiding (filter)
import System.IO.Unsafe

import HLearn.Algebra.Structures.Groups

-------------------------------------------------------------------------------
-- higher order functions

-- | Parallelizes any batch trainer to run over multiple processors on a single machine.  The function automatically detects the number of available processors and parallelizes the function accordingly.  This requires the use of unsafePerformIO, however, the result should still be safe.
parallel :: 
    ( Semigroup model
    , NFData model
    , CK.Partitionable container
    , CK.PartitionableConstraint container datapoint
    ) => (container datapoint -> model) -- ^ sequential batch trainer
      -> (container datapoint -> model) -- ^ parallel batch trainer
parallel train = \datapoint ->
--     F.foldl' (mappend) mempty $ parMap strat train (CK.partition n datapoint)
    reduce $ parMap strat train (CK.partition n datapoint)
    where
        strat = rdeepseq
        n = unsafePerformIO $ getNumCapabilities

-- | Converts a batch trainer into an online trainer.  The input function should be a semigroup homomorphism.
online :: 
    ( Semigroup model
    ) => (datapoint -> model) -- ^ singleton trainer
      -> (model -> datapoint -> model) -- ^ online trainer
online train = \model datapoint -> model <> (train datapoint)

-- | The inverse of 'online'.  Converts an online trainer into a batch trainer.
offline :: 
    ( Monoid model
    ) => (model -> datapoint -> model) -- ^ online singleton trainer
      -> (datapoint -> model) -- ^ singleton trainer
offline train = \dp -> train mempty dp

-- | Converts a singleton trainer into a batch trainer, which is also a semigroup homomorphism.
batch ::
    ( Monoid model
    , Functor container
    , F.Foldable container
    ) => (datapoint -> model) -- ^ singleton trainer
      -> (container datapoint -> model) -- ^ batch trainer
batch train = \dps -> F.foldl mappend mempty $ fmap train dps

batchCK ::
    ( Monoid model
    , CK.Functor container
    , CK.FunctorConstraint container model
    , CK.FunctorConstraint container datapoint
    , CK.Foldable container
    , CK.FoldableConstraint container model
    ) => (datapoint -> model) -- ^ singleton trainer
      -> (container datapoint -> model) -- ^ batch trainer
batchCK train = \dps -> CK.foldl' mappend mempty $ CK.fmap train dps

-- | Inverse of 'unbatch'.  Converts a semigroup homomorphism into a singleton trainer.
unbatch :: 
    ([datapoint] -> model) -- ^ batch trainer
    -> (datapoint -> model) -- ^ singleton trainer
unbatch train = \datapoint -> train [datapoint]

-- | Normally we would define our semigroup operation explicitly.  However, it is possible to generate one from an online trainer and a pseudo inverse.
semigroup :: 
    (model -> datapoint -> model) -- ^ online trainer
    -> (model -> datapoint) -- ^ pseudo inverse
    -> (model -> model -> model) -- ^ The semigroup operation
semigroup trainonline pseudoinverse = \model1 model2 -> trainonline model1 (pseudoinverse model2)
    
-------------------------------------------------------------------------------
-- Helper Functions

-- | Like fold, but (i) only for use on the semigroup operation (\<\>) and (ii) uses the fan-in reduction strategy which is more efficient when the semigroup operation takes nonconstant time depending on the size of the data structures being reduced.
reduce :: 
    ( Semigroup sg
    , F.Foldable container
    ) => container sg -> sg
reduce = reduceL . F.toList

reduceCK :: 
    ( Semigroup sg
    , CK.Foldable container
    , CK.FoldableConstraint container sg
    , CK.FoldableConstraint container [sg]
    ) => container sg -> sg
reduceCK = reduceL . CK.toList

reduceL :: (Semigroup sg) => [sg] -> sg
reduceL []  = error "reduce: cannot reduce empty list"
reduceL [x] = x
reduceL xs  = reduceL $ itr xs
    where
        itr :: (Semigroup sg) => [sg] -> [sg]
        itr []            = []
        itr [x]           = [x]
        itr (x1:x2:xs)    = (x1<>x2):(itr xs)
