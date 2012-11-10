{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module HLearn.Algebra.Functions
    where

import qualified Control.ConstraintKinds as CK
-- import Control.Applicative
import Control.Concurrent
import Control.Parallel.Strategies
-- import Data.Traversable
-- import qualified Data.Foldable as F
import GHC.Exts (Constraint)
import Prelude hiding (filter)
import System.IO.Unsafe

import HLearn.Algebra.Structures

-------------------------------------------------------------------------------
-- Functions

reduce :: 
    ( Semigroup sg
    , CK.Foldable container
    , CK.FoldableConstraint container sg
    , CK.FoldableConstraint container [sg]
    ) => container sg -> sg
reduce = reduceL . CK.toList

reduceL :: (Semigroup sg) => [sg] -> sg
reduceL []  = error "reduce: cannot reduce empty list"
reduceL [x] = x
reduceL xs  = reduceL $ itr xs
    where
        itr :: (Semigroup sg) => [sg] -> [sg]
        itr []            = []
        itr [x]           = [x]
        itr (x1:x2:xs)    = (x1<>x2):(itr xs)

instance Semigroup Integer where
    (<>) = (+)

-------------------------------------------------------------------------------
-- higher order functions

-- | if `train` is a semigroup homomorphism, then `online train` = `train`
online :: 
    ( Semigroup model
    ) => (datapoint -> model) -- ^ trains single data point
      -> (model -> datapoint -> model) -- ^ trains in online mode
online train = \model datapoint -> model <> (train datapoint)

offline :: 
    ( HasIdentity model
    ) => (model -> datapoint -> model) 
      -> (datapoint -> model)
offline train = \dp -> train identity dp

batch ::
    ( Monoid model
    , CK.Functor container
    , CK.FunctorConstraint container model
    , CK.FunctorConstraint container datapoint
    , CK.Foldable container
    , CK.FoldableConstraint container model
    ) => (datapoint -> model) -- ^ trains single data point
      -> (container datapoint -> model) -- ^ trains in batch mode
batch train = \dps -> CK.foldl' mappend mempty $ CK.fmap train dps

unbatch :: ([datapoint] -> model) -> (datapoint -> model)
unbatch train = \datapoint -> train [datapoint]

semigroup :: (model -> datapoint -> model) -> (model -> datapoint) -> (model -> model -> model)
semigroup trainonline pseudoinverse = \model1 model2 -> trainonline model1 (pseudoinverse model2)
    

parallel :: 
    ( Semigroup model
    , NFData model
    , CK.Partitionable container
    , CK.PartitionableConstraint container datapoint
    , CK.FoldableConstraint container model
    , CK.Functor container
    , CK.FunctorConstraint container model
    , CK.FunctorConstraint container datapoint
    , CK.Applicative container
    , CK.ApplicativeConstraint container datapoint
    , CK.Traversable container
    , CK.TraversableConstraint container model
    , CK.TraversableConstraint container datapoint
    ) => (container datapoint -> model) -> (container datapoint -> model)
parallel train = \datapoint ->
--     F.foldl' (mappend) mempty $ parMap strat train (CK.partition n datapoint)
    reduce $ parMap strat train (CK.partition n datapoint)
    where
        strat = rdeepseq
        n = unsafePerformIO $ getNumCapabilities
