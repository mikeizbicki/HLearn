{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module HLearn.Algebra.Functions
    where

import qualified Control.ConstraintKinds as CK
import Control.Applicative
import Control.Parallel.Strategies
import Data.Traversable
import qualified Data.Foldable as F
import GHC.Exts (Constraint)

import HLearn.Algebra.Models
import HLearn.Algebra.Structures

-------------------------------------------------------------------------------
-- Functions

-- class Surjective input output function
-- class Injective input output function
-- class (Surjective input output function, Injective input output function) => 
--     Bijective input output function
-- 
-- class (objtype input, objtype output) => 
--     Homomorphism (objtype :: * -> Constraint) input output function
-- 
-- class ( Surjective input output function, Homomorphism objtype input output function) => 
--     Epimorphism objtype input output function
-- 
-- class PseudoInverse inverse function
-- class Inverse inverse function
-- 
-- instance (Inverse inverse function) => PseudoInverse inverse function

-------------------------------------------------------------------------------
-- higher order functions

-- | if `train` is a semigroup homomorphism, then `online train` = `train`
online :: 
    ( Model modelparams model
    , Semigroup model
    ) => 
    (modelparams -> datapoint -> model) -- ^ trains single data point
        -> (model -> datapoint -> model) -- ^ trains in online mode
online train = \model datapoint -> model <> (train (params model) datapoint)

batch ::
    ( Monoid model
    , CK.Functor container
    , CK.FunctorConstraint container model
    , CK.FunctorConstraint container datapoint
    , CK.Foldable container
    , CK.FoldableConstraint container model
    , Model modelparams model
    ) =>
    (modelparams -> datapoint -> model) -- ^ trains single data point
        -> (modelparams -> container datapoint -> model) -- ^ trains in batch mode
batch train = \modelparams dps -> CK.foldl' mappend mempty $ CK.fmap (train modelparams) dps

-- single :: (Applicative container, F.Foldable container, Functor container) =>
--     (modelparams -> container datapoint -> model) -> (modelparams -> datapoint -> model)
-- single batchTrain = \modelparams datapoint -> batchTrain modelparams $ pure datapoint

batchInv :: (modelparams -> [datapoint] -> model) -> (modelparams -> datapoint -> model)
batchInv batchTrain = \modelparams datapoint -> batchTrain modelparams [datapoint]

semigroup :: 
--     ( Semigroup datapoint
--     , PseudoInverse (model -> datapoint) (modelparams -> datapoint -> model)
--     ) =>
    (model -> datapoint -> model) -> (model -> datapoint) -> (model -> model -> model)
semigroup trainonline pseudoinverse = \model1 model2 -> trainonline model1 (pseudoinverse model2)

parallel :: 
    ( Semigroup model
    , Applicative container
    , Traversable container
    ) => 
    Strategy model 
        -> (modelparams -> container datapoint -> model) 
        -> (modelparams -> container datapoint -> model)        
parallel strat train = \modelparams datapoint -> 
    F.foldl1 (<>) $ (withStrategy (parTraversable strat) . fmap (train modelparams . pure)) datapoint