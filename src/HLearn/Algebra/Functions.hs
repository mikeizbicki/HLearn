{-# LANGUAGE PolyKinds #-}

-- | This module contains \"low-level higher order functions\" for manipulating algebraic homomorphisms.  You probably want to use the "HomTrainer" type-class rather than using these functions directly.

module HLearn.Algebra.Functions
    ( 
    -- * Almost dependently typed function
    Function (..)
    
    -- * Higher order functions
    -- ** Parallelism
    , parallel
    , parallelN

    -- ** Manipulating homomorphisms
    , online, offline
    , batch, batchCK, unbatch
    , semigroup
    -- ** Helper functions
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

import HLearn.Algebra.Structures.Comonoid
import HLearn.Algebra.Structures.Groups

-------------------------------------------------------------------------------
-- type classes

-- | Every data type that implements this class has a corresponding function.  We can use this data type as type level parameters to other data types.  This gives us some of the benefit of dependently typed functions.
class Function f domain range | f domain -> range where
    function :: f -> domain -> range

-------------------------------------------------------------------------------
-- higher order functions

-- | Parallelizes any batch trainer to run over multiple processors on a single machine.  
parallelN :: 
    ( Comonoid domain 
    , Monoid range
    , NFData range
    ) => Int -- ^ number of parallel threads
      -> (domain -> range) -- ^ sequential batch trainer
      -> (domain -> range) -- ^ parallel batch trainer
parallelN n train = \datapoint ->
    reduce $ parMap strat train (partition n datapoint)
    where
        strat = rdeepseq

-- | Parallelizes any batch trainer to run over multiple processors on a single machine.  The function automatically detects the number of available processors and parallelizes the function accordingly.  This requires the use of unsafePerformIO, however, the result should still be safe.
parallel :: 
    ( Comonoid domain 
    , Monoid range
    , NFData range
    ) => (domain -> range) -- ^ sequential batch trainer
      -> (domain -> range) -- ^ parallel batch trainer
parallel = parallelN (unsafePerformIO getNumCapabilities) 

-- safeParallel1 :: (NonCocommutative domain, Monoid range, NFData range) => (domain -> range) -> (domain -> range)
-- safeParallel2 :: (Cocommutative domain, Abelian range, NFData range) => (domain -> range) -> (domain -> range)

-- | Converts a batch trainer into an online trainer.  The input function should be a semigroup homomorphism.
online :: 
    ( Monoid model
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
    , F.Foldable container
    ) => (datapoint -> model) -- ^ singleton trainer
      -> (container datapoint -> model) -- ^ batch trainer
batch = F.foldMap

batchCK ::
    ( Monoid model
    , CK.Foldable container
    , CK.FoldableConstraint container model
    , CK.FoldableConstraint container datapoint
    ) => (datapoint -> model) -- ^ singleton trainer
      -> (container datapoint -> model) -- ^ batch trainer
batchCK = CK.foldMap

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
    ( Monoid sg
    , F.Foldable container
    ) => container sg -> sg
reduce = reduceL . F.toList

reduceCK :: 
    ( Monoid sg
    , CK.Foldable container
    , CK.FoldableConstraint container sg
    , CK.FoldableConstraint container [sg]
    ) => container sg -> sg
reduceCK = reduceL . CK.toList

reduceL :: (Monoid sg) => [sg] -> sg
reduceL []  = mempty -- error "reduce: cannot reduce empty list"
reduceL [x] = x
reduceL xs  = reduceL $ itr xs
    where
        itr :: (Monoid sg) => [sg] -> [sg]
        itr []            = []
        itr [x]           = [x]
        itr (x1:x2:xs)    = (x1<>x2):(itr xs)
