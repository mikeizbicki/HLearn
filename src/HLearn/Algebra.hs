-- | This is the base module for the HLearn library.  It exports all the functions / data structures needed.

module HLearn.Algebra
    ( module GHC.TypeLits
    , module Data.Proxy

    , module HLearn.Algebra.Common
    , module HLearn.Algebra.Functions
    , module HLearn.Algebra.Types.Frac
    , module HLearn.Algebra.Models.HomTrainer
    , module HLearn.Algebra.Models.Lame
    , module HLearn.Algebra.Structures.CanError
    , module HLearn.Algebra.Structures.Groups
    , module HLearn.Algebra.Structures.MetricSpace
    , module HLearn.Algebra.Structures.Modules
    , module HLearn.Algebra.Structures.Comonoid
    , module HLearn.Algebra.Structures.Topology
    , module HLearn.Algebra.Structures.Free.AddUnit
    )
    where
          
import GHC.TypeLits
import Data.Proxy

import HLearn.Algebra.Common
import HLearn.Algebra.Functions
import HLearn.Algebra.Types.Frac
import HLearn.Algebra.Models.HomTrainer
import HLearn.Algebra.Models.Lame
import HLearn.Algebra.Structures.CanError
import HLearn.Algebra.Structures.Groups
import HLearn.Algebra.Structures.MetricSpace
import HLearn.Algebra.Structures.Modules
import HLearn.Algebra.Structures.Comonoid
import HLearn.Algebra.Structures.Topology
import HLearn.Algebra.Structures.Free.AddUnit
