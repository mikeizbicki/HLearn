module HLearn.Algebra.Structures.SigmaAlgebra
    where

import GHC.Prim
import HLearn.Algebra.Structures.Modules

-------------------------------------------------------------------------------
--

class SigmaAlgebra m where
    type SigmaConstraint m a :: Constraint
    type SigmaConstraint m a = ()

    complement :: m a -> m a
    union :: m a -> m a -> m a


class SigmaAlgebra m => Measure m where
    measure :: m a -> Ring (m a)
