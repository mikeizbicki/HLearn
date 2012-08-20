module AlgebraTest
    where

import Numeric.Algebra
import Prelude hiding ((+),(-),(/),(*))
import qualified Prelude

instance Division Double where
    (/) a b = (Prelude./) a b

test = (1::Double) / (2::Double)