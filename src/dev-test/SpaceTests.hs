import HLearn.Algebra
import HLearn.Models.Distributions

import Control.Monad
import Control.Monad.ST
import Data.List
import Data.STRef
import Data.Number.LogFloat
import qualified Control.ConstraintKinds as CK

import Statistics.Distribution
import Statistics.Distribution.Normal

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as G
import qualified Data.Foldable as F


import qualified Data.Vector as V
import qualified Data.Foldable as F
size=10^7
main = do
    print $ (foldl' mappend mempty $ map train1dp [(0::Double)..size] :: Gaussian Double)
    putStrLn "done."
--     print $ (train (concat $ replicate (10^6) ["a","b"]) :: Categorical String Double)
--     print $ (train [1..fromIntegral size::Double] :: Gaussian Double)
--     print $ (train (VU.enumFromN (0::Double) size) :: Gaussian Double)
--     print $ normalFromSample $ VU.enumFromN 0 size
