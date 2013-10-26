module HLearn.Metrics.RandomProjection
    where

import Control.Monad
import Control.Monad.Random
import qualified Data.Vector.Unboxed as VU
import System.Random

import HLearn.Algebra

-------------------------------------------------------------------------------
-- data types

data RandomProjection (seed::Nat) (n::Nat) dp = RandomProjection 
    { weights :: !(VU.Vector (Ring dp)) 
    , rawdp :: !dp
    }

-- basisVector :: RandomGen g => Int -> Rand g (VU.Vector r)
basisVector d = liftM (VU.fromList) $ replicateM d $ getRandomR (-1,1)
         

-------------------------------------------------------------------------------
-- algebra

