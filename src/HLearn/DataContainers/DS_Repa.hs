{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HLearn.DataContainers.DS_Repa
    where

import Control.DeepSeq
import Control.Monad
import Data.Binary
import Data.List
import Data.Maybe

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Vector as R
import qualified Data.Array.Repa.Repr.ForeignPtr as R
import qualified Data.Array.Repa.Repr.ByteString as R
import qualified Data.Foldable as F

import HLearn.DataContainers
import HLearn.DataContainers.CSVParser
import HLearn.MiscUtils
import HLearn.RandUtils

-------------------------------------------------------------------------------
-- DS_Repa

data DS_Repa label dataType = DS_Repa
    { dsR :: R.Array R.V R.DIM1 dataType
    }
--     deriving (Read,Show)

-- instance F.Foldable (DS_Repa label) where
--     foldr f model0 ds = foldr f model0 (dsL ds)

instance Functor (DS_Repa label) where
    fmap f ds = ds { dsR = fromJust $ R.computeP $ R.map f $ dsR ds }

-- instance (Ord label) => DataSparse label (DS_Repa label) dataType where
--     getDataDesc ds = dsDesc ds
--     getNumObs = dsLen
--     getObsL ds = [0..(dsLen ds)-1]
--     getLabelL = dsLabelL
--     getDataL = dsL
--     
--     randSplit factor ds = do
--         (dsL1,dsL2) <- randSplitL factor $ dsL ds
--         let ds1 = ds
--                 { dsL = dsL1
--                 , dsLen = undefined
--                 }
--         let ds2 = ds
--                 { dsL = dsL2
--                 , dsLen = undefined
--                 }
--         return (ds1,ds2)
--         
--     takeFirst len ds = ds { dsL = take len $ dsL ds }

-- instance (NFData label, NFData dataType) => NFData (DS_Repa dataType label) where
--     rnf ds = seq (rnf (dsDesc ds)) $ rnf (dsL ds)

-- instance (Binary label, Binary dataType) => Binary (DS_Repa dataType label) where
--     put ds = do
--         put $ dsDesc ds
--         put $ Stream $ dsL ds
--         put $ dsLen ds
--         put $ dsLabelL ds
--     get = liftM4 DS_List get (liftM unstream get) get get
