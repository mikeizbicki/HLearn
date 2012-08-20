{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HMine.DataContainers.DS_Vector
    where

import Control.DeepSeq
import Control.Monad
import Data.Binary
import Data.List

import qualified Data.Vector as V
import qualified Data.Foldable as F

import HMine.DataContainers
import HMine.DataContainers.CSVParser
import HMine.MiscUtils
import HMine.RandUtils

-------------------------------------------------------------------------------
-- DS_Vector

data DS_Vector label dataType = DS_Vector
    { dsV :: V.Vector dataType
    }    
    deriving (Read,Show)

instance F.Foldable (DS_Vector label) where
    foldr f model0 ds = V.foldr f model0 (dsV ds)

instance Functor (DS_Vector label) where
    fmap f ds = ds { dsV = fmap f $ dsV ds }

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
