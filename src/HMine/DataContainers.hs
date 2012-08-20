{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, FlexibleContexts #-}

module HMine.DataContainers
    where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Data.Binary
import Data.Functor
import Data.Hashable
import Data.List
import Data.Semigroup
import Test.QuickCheck

import qualified Data.Foldable as F
import qualified Data.Traversable as T

import HMine.Base
import HMine.MiscUtils

-------------------------------------------------------------------------------
-- DataItem

data DataItem = Missing
              | Continuous !Double
              | Discrete !String
    deriving (Read,Show,Eq,Ord)

fromContinuous :: DataItem -> Double
fromContinuous (Continuous x) = x

instance Hashable DataItem where
    hash Missing = 0
    hash (Discrete x) = 1 `combine` (hash x)
    hash (Continuous x) = 2 `combine` (hash x)

instance NFData DataItem where
    rnf Missing = ()
    rnf (Discrete x) = rnf x
    rnf (Continuous x) = rnf x
    
instance Binary DataItem where
    put x = case x of
                 Missing -> put (0::Word8)
                 Discrete y -> put (1::Word8) >> put y
                 Continuous y -> put (2::Word8) >> put y
    get = do
        i <- get
        case i::Word8 of
            0 -> return Missing
            1 -> liftM Discrete get
            2 -> liftM Continuous get

-------------------------------------------------------------------------------
-- DataDesc

data DataDesc label = DataDesc
    { numLabels :: Int
    , labelL :: [label]
    , numAttr :: Int
    }
    deriving (Eq,Read,Show)

-- intLabelL :: DataDesc label -> [Int]
-- intLabelL desc = [0..numLabels desc-1]

instance (Binary label) => Binary (DataDesc label) where
    put desc = do
        put $ numLabels desc
        put $ labelL desc
        put $ numAttr desc
    get = liftM3 DataDesc get get get

instance (NFData label) => NFData (DataDesc label) where
    rnf desc = deepseq (deepseq (labelL desc) (rnf $ numLabels desc)) (rnf $ numAttr desc)

instance Arbitrary (DataDesc Int) where
    arbitrary = do
        labels <- choose (2,50)
        attrs <- choose (1,50)
        return $ DataDesc labels [0..labels-1] attrs

-------------------------------------------------------------------------------
-- DataLoaderCSV

class DataLoaderCSV dl where
    loadDataCSV :: DatafileDesc -> IO ({-Either String -}dl)

-- | Describes the data file to be loaded
data DatafileDesc = DatafileDesc
    { datafileName :: String -- ^ The path of the file to open 
    , datafileTrueClass :: Maybe String -- ^ Currently, in order to use the binary classification algorithms you must specify which class label should be considered as True, and all the rest are considered False.
    , datafileMissingStr :: Maybe String -- ^ This field specifies what symbol is used to represent missing data if present.  "?" is very common.
--     , datafileForce :: Maybe [String -> DataItem]-- ^ If this is set to Nothing, then text fields will automatically be Discrete and numeric fields automatically be Continuous.  Occasionally, however, you want numeric fields to be discrete, so you would have to set this.
    }
    deriving Show
        
-- instance Show (String->DataItem) where
--     show xs = ""

-------------------------------------------------------------------------------
-- DataContainers

type DPS         = [(Int,DataItem)] -- ^ DPS = DataPointSparse
type UDPS label  = DPS -- ^ UDPS = Unabeled DataPointSparse
type LDPS label  = (label,DPS) -- ^ LDPS = Labeled DataPointSparse
type WLDPS label = (Weighted (LDPS label)) -- ^ WDPS = Weighted labeled DataPointSparse
type WUDPS label = (Weighted (UDPS label)) -- ^ WDPS = Weighted labeled DataPointSparse

type Weighted var = (var,Double)

fetchAttr :: Int -> DPS -> DataItem
fetchAttr attrI dps = 
    case lookup attrI dps of
        Nothing -> Missing
        Just x  -> x

-------------------------------------------------------------------------------
-- DataSparse
class
    ( F.Foldable ds
    , Functor ds
    , T.Traversable ds
    , Show label
    , Show dataType
    , Show (ds dataType)
    , Ord label
    , Ord dataType
    , Semigroup (ds dataType)
    ) => 
    DataSparse label ds dataType | ds -> label 
        where
    
    emptyds :: DataDesc label -> ds dataType
    
    getDataDesc :: ds dataType -> DataDesc label
    getNumObs :: ds dataType -> Int
    getObsL :: ds dataType -> [Int]
    getDataL :: ds dataType -> [dataType]
--     getNumLabels :: ds dataType -> Int
--     getNumLabels = length . getLabelL
--     getLabelL :: ds dataType -> [label]

    filterds :: (dataType -> Bool) -> ds (dataType) -> ds (dataType)
    splitdtree :: dataType -> ds (dataType) -> (ds dataType, ds dataType)
    sample :: Int -> ds (Weighted dataType) -> HMine (ds dataType)
--     zipds :: ds dataType -> ds dataType2 -> ds (dataType,dataType2)
    zipdsL :: ds dataType -> [w] -> ds (dataType,w)
    randSplit :: (RandomGen g) => Double -> ds dataType -> Rand g (ds dataType, ds dataType)
    takeFirst :: Int -> ds dataType -> ds dataType
    dropFirst :: Int -> ds dataType -> ds dataType

-- newtype LDPS_Compare label = LDPS_Compare (LDPS label)
-- 
-- splitdsLDPS :: (DataSparse label ds (LDPS label)) => (Int,DataItem) -> ds (LDPS label) -> (ds (LDPS label),ds(LDPS label))
-- splitdsLDPS = undefined

getTransposeL :: (DataSparse label ds (LDPS label)) => ds (LDPS label) -> [[(label,DataItem)]]
getTransposeL ds = map (zip (map fst dsL)) . map (map snd) . transpose . map (fillL (numAttr $ getDataDesc ds)) $ map snd dsL
    where 
        dsL = getDataL ds

fillL :: Int -> DPS -> DPS
fillL limit = go 0
    where
        go itr []
            | itr < limit   = (itr,Missing):(go (itr+1) [])
            | otherwise     = []
        go itr (x:xs)
            | fst x==itr    = x:(go (itr+1) xs)
            | otherwise     = (itr,Missing):(go (itr+1) (x:xs))

lds2uds :: 
    ( DataSparse label ds (LDPS label)
    , DataSparse label ds (UDPS label)
    ) => ds (LDPS label) -> ds (UDPS label)
lds2uds lds = fmap snd lds
    
-- splitds :: (DataSparse label ds dataType) => Int -> ds dataType -> [ds dataType]
splitds :: (DataSparse label ds (LDPS label)) => Int -> ds (LDPS label) -> [ds (LDPS label)]
splitds len ds = (takeFirst len ds):nextL
    where 
        nextL=if getNumObs ds' == 0
                then []
                else splitds len ds'
        ds'=dropFirst len ds
    
-- class LabeledDataSparse lds label | lds -> label where    
--     trainOnline :: {-(Monoid model) => -}(model -> LabeledDataPointSparse label -> model) -> model -> lds -> model
--     trainOnlineM :: (Monad m) => (model -> LabeledDataPointSparse label -> m model) -> model -> lds -> m model
-- --     trainOnline :: (Monoid model) => (LabeledDataPointSparse label -> model) -> lds -> model
-- 
-- class UnlabeledDataSparse uds label | uds -> label where
   