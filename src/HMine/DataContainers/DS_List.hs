{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HMine.DataContainers.DS_List
    where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Data.Binary
import Data.List
import Data.Semigroup
import Debug.Trace
import Safe
import Test.QuickCheck

import qualified Data.Foldable as F
import qualified Data.Traversable as T

import HMine.DataContainers
import HMine.DataContainers.CSVParser
import HMine.MiscUtils
import HMine.RandUtils

-------------------------------------------------------------------------------
-- DS_List

data DS_List label dataType = DS_List
    { dsDesc :: DataDesc label
    , dsL :: [dataType]
    , dsLen :: Int
    }
    deriving (Read,Show)
    
instance (Eq label) => Semigroup (DS_List label dataType) where
    (<>) (DS_List dsDesc1 dsL1 dsLen1) (DS_List dsDesc2 dsL2 dsLen2) = 
        if (dsDesc1 /= dsDesc2)
           then error "DS_List.(<>): adding two elements of different dimensions"
           else DS_List dsDesc1 (dsL1++dsL2) (dsLen1+dsLen2)

-- arbitraryDS_List :: DataDesc -> Gen (DS_List Int (LDPS Int))
-- arbitraryDS_List desc = do
--     dsLen <- choose (100,200)
--     dsL <- vector dsLen
--     return $ DS_List 
--         { dsDesc = desc
--         , dsL = dsL
--         , dsLen = dsLen
--         , dsLabelL = [0..numLabels desc]
--         }

instance F.Foldable (DS_List label) where
    foldr f model0 ds = foldr f model0 (dsL ds)

instance Functor (DS_List label) where
    fmap f ds = ds { dsL = fmap f $ dsL ds }

instance T.Traversable (DS_List label) where
    traverse f ds = liftA (\dsL' -> ds { dsL = dsL' } ) $ T.traverse f (dsL ds)

instance (Ord label, Ord dataType, Show label, Show dataType) => DataSparse label (DS_List label) dataType where
    emptyds desc = DS_List
        { dsDesc = desc
        , dsL = []
        , dsLen = 0
        }

    getDataDesc ds = dsDesc ds
    getNumObs = dsLen
    getObsL ds = [0..(dsLen ds)-1]
--     getLabelL = dsLabelL
    getDataL = dsL

    sample num wds = do
        dsL' <- replicateM num (fromList $ fmap (\(x,w) -> (x,toRational w)) $ dsL wds)
        return $ wds { dsL = dsL', dsLen = num }

{-    zipds ds1 ds2 = DS_List { dsDesc = dsDesc, dsL = zipped, dsLen = length zipped }
        where 
            dsDesc = if dsDesc ds1 /= dsDesc ds2
                then error "DS_List.zipds: DataDesc not equal!"
                else dsDesc ds1
            zipped = zip (dsL ds1) (dsL ds2)-}
              
    zipdsL ds xs = ds { dsL = zipped, dsLen = length zipped }
        where zipped = zip (dsL ds) xs
              
    filterds cond ds = ds { dsL = dsL', dsLen = length dsL' }
        where dsL' = filter cond $ dsL ds
    
    splitdtree dt ds = ( ds { dsL = dsL1, dsLen = length dsL1 }
                       , ds { dsL = dsL2, dsLen = length dsL2 }
                       )
        where 
            dsL1 = filter (<=dt) (dsL ds)
            dsL2 = filter (> dt) (dsL ds)
    
    randSplit factor ds = do
        (dsL1,dsL2) <- randSplitL factor $ dsL ds
        let ds1 = ds
                { dsL = dsL1
                , dsLen = trace "DS_List.randSplit.ds1 WARNING: using slow length function" $ length dsL2
                }
        let ds2 = ds
                { dsL = dsL2
                , dsLen = trace "DS_List.randSplit.ds2 WARNING: using slow length function" $ length dsL2
                }
        return (ds1,ds2)
        
    takeFirst len ds = ds 
        { dsL = newL
        , dsLen = length newL
        }
        where
            newL = take len $ dsL ds 

    dropFirst len ds = ds 
        { dsL = drop len $ dsL ds 
        , dsLen = positive $ (dsLen ds)-len
        }
        where
            positive x = if x<0
                            then 0
                            else x

instance (NFData label, NFData dataType) => NFData (DS_List dataType label) where
    rnf ds = seq (rnf (dsDesc ds)) $ rnf (dsL ds)

instance (Binary label, Binary dataType) => Binary (DS_List dataType label) where
--     put lds = put (ldsDesc lds) >> put (Stream $ map (\(l,dp) -> (l,Stream dp)) $ ldsL lds) {->> put (ldLen lds)-}
    put ds = do
        put $ dsDesc ds
        put $ Stream $ dsL ds
        put $ dsLen ds
    get = liftM3 DS_List get (liftM unstream get) get

---------------------------------------
-- test LDS_List

test_numLabels = 97
test_lenList = 10000

inflds = DS_List
    { dsDesc = DataDesc test_numLabels [0..test_numLabels-1] 500000
    , dsL = [(mod i test_numLabels, [(j,Continuous $ fromIntegral j) | j<-[1..100]]) | i <- [1..test_lenList]]
    , dsLen = test_lenList
    }
    

-------------------------------------------------------------------------------
-- IO functions

dd2intdd :: (DataDesc label) -> (DataDesc Int)
dd2intdd desc = desc { labelL = [0..numLabels desc-1] }

ds2intds :: (Ord label, Show label) => (DS_List label (LDPS label)) -> (DS_List Int (LDPS Int))
ds2intds ds = DS_List
    { dsDesc = dd2intdd $ dsDesc ds
    , dsL = map (\(label,dp)->(getIndex label, dp)) $ dsL ds
    , dsLen = dsLen ds
    }
    where
        getIndex label = case (elemIndex label $ labelL $ getDataDesc ds) of
                              Nothing -> error "stringds2intds: something awful happend"
                              Just x -> x

instance DataLoaderCSV (DS_List String (LDPS String)) where
    loadDataCSV filedesc = do
        ret <- loadData filedesc
        return $
            case ret of
                Left msg -> error "$ show msg"
                Right x -> x

-- | Lazily loads a file into a TrainingData type for use with the classification algorithms
loadData :: DatafileDesc -> IO (Either ParseError (DS_List String (LDPS String)))
loadData filedesc = liftM (liftM $ csv2data filedesc) $ loadCSV $ datafileName filedesc

{-    do
    csveither <- loadCSV $ datafileName filedesc
    return $ liftM csv2data csveither
    return $ do
        csv <- csveither
        return csv2data-}
--         let dsL = map (\x -> (last x, list2datapoints $ init x)) csv
--         let labelL = extractLabelL dsL []
--         
--         return $ DS_List 
--             { dsDesc = DataDesc 
--                 { numLabels = length labelL
--                 , numAttr = length $ snd $ head dsL
--                 } 
--             , dsL = dsL
--             , dsLen = length dsL 
--             , dsLabelL = labelL
--             }

csv2data :: DatafileDesc -> [[String]] -> (DS_List String (LDPS String))
csv2data filedesc csv = DS_List 
    { dsDesc = DataDesc 
        { numLabels = length labelL
        , labelL = labelL
        , numAttr = length $ snd $ head dsL
        } 
    , dsL = dsL
    , dsLen = length dsL 
    }
    where
        dsL = map (\x -> (last x, list2datapoints (datafileMissingStr filedesc) $ init x)) csv
        labelL = extractLabelL dsL []

list2datapoints :: Maybe String -> [String] -> [(Int,DataItem)]
list2datapoints missingStr xs = mapi (\i x -> (i,format x)) xs
    where 
        format str = 
            case readMay str of
                 Just x  -> Continuous x
                 Nothing -> if Just str==missingStr
                               then Missing
                               else Discrete str

mapi :: (Int -> a -> b) -> [a] -> [b]
mapi = go 0
    where
        go itr f []     = []
        go itr f (x:xs) = (f itr x):(go (itr+1) f xs)


extractLabelL :: (Eq label) => [(label,dp)] -> [label] -> [label]
extractLabelL []              labelL = labelL
extractLabelL ((label,dp):xs) labelL = 
    if label `elem` labelL
        then extractLabelL xs labelL
        else extractLabelL xs (label:labelL)

---------------

-- csv2data :: Maybe String -> Maybe [String -> DataItem] -> [[String]] -> TrainingData String
-- csv2data missingStr Nothing   csv = csv2dataAuto missingStr csv
-- csv2data missingStr (Just fs) csv = error "forced value types not implemented"
-- -- csv2data missingStr (Just fs) csv = csv2dataForce missingStr fs csv
-- 
-- csv2dataAuto :: Maybe String -> [[String]] -> TrainingData String
-- csv2dataAuto missingStr csv = map (\dp -> (last dp, map cell2sql $ init dp)) csv
--     where 
--         cell2sql x = 
--             if Just x==missingStr
--                then Missing
--                else case (reads x::[(Double,String)]) of
--                         []     -> toDataItem (x::String)
--                         (x:xs) -> toDataItem $ fst x

-- csv2dataForce :: Maybe String -> [String -> DataItem] -> [[String]] -> TrainingData String
-- csv2dataForce missingStr fs csv = 
--     [(last line,
--         [ if Just cell==missingStr
--              then Missing
--              else f cell
--         | (f,cell) <- zip fs $ init line
--         ])
--     | line <- csv
--     ]
