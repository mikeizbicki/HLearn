{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}

module HMine.Classifiers.DTree
    where

import Control.DeepSeq
import Control.Monad
import Data.Binary
import Data.List
import Data.List.Extras
import qualified Data.Map as Map
import Data.Semigroup
import Data.Number.LogFloat hiding (log)
import Debug.Trace

import HMine.Algebra
import HMine.Base
import HMine.Classifiers.TypeClasses
import HMine.Classifiers.Dirichlet
import HMine.DataContainers
import HMine.MiscUtils

-------------------------------------------------------------------------------
-- DTreeParams

data DTreeParams modelparams = DTreeParams
    { maxDepth :: Int
    , leafModelParams :: modelparams
    }
    deriving (Read,Show,Eq)

instance (NFData modelparams) => NFData (DTreeParams modelparams) where
    rnf params = deepseq (maxDepth params) (rnf $ leafModelParams params)

defDTreeParams = DTreeParams 4 DirichletParams
defDStumpParams = DTreeParams 1 DirichletParams

-------------------------------------------------------------------------------
-- DTree

type SplitPoint xs = (xs, DataItem, xs)

data DTree model label 
    = DLeaf model
    | DTree
        { dataDesc :: !(DataDesc label)
        , root :: !(Int,SplitPoint (DTree model label)) -- ^ (Column to split at, (modelLT, dp to split at, modelGT))
    --     , root :: !(DBranch label)
    --     , 
        }
    deriving (Read,Show,Eq)

instance (NFData label, NFData model) => NFData (DTree model label) where
    rnf (DTree dataDesc root) = deepseq dataDesc $ rnf root
    rnf (DLeaf model) = rnf model

-------------------------------------------------------------------------------
-- Training

instance (BatchTrainer modelparams model label) => BatchTrainer (DTreeParams modelparams) (DTree model label) label where
-- instance BatchTrainer DTreeParams (DTree Int) Int where
    
    trainBatch params ds = do
        splitLT <- splitmodel dsLT
        splitGT <- splitmodel dsGT
        return $ DTree
            { dataDesc = getDataDesc ds
            , root = (attrI, (splitLT, sp, splitGT))
    --         , root = DLeaf $ findSplitAttr $ getTransposeL ds
            }
        where
            (attrI,(lt,sp,gt)) = findSplitAttr $ getTransposeL ds
            (dsLT,dsGT) = ( filterds (\(l,dps) -> fetchAttr attrI dps <= sp) ds 
                          , filterds (\(l,dps) -> fetchAttr attrI dps >  sp) ds )
--             (dsLT,dsGT) = splitdsLDPS (attrI,sp) ds
            splitmodel ds' = if (maxDepth params > 1) && (getNumObs ds' > 2)
                then trainBatch (params { maxDepth = (maxDepth params)-1 }) ds'
                else fmap DLeaf $ trainBatch (leafModelParams params) ds'

--             (dsLT,dsGT) = splitdtree sp ds
{-            newroot = if (maxDepth params > 0)
                then (attrI, ( DTree $ 
                else (attrI, ( DLeaf $ trainBatch (leafModelParams params) dsLT
                             , sp
                             , DLeaf $ trainBatch (leafModelParams params) dsGT
                             ))-}
--             splitmodel ds' = fmap DLeaf $ trainBatch (leafModelParams params) ds'

findSplitAttr :: (Label label) => [[(label,DataItem)]] -> (Int,SplitPoint label)
findSplitAttr [] = error "findSplitAttr: should never be called with an empty list"
findSplitAttr xs = (index,fst sp)
    where 
        xs' = map findSplitPoint xs
        
        index = case elemIndex sp xs' of
                     Just i -> i
                     Nothing -> error "findSplitAttr: something horrible has happened."
                     
--         sp :: (Ord label) => (SplitPoint label,Double)
        sp = argmax snd xs'

findSplitPoint :: (Label label) => [(label,DataItem)] -> (SplitPoint label,Double)
findSplitPoint [] = error "findSplitPoint: empty list"
findSplitPoint xs = 
    case condensed of
         [] -> error "findSplitPoint: condensed empty"
         x:[] -> ((fst x,snd x,fst x),0) -- error "findSplitPoint: condensed 1 elem"
         otherwise -> split2splitpoint $ argmaxWithMax (infoGain xs) [[take i condensed, drop i condensed] | i<-[1..length condensed-1]]
    where
        -- | first we sort all the data points (by value, then label), then we will remove 
        -- datapoints that are "useless" in that they are surrounded by other datapoints of the
        -- same label.  No decision boundary will ever be between them.
        condensed = map last $ groupBy (\x1 x2 -> (fst x1)==(fst x2)) sorted
        sorted = sortBy (\x1 x2 -> (compare (snd x1) (snd x2)) `mappend` (compare (fst x1) (fst x2))) xs

        split2splitpoint :: (Ord label) => ([[(label,DataItem)]],Double) -> (SplitPoint label,Double)
        split2splitpoint ([ys1,ys2],score) = ((fst $ last ys1, snd $ last ys1, fst $ last ys2),score)

-------------------------------------------------------------------------------
-- split criteria

infoGain :: (Ord label) => [(label,DataItem)] -> [[(label,DataItem)]] -> Double
infoGain xs ys = {-trace ("xs="++show xs++"; ys="++show ys) $-} 
    infoGainHistogram (histogram $ map fst xs) (map (histogram . map fst) ys)

-- infoGain :: (Ord label) => [(label,DataItem)] -> [(label,DataItem)] -> Double
-- infoGain xs ys = (info xs) - (sum $ map weightedInfo ys)
--     where weightedInfo zs = (fromIntegral $ length zs)/(fromIntegral $ length xs)*(info zs)

infoGainHistogram :: (Ord label) => [(label,Int)] -> [[(label,Int)]] -> Double
infoGainHistogram xs ys = {-trace ("xs="++show xs++"; ys="++show ys) $-} (info $ map snd xs) - (sum $ map (weightedInfo . map snd) ys)
    where weightedInfo zs = (fromIntegral $ length zs)/(fromIntegral $ length xs)*(info zs)

info :: [Int] -> Double
info xs = sum $ map (\x -> -(x/tot)*(lg $ x/tot)) xs'
    where xs' = map (\x -> 0.000001+fromIntegral x) xs
          tot = sum xs'

lg :: Double -> Double -- base 2 logarithm
lg x = log x / log 2

histogram :: (Ord label) => [label] -> [(label,Int)]
histogram = Map.assocs . go Map.empty {-. map (\x -> (x,1))-}
    where
        go :: (Ord label) => Map.Map label Int -> [label] -> Map.Map label Int
        go !m ![]     = m
        go !m !(x:xs) = go (Map.insertWith (+) x 1 m) xs

-------------------------------------------------------------------------------
-- Classifying

instance (Classifier model label) => Classifier (DTree model label) label where
    classify (DLeaf model) dp = classify model dp
    classify (DTree _ (attrI,(modelLT,sp,modelGT))) dp = 
        case lookup attrI dp of
             Just x  -> if x <= sp
                then classify modelLT dp
                else classify modelGT dp
             Nothing -> error "DTree.classify: this should never happen."

instance (ProbabilityClassifier model label) => ProbabilityClassifier (DTree model label) label where
    probabilityClassify (DLeaf model) dp = probabilityClassify model dp
    probabilityClassify (DTree _ (attrI,(modelLT,sp,modelGT))) dp = 
        case lookup attrI dp of
             Just x  -> if x <= sp
                then probabilityClassify modelLT dp
                else probabilityClassify modelGT dp
             Nothing -> error "DTree.classify: this should never happen."
