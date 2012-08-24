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
import Data.Semigroup
import Data.Number.LogFloat hiding (log)
import Debug.Trace

import qualified Data.Foldable as F
import qualified Data.Map as Map

import HMine.Base
import HMine.Classifiers.Dirichlet
import HMine.DataContainers
import HMine.Math.Algebra
import HMine.Math.TypeClasses
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

-- type SplitPoint xs = (xs, DataItem, xs)
type SplitPoint xs = (xs, Ordering, DataItem)

data DTree model label 
    = DLeaf !model
    | DTree
        { dataDesc :: !(DataDesc label)
        , root :: !(Int,[SplitPoint (DTree model label)]) -- ^ (Column to split at, (modelLT, dp to split at, modelGT))
        }
    deriving (Read,Show,Eq)

instance (NFData label, NFData model) => NFData (DTree model label) where
    rnf (DTree dataDesc root) = deepseq dataDesc $ {-trace "Warning: Not deepseq-ing DTree" $ -}rnf ()
    rnf (DLeaf model) = rnf model

-------------------------------------------------------------------------------
-- Training

instance (BatchTrainer modelparams model label) => BatchTrainer (DTreeParams modelparams) (DTree model label) label where
-- instance BatchTrainer DTreeParams (DTree Int) Int where
    
    trainBatch params ds = do
        spL <- mapM (\(ord,di) -> do
            modelsplit <- calcmodel $ splitds ord di
            return (modelsplit,ord,di)
            ) xs
        return $ DTree
            { dataDesc = getDataDesc ds
            , root = (attrI,spL)
            }
        where
            (attrI,xs) = findSplitAttr $ getTransposeL ds
            splitds ord di = filterds (\(l,dps) -> comparedi (fetchAttr attrI dps) di) ds 
                where
                    comparedi Missing Missing = True
                    comparedi Missing _ = False
                    comparedi _ Missing = False
                    comparedi (Discrete x) (Discrete y) = x==y
                    comparedi (Continuous x) (Continuous y) = if ord==LT
                        then x<=y
                        else x>y
                
            calcmodel ds' = if (maxDepth params > 1) && (getNumObs ds' > 2)
                then trainBatch (params { maxDepth = (maxDepth params)-1 }) ds'
                else fmap DLeaf $ trainBatch (leafModelParams params) ds'

findSplitAttr :: (Label label) => [[(label,DataItem)]] -> (Int,[(Ordering,DataItem)])
findSplitAttr [] = error "findSplitAttr: should never be called with an empty list"
findSplitAttr xs = {-trace ("sp="++show sp++", xs'="++show xs') -}ret
    where 
        ret = (index,fst sp)
        xs' = map findSplitPoint xs        
        sp = {-head xs'---} argmax snd xs'
        index = case elemIndex sp xs' of
                     Just i -> i
                     Nothing -> error "findSplitAttr: something horrible has happened."

findSplitPoint :: (Label label) => [(label,DataItem)] -> ([(Ordering,DataItem)],Double)
findSplitPoint xs = case attrType xs of
    Continuous x -> findSplitPointC xs
    Discrete x -> findSplitPointD xs
    where
        attrType [] = Discrete ""
        attrType ((l,di):xs) = case di of
            Missing -> attrType xs
            otherwise -> di
    
findSplitPointD :: (Label label) => [(label,DataItem)] -> ([(Ordering,DataItem)],Double) -- (SplitPoint label,Double)
findSplitPointD [] = error "findSplitPointD: empty list"
findSplitPointD xs = (attrL,score)
    where
        attrL = map (\x -> (EQ,snd $ head x)) condensed
        score = infoGain xs condensed
        condensed = groupBy (\x1 x2 -> snd x1 == snd x2) xs
            
findSplitPointC :: (Label label) => [(label,DataItem)] -> ([(Ordering,DataItem)],Double) -- (SplitPoint label,Double)
findSplitPointC [] = error "findSplitPointC: empty list"
findSplitPointC xs = 
    case condensed of
         [] -> error "findSplitPoint: condensed empty"
         x:[] -> ([(LT,snd x),(GT,snd x)],0) -- error "findSplitPoint: condensed 1 elem"
         otherwise -> let tmp = [[take i condensed, drop i condensed] | i<-[1..length condensed-1]]
                          in split2splitpoint $ {-trace ("tmp="++(show $ map (\x -> (x,infoGain xs x)) tmp)) $ -}argmaxWithMax (infoGain xs)  tmp
    where
        -- | first we sort all the data points (by value, then label), then we will remove 
        -- datapoints that are "useless" in that they are surrounded by other datapoints of the
        -- same label.  No decision boundary will ever be between them.
        condensed = sorted -- map last $ groupBy (\x1 x2 -> (fst x1)==(fst x2)) sorted
        sorted = sortBy (\x1 x2 -> (compare (snd x1) (snd x2)) `mappend` (compare (fst x1) (fst x2))) xs

        split2splitpoint :: (Ord label) => ([[(label,DataItem)]],Double) -> ([(Ordering,DataItem)],Double)
        split2splitpoint ([ys1,ys2],score) = {-trace ("s2sp="++show ret) $ -}ret
            where ret=([(LT,snd $ last ys1),(GT,snd $ last ys1)],score)
--         split2splitpoint ([ys1,ys2],score) = ((fst $ last ys1, snd $ last ys1, fst $ last ys2),score)

-------------------------------------------------------------------------------
-- split criteria

infoGain :: (Ord label) => [(label,DataItem)] -> [[(label,DataItem)]] -> Double
infoGain xs ys = ret {-trace ("xs="++show xs++"; ys="++show ys) $-} 
    where
        ret = infoGainHistogram (histogram $ map fst xs) (map (histogram . map fst) ys)

-- infoGain :: (Ord label) => [(label,DataItem)] -> [(label,DataItem)] -> Double
-- infoGain xs ys = (info xs) - (sum $ map weightedInfo ys)
--     where weightedInfo zs = (fromIntegral $ length zs)/(fromIntegral $ length xs)*(info zs)

infoGainHistogram :: (Ord label) => [(label,Int)] -> [[(label,Int)]] -> Double
infoGainHistogram xs ys = {-trace ("infogain="++show ret++", left="++show (info $ map snd xs)++", right="++show (sum $ map (weightedInfo . map snd) ys)++", xs="++show xs++", ys="++show ys) -} 
                          ret
    where 
        ret = (info $ map snd xs) - (sum $ map (weightedInfo . map snd) ys)
        weightedInfo zs = (info zs)*(fromIntegral $ sum zs)/(fromIntegral $ sum $ map snd xs)
--         numitems = sum . map snd
--         weightedInfo zs = info zs

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

instance (ProbabilityClassifier leafmodel label) => Classifier (DTree leafmodel label) label where
    classify model dp = fst $ argmaxBy compare snd $ probabilityClassify model dp

-- instance (Classifier model label) => Classifier (DTree model label) label where
--     classify (DLeaf model) dp = classify model dp
--     classify (DTree _ (attrI,(modelLT,sp,modelGT))) dp = 
--         case lookup attrI dp of
--              Just x  -> if x <= sp
--                 then classify modelLT dp
--                 else classify modelGT dp
--              Nothing -> error "DTree.classify: this should never happen."
-- 
instance (ProbabilityClassifier model label) => ProbabilityClassifier (DTree model label) label where
    probabilityClassify (DLeaf model) dp = probabilityClassify model dp
    probabilityClassify (DTree desc (attrI,xs)) dp = go xs
        where
            go [] = Map.toList $ F.foldl' mappend Map.empty $ map (\(model,_,_) -> Map.fromList $ probabilityClassify model dp) xs
--             go [] = error ("DTree.classify: barf. dp="++show dp++", attrI="++show attrI++", xs="++show xs)
            go ((model,ord,di):xs) = if boolcond
                then {-trace "pC" $-} probabilityClassify model dp
                else {-trace "go" $-} go xs
                where
                    boolcond = case lookup attrI dp of
                        Just x -> case ord of
                            EQ -> {-trace "EQ" $-} x==di
                            LT -> {-trace ("LT, di="++show di++", x="++show x) $-} x<=di
                            GT -> {-trace "GT" $-} x>di
                        Nothing -> error "DTree.probabilityClassify: this should never happen."