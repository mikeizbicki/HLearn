{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{- | K-Nearest Neighbor is a simple but slow classification algorithm that can be used as a base learner for boosting algorithms.  Wikipedia: <http://en.wikipedia.org/wiki/K-nearest_neighbor_algorithm>

-}

module HMine.Classifiers.KNN
    ( KNN (..)
    , KNNParams (..)
    , defKNNParams
    )
    where

import Control.DeepSeq
import Data.List
import Data.List.Extras
import Data.Trees.KdTree

import HMine.Base
import HMine.DataContainers
import HMine.Math.TypeClasses

-------------------------------------------------------------------------------
-- KNNParams

data KNNParams = KNNParams
    { k :: Int
    }
    deriving (Read,Show,Eq)

instance NFData KNNParams where
    rnf (KNNParams k) = rnf k

defKNNParams = KNNParams 1

instance Point (LDPS label) where
    dimension (l,p) = length p
    
    coord n (l,p) = 
        case lookup n p of
             Just (Continuous x)    -> x
             otherwise              -> 0
    
--     dist2 pt1@(l1,dp1) (l2,dp2) = sum . map diff2 $ [0..dimension pt1 - 1]
--         where 
--             diff2 i = (coordsub (dp1!!i) (dp2!!i))^2
--               
--             coordsub (Discrete a) (Discrete b) = 
--                 if a==b
--                    then 0
--                    else 1
--             coordsub (Continuous a) (Continuous b) = a-b
--             coordsub _ Missing = 0
--             coordsub Missing _ = 0

-------------------------------------------------------------------------------
-- KNN

data KNN label = KNN 
    { kdtree :: KdTree (LDPS label)
    , kddesc :: DataDesc label
    , knnparams :: KNNParams
    }
    deriving Show

instance NFData (KNN label) where
    rnf knn = () -- ^ FIXME: No deepseq instance for kd-tree

-------------------------------------------------------------------------------
-- Training

instance (Label label) => BatchTrainer KNNParams (KNN label) DPS label where

--     trainBatch :: (DataSparse label ds (WDPS label),DataSparse label ds (LDPS label)) =>
--         modelparams -> ds (LDPS label) -> HMine model
    trainBatch modelparams ds = return $ KNN
        { kdtree = fromList $ getDataL ds
        , kddesc = getDataDesc ds
        , knnparams = modelparams
        }

instance (Label label) => EmptyTrainer KNNParams (KNN label) label where
    emptyModel desc modelparams = KNN
        { kdtree = fromList []
        , kddesc = desc
        , knnparams = modelparams
        }


instance (Label label) => OnlineTrainer KNNParams (KNN label) DPS label where
    add1dp desc modelparams model dps = return $ model
        { kdtree = fromList $ dps:(toList $ kdtree model)
        }

-------------------------------------------------------------------------------
-- Classification

instance (Label label) => Classifier (KNN label) DPS label where
    classify model dp = fst $ argmaxBy compare snd $ probabilityClassify model dp

instance (Label label) => ProbabilityClassifier (KNN label) DPS label where -- must use the Int version until DataDesc holds labelL

    probabilityClassify knn dp = addMissing $ map reduce $ groupBy sameLabel neighbors
        where
            reduce xs = (fst $ head xs,(fromIntegral $ length xs)/(fromIntegral $ k $ knnparams knn))
            sameLabel (l1,d1) (l2,d2) = l1==l2
            neighbors = kNearestNeighbors (kdtree knn) (k $ knnparams knn) (undefined,dp)

            addMissing xs = [ case lookup i xs of
                                 Just x ->(i,x)
                                 Nothing->(i,0)
                            | i<-labelL $ kddesc knn
                            ]