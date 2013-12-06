{-# LANGUAGE TypeSynonymInstances,FlexibleInstances,DataKinds,TypeOperators #-}

import Control.Monad
import Control.Monad.Random
import Control.DeepSeq
import Data.Semigroup

import HLearn.Algebra
import HLearn.DataStructures.CoverTree
import HLearn.DataStructures.SpaceTree
import HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor
import HLearn.DataStructures.SpaceTree.Algorithms.RangeSearch
import HLearn.DataStructures.SpaceTree.DualTreeMonoids

main = do
--     xs <- replicateM 100000 $ do
    xs <- replicateM 1000 $ do
        x <- randomRIO (-2^5,2^5)
        y <- randomRIO (-2^5,2^5)
        return $ (x,y)
    deepseq xs $ print "random done."
--     let m=parallel train xs :: TreeAlg (KNN2 1) CoverTree (Double,Double)
--     let m=parallel train xs :: TreeAlg (RangeSearch2 (0%1) (1%1)) CoverTree (Double,Double)
    let m=parallel train xs :: CoverTree (Double,Double)
--     let m=insertBatch xs :: CoverTree' (Double,Double)
    deepseq m $ print "deepseq m"

    let res=knn2_slow (DualTree m m) :: KNN2 1 (Double,Double)
--     let res=rangesearch (DualTree m m) :: RangeSearch2 (0%1) (1%1) (Double,Double)
    deepseq res $ print "deepseq res"
