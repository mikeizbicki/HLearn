{-# LANGUAGE TypeSynonymInstances,FlexibleInstances,DataKinds #-}

import Control.Monad
import Control.Monad.Random
import Control.DeepSeq
import Data.Semigroup

import HLearn.Algebra
import HLearn.DataStructures.CoverTree
import HLearn.DataStructures.SpaceTree
import HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor

main = do
--     xs <- replicateM 100000 $ do
    xs <- replicateM 2000 $ do
        x <- randomRIO (-2^5,2^5)
        y <- randomRIO (-2^5,2^5)
        return $ (x,y)
    deepseq xs $ print "random done."
    let m=parallel train xs :: CoverTree (Double,Double)
--     let m=insertBatch xs :: CoverTree' (Double,Double)
    deepseq m $ print "deepseq m"

--     let res=knn2 (DualTree m m) :: KNN2 1 (Double,Double)
    let res=cover_knn2' m m :: KNN2 1 (Double,Double)
    deepseq res $ print "deepseq res"
