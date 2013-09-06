{-# LANGUAGE TypeSynonymInstances,FlexibleInstances,DataKinds #-}

import Control.Monad
import Control.Monad.Random
import Control.DeepSeq
import Data.Semigroup

import HLearn.Algebra
import HLearn.DataStructures.CoverTree
import HLearn.DataStructures.SpaceTree

main = do
--     xs <- replicateM 100000 $ do
    xs <- replicateM 10000 $ do
        x <- randomRIO (-2^5,2^5)
        y <- randomRIO (-2^5,2^5)
        return $ (x,y)
    deepseq xs $ print "random done."
    let m=parallel train xs :: CoverTree (Double,Double)
--     let m=insertBatch xs :: CoverTree' (Double,Double)
    seq m $ print "seq"
    let res=knn (0,0) m :: KNN 10 (Double,Double)
    print res
