{-# LANGUAGE TypeSynonymInstances,FlexibleInstances,TypeFamilies,BangPatterns #-}

import Control.Monad
import Control.Monad.Random
import Control.DeepSeq
import Data.Semigroup

import HLearn.Algebra
import HLearn.DataStructures.CoverTree

instance Num a => HasRing (a,a,a,a) where
    type Ring (a,a,a,a) = a

instance (RealFrac a, Floating a) => MetricSpace (a,a,a,a) where
    distance (a1,a2,a3,a4) (b1,b2,b3,b4) = sqrt $ (a1-b1)*(a1-b1)+(a2-b2)*(a2-b2)+(a3-b3)*(a3-b3)+(a4-b4)*(a4-b4)

    distanceFastMono (a1,a2,a3,a4) (b1,b2,b3,b4) = (abs$a1-b1)+(abs$a2-b2)+(abs$a3-b3)+(abs$a4-b4)

    {-# INLINABLE distanceFastBound #-}
    distanceFastBound !(a1,a2,a3,a4) !(b1,b2,b3,b4) !bound = 
        if pt1 > threshold
        || pt2 > threshold
        || pt3 > threshold
            then threshold+1
            else pt4
                                                         
        where
            threshold = bound*bound
            pt1=(a1-b1)*(a1-b1)
            pt2=(a2-b2)*(a2-b2)+pt1
            pt3=(a3-b3)*(a3-b3)+pt2
            pt4=(a4-b4)*(a4-b4)+pt3

-- instance Num a => HasRing [a] where
--     type Ring [a] = a
-- 
-- instance (RealFrac a, Floating a) => MetricSpace [a] where
--     distance xs ys = sqrt $ sum $ zipWith (\x y -> (x-y)*(x-y)) xs ys

main = do
--     xs <- replicateM 100000 $ do
    xs <- replicateM 100000 $ do
        x1 <- randomRIO (-2^50,2^50)
        x2 <- randomRIO (-2^50,2^50)
        x3 <- randomRIO (-2^50,2^50)
        x4 <- randomRIO (-2^50,2^50)
        return $ (x1,x2,x3,x4)
--         replicateM 2 $ randomRIO (-2^5,2^5)
--         return (x1,x2)
    deepseq xs $ print "random done."
--     let m=parallel train xs :: CoverTree [Double]
--     let m=parallel train xs :: CoverTree (Float,Float)
--     let m=parallel train xs :: CoverTree (Float,Float,Float,Float)
    let m=parallel train xs :: CoverTree (Double,Double,Double,Double) 

    seq m $ print "done"
