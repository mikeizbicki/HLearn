module HLearn.Algebra.Structures.Comonoid
    where

import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

-------------------------------------------------------------------------------
-- classes

class Comonoid t where
    partition :: Int -> t -> [t]

class Comonoid t => Cocommutative t
class Comonoid t => NonCocommutative t

-------------------------------------------------------------------------------
-- instances

instance Cocommutative [a]
instance Comonoid [a] where
    partition n xs = [map snd $ filter (\(i,x)->i `mod` n==j) ixs | j<-[0..n-1]]
        where
            ixs = addIndex 0 xs
            addIndex i [] = []
            addIndex i (x:xs) = (i,x):(addIndex (i+1) xs)

instance NonCocommutative (V.Vector a)
instance Comonoid (V.Vector a) where
    partition n vec = go 0
        where
            go i = if i>=V.length vec
                then []
                else (V.slice i len vec):(go $ i+lenmax)
                where
                    len = if i+lenmax >= V.length vec
                        then (V.length vec)-i
                        else lenmax
                    lenmax = ceiling $ (fromIntegral $ V.length vec) / (fromIntegral n)

instance VU.Unbox a => NonCocommutative (VU.Vector a)
instance VU.Unbox a => Comonoid (VU.Vector a) where
    partition n vec = go 0
        where
            go i = if i>=VU.length vec
                then []
                else (VU.slice i len vec):(go $ i+lenmax)
                where
                    len = if i+lenmax >= VU.length vec
                        then (VU.length vec)-i
                        else lenmax
                    lenmax = ceiling $ (fromIntegral $ VU.length vec) / (fromIntegral n)
