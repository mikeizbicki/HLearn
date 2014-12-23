module HLearn.Algebra.Structures.Comonoid
    where

import qualified Data.Vector.Generic as VG
import qualified Prelude as P

import SubHask
import SubHask.Compatibility.Vector

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
    partition n xs = [map snd $ P.filter (\(i,x)->i `mod` n==j) ixs | j<-[0..n-1]]
        where
            ixs = addIndex 0 xs
            addIndex i [] = []
            addIndex i (x:xs) = (i,x):(addIndex (i+1) xs)

instance VG.Vector v a => NonCocommutative (ArrayT v a)
instance VG.Vector v a => Comonoid (ArrayT v a) where
    partition n vec = go 0
        where
            go i = if i>=VG.length vec
                then []
                else (VG.slice i len vec):(go $ i+lenmax)
                where
                    len = if i+lenmax >= VG.length vec
                        then (VG.length vec)-i
                        else lenmax
                    lenmax = ceiling $ (fromIntegral $ VG.length vec :: Double) / (fromIntegral n)
