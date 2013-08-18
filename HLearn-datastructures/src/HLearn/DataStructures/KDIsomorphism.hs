{-# LANGUAGE TemplateHaskell #-}
module HLearn.DataStructures.KDIsomorphism
    where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Control.Monad.ST
import Data.Bits
import Data.Bits.Extras
import Data.Primitive.ByteArray
import GHC.Word
import Unsafe.Coerce
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Generic as VG
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Mutable as VM
import qualified Data.Foldable as F
import Test.QuickCheck hiding ((.&.),(.|.),vector)
import Test.HUnit
import Debug.Trace


import qualified Control.ConstraintKinds as CK
-- import Data.Prunable
import HLearn.Algebra
import HLearn.DataStructures.SortedVector
-- import Data.BitTwiddler
import HLearn.DataStructures.BitTwiddlerSort.BitTwiddler
import HLearn.DataStructures.FourD

-------------------------------------------------------------------------------
-- data types

newtype KDVector a = KDVector (SortedVector' VS.Vector TimSort BitTwiddler4)
    deriving (Eq,Ord,Monoid,Abelian)

instance Show (KDVector FourD) where
    show (KDVector (SortedVector' v)) = concatMap show $ VS.toList v

instance NFData (KDVector a) where
    rnf (KDVector sv) = rnf sv

-------------------------------------------------------------------------------
-- algebra

instance CK.Foldable KDVector where
    type FoldableConstraint KDVector a = (Ord a, ToBitTwiddler a, BitTwiddler a ~ BitTwiddler4, VS.Storable a)
    foldr f b (KDVector (SortedVector' v)) = VS.foldr f b $ VS.map fromBitTwiddler v
    foldr1 f (KDVector (SortedVector' v)) = VS.foldr1 f $ VS.map fromBitTwiddler v
    foldl = undefined
    foldl' = undefined
    foldl1 = undefined

instance (ToBitTwiddler a, BitTwiddler a ~ BitTwiddler4) => HomTrainer (KDVector a) where
    type Datapoint (KDVector a) = a
    train1dp = KDVector . train1dp . toBitTwiddler 
    train = KDVector . train . fmap toBitTwiddler

-------------------------------------------------------------------------------
-- search

testcase1 = 1 @=? boundDistance (FourD 0 0 0 0) (Bound (FourD 0 0 1 1) (FourD 0 0 1 (-1))) 
testcase2 = 2 @=? boundDistance (FourD 0 0 0 0) (Bound (FourD 1 2 1 2) (FourD 2 1 2 1))

boundDistance :: FourD -> Bound FourD -> Double
boundDistance query bound = distance query $ FourD
    { _d0 = dim2val _d0
    , _d1 = dim2val _d1
    , _d2 = dim2val _d2
    , _d3 = dim2val _d3
    }
    where    
        dim2val :: (FourD -> Double) -> Double
        dim2val dim = if q > l && q < u
            then q
            else if abs (u-q) > abs (l-q)
                then l
                else u
            where
                q = dim query 
                u = dim $ upper bound
                l = dim $ lower bound

data NNRet = NNRet
    { nnret_dist :: Double
    }

nn :: NNRet -> FourD -> KDVector FourD -> NNRet
nn ret query kv@(KDVector (SortedVector' v)) = if VG.length v == 0
    then ret
    else if VG.length v == 1 || boundDistance query (vec2bound kv) < nnret_dist ret
        then ret'
        else if (toBitTwiddler query) < midval_BitTwiddler
             then undefined 
             else undefined
    where
        midpt = floor $ (fromIntegral $ VG.length v :: Float)/2
        midval_BitTwiddler = v VG.! midpt
        midval_FourD = fromBitTwiddler midval_BitTwiddler

        dist' = min (nnret_dist ret) (distance query midval_FourD)
        ret' = ret
            { nnret_dist = dist'
            }

vec2bound :: KDVector FourD -> Bound FourD
vec2bound (KDVector (SortedVector' v)) = Bound
    { upper = fromBitTwiddler $ v VG.! (VG.length v-1)
    , lower = fromBitTwiddler $ v VG.! 0
    }
--         dist' = min (nnret_dist ret) (distance query midval)


mindist query = CK.foldr1 (mindist_cata query)

mindist_cata :: FourD -> FourD -> FourD -> FourD
mindist_cata query a b = if distance query a < distance query b
    then a
    else b

mindist_cata2 :: FourD -> FourD -> (Double,FourD) -> (Double,FourD)
mindist_cata2 query newdp (dist,dp) = if newdist < dist
    then (newdist,newdp)
    else (dist,dp)
    where
        newdist = distance query dp

data Hylo = Hylo
    { bound :: !(Bound BitTwiddler4)
    , dist  :: !(Double)
    , dp    :: !(FourD)

    , query_FourD :: !FourD
    , query_BitTwiddler4 :: !BitTwiddler4
--     , misc :: a
    }
    deriving Show

inf = 1/0
emptyhylo query = Hylo
    { bound = Bound
        { upper = toBitTwiddler $ FourD inf inf inf inf
        , lower = toBitTwiddler $ FourD (-inf) (-inf) (-inf) (-inf)
        }
    , dist = inf
    , dp = FourD inf inf inf inf
    , query_FourD = query
    , query_BitTwiddler4 = toBitTwiddler query
    }

prunefoldr :: Hylo -> KDVector FourD -> Hylo
prunefoldr hylo kv@(KDVector sv@(SortedVector' v)) 
    | VG.length v == 0 = hylo
    | otherwise = prunefoldr (prunefoldr hylo' kvl) kvr

    where
        rightleft = prunefoldr (prunefoldr hylo' kvr) kvl
        leftright = prunefoldr (prunefoldr hylo' kvl) kvr

        midpt = VG.length v `intdiv` 2
        midval_BitTwiddler4 = v VG.! midpt :: BitTwiddler4
        midval_FourD = fromBitTwiddler midval_BitTwiddler4 :: FourD
        midval_dist = distance (query_FourD hylo) midval_FourD

        kvr = KDVector (SortedVector' $ VG.slice midpt (VG.length v-midpt-1) v)
        kvl = KDVector (SortedVector' $ VG.slice 0 midpt v) 

        hylo' = if midval_dist > dist hylo
            then hylo
            else hylo
                { dist = midval_dist
                , dp = midval_FourD
                , bound = calcBound_BitTwiddler4 (midval_dist,midval_FourD)
                }

intdiv :: Int -> Int -> Int
intdiv a b = floor $ (fromIntegral a / fromIntegral b :: Float)

-- maxvisit :: Bound BitTwiddler4 -> KDVector FourD -> Int
-- maxvisit bound (KDVector (SortedVector' v)) = VG.foldr1 (\x -> if x then 1; else 0) $ VG.map (\x -> x>lower bound && x<upper bound) v

-- maxvisit bound (KDVector (SortedVector' v)) = VG.map (\x -> x>=lower bound && x<=upper bound) v

minvisit :: Bound BitTwiddler4 -> KDVector FourD -> Int
minvisit bound (KDVector (SortedVector' v)) = length . filter (\x->x>=lower bound&&x<=upper bound) $ VG.toList v

minvisit_theory :: Bound BitTwiddler4 -> KDVector FourD -> Int
minvisit_theory bound (KDVector (SortedVector' v)) = length . filter theory $ VG.toList v
    where
        theory x = _d0 fd < _d0 hi && _d0 fd > _d0 lo
                && _d1 fd < _d1 hi && _d1 fd > _d1 lo
                && _d2 fd < _d2 hi && _d2 fd > _d2 lo
                && _d3 fd < _d3 hi && _d3 fd > _d3 lo
            where
                fd = fromBitTwiddler x :: FourD

        hi = fromBitTwiddler $ upper bound :: FourD
        lo = fromBitTwiddler $ lower bound :: FourD

---------------------------------------
-- test

offset=0
tdata = map (\x -> FourD x x x x) [offset..offset+1000]
tquery = FourD (offset+200) (offset+21) (offset+220) (offset+23)
tmodel = train tdata :: KDVector FourD

randdata len offset r = replicateM len $ do
    let range = (-r+offset,r+offset)
    a <- randomRIO range
    b <- randomRIO range
    c <- randomRIO range
    d <- randomRIO range
    return $ FourD a b c d

randmodel :: IO (KDVector FourD)
randmodel = fmap train $ randdata 1000 0 1000
