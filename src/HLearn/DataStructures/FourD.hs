{-# LANGUAGE TemplateHaskell #-}
module HLearn.DataStructures.FourD
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
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Mutable as VM
import qualified Data.Foldable as F
import Test.QuickCheck hiding ((.&.),(.|.),vector)
import Debug.Trace
import Foreign

import qualified Control.ConstraintKinds as CK
import Data.Prunable
import HLearn.Algebra
import HLearn.DataStructures.SortedVector
-- import Data.BitTwiddler
import HLearn.DataStructures.BitTwiddlerSort.BitTwiddler

data Bound a = Bound 
    { upper :: !a
    , lower :: !a
    }
    deriving (Read,Show,Eq,Ord)

calcBound_FourD :: (Double,FourD) -> Bound FourD
calcBound_FourD (dist,dp) = Bound
    { upper = FourD 
        { _d0 = _d0 dp + dist
        , _d1 = _d1 dp + dist
        , _d2 = _d2 dp + dist
        , _d3 = _d3 dp + dist
        }
    , lower = FourD 
        { _d0 = _d0 dp - dist
        , _d1 = _d1 dp - dist
        , _d2 = _d2 dp - dist
        , _d3 = _d3 dp - dist
        }
    }

calcBound_BitTwiddler4 :: (Double,FourD) -> Bound BitTwiddler4
calcBound_BitTwiddler4 (dist,fd) = Bound 
    { upper = toBitTwiddler $ upper bound
    , lower = toBitTwiddler $ lower bound
    }
    where
        bound = calcBound_FourD (dist,fd)

-------------------------------------------------------------------------------
-- data types 

data FourD = FourD 
    { _d0 :: !Double
    , _d1 :: !Double
    , _d2 :: !Double
    , _d3 :: !Double
    }
    deriving (Read,Show,Eq,Ord)

instance NFData FourD where
    rnf a = seq a ()

instance Storable FourD where
    {-# INLINE sizeOf #-}
    {-# INLINE alignment #-}
    {-# INLINE peek #-}
    {-# INLINE poke #-}

    sizeOf _ = 32
    alignment _ = 32 

    peek addr = do
        a :: Double <- peek $ addr `plusPtr` 0
        b :: Double <- peek $ addr `plusPtr` 8
        c :: Double <- peek $ addr `plusPtr` 16
        d :: Double <- peek $ addr `plusPtr` 24
        return $ FourD a b c d 

    poke addr bt = do
        poke (addr `plusPtr` 0) (_d0 bt)
        poke (addr `plusPtr` 8) (_d1 bt)
        poke (addr `plusPtr` 16) (_d2 bt)
        poke (addr `plusPtr` 24) (_d3 bt)

instance ToBitTwiddler FourD where
    type BitTwiddler FourD = BitTwiddler4
    toBitTwiddler (FourD a b c d) = bitzip 
        ( bitorderMarshall a 
        , bitorderMarshall b 
        , bitorderMarshall c
        , bitorderMarshall d
        )
    fromBitTwiddler bt = FourD 
        (bitorderUnmarshall a) 
        (bitorderUnmarshall b) 
        (bitorderUnmarshall c) 
        (bitorderUnmarshall d)
        where
            (a,b,c,d) = bitunzip bt

leadingCommonBits :: (Bits a) => a -> a -> Int
leadingCommonBits a b = go (bitSize a-1)
    where
        go (-1) = bitSize a 
        go i = if testBit a i == testBit b i
            then go (i-1)
            else bitSize a-1-i

-------------------------------------------------------------------------------
-- algebra

instance HasRing FourD where
    type Ring FourD = Double

instance MetricSpace FourD where
    distance (FourD a1 b1 c1 d1) (FourD a2 b2 c2 d2) = sqrt $ (a1-a2)^^2 + (b1-b2)^^2 + (c1-c2)^^2 + (d1-d2)^^2
--     distance (FourD a1 b1 c1 d1) (FourD a2 b2 c2 d2) = sqrt $ (a1-a2)^^2 + (b1-b2)^^2 -- + (c1-c2)^^2 + (d1-d2)^^2

---------------------------------------
-- test FourD

type Prop1 a = a -> Bool
type Prop2 a = a -> a -> Bool
type Prop3 a = a -> a -> a -> Bool

property_distance0 :: (Eq (Ring a), MetricSpace a) => a -> Bool
property_distance0 a = distance a a == 0

property_triangle :: (Ord (Ring a), MetricSpace a, Monoid a) => a -> a -> a -> Bool
property_triangle a b c = distance a c + distance b c >= distance (a<>b) c

instance Arbitrary FourD where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ FourD a b c d

