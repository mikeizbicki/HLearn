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
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Mutable as VM
import qualified Data.Foldable as F
import Test.QuickCheck hiding ((.&.),(.|.),vector)
import Debug.Trace

import qualified Control.ConstraintKinds as CK
import Data.Prunable
import HLearn.Algebra
import HLearn.DataStructures.SortedVector
import Data.BitTwiddler


-------------------------------------------------------------------------------
-- FourD

fd1 = FourD 0.5 0.5 0.5 0.5
fd2 = FourD 1 1 1 1
fd3 = FourD 2 2 2 2
fd4 = FourD 3 3 3 3
bw1 = toBitTwiddler fd1
bw2 = toBitTwiddler fd2
bw3 = toBitTwiddler fd3
bw4 = toBitTwiddler fd4

expand_boundingbox_FourD :: (FourD,FourD) -> (FourD,FourD)
expand_boundingbox_FourD (x,y) = (fromBitTwiddler common, fromBitTwiddler $ (complement mask) .|. common)
    where
        common = mask .&. x'
        mask = bitwisemask 4 $ leadingcommonbits x' y'
        x' = toBitTwiddler x
        y' = toBitTwiddler y

isOverlap :: (BitTwiddler a,BitTwiddler a) -> (BitTwiddler a,BitTwiddler a) -> Bool
isOverlap (a,b) (c,d) = a .&. mask == c .&. mask
    where
        mask = mask1 .&. mask2
        mask1 = bitwisemask 4 (leadingcommonbits a b)
        mask2 = bitwisemask 4 (leadingcommonbits c d)

boundingbox_FourD :: FourD -> Ring (FourD) -> (FourD,FourD)
boundingbox_FourD center@(FourD a b c d) dist = (minus,plus)
    where
        minus = FourD (a-dist) (b-dist) (c-dist) (d-dist)
        plus  = FourD (a+dist) (b+dist) (c+dist) (d+dist)

boundingbox_BitTwiddler :: BitTwiddler FourD -> Ring (FourD) -> (BitTwiddler FourD,BitTwiddler FourD)
boundingbox_BitTwiddler bw dist = (toBitTwiddler a,toBitTwiddler b)
    where
        (a,b) = boundingbox_FourD (fromBitTwiddler bw) dist

leadingcommonbits :: BitTwiddler a -> BitTwiddler a -> Int
leadingcommonbits (BitTwiddler a) (BitTwiddler b) = go 0
    where
        size = sizeofByteArray a `intdiv` 8

        go :: Int -> Int 
        go i = if i== size
            then 64*size
            else if worda == wordb
                then go (i+1)
                else i*64 + (nlz $ worda `xor` wordb)
            where 
                worda = indexByteArray a (size-i-1) :: Word64
                wordb = indexByteArray b (size-i-1) :: Word64

leadingcommonbits_old :: BitTwiddler a -> BitTwiddler a -> Int
leadingcommonbits_old (BitTwiddler a) (BitTwiddler b) = go 0
    where
        go :: Int -> Int 
        go 5 = 64*4
        go i = if worda == wordb
            then go (i+1)
            else i*64 + (rank $ worda `xor` wordb) -1
            where 
                worda = indexByteArray a i :: Word64
                wordb = indexByteArray b i :: Word64

bitwisemask :: Int -> Int -> BitTwiddler a
bitwisemask numWord64 n = BitTwiddler $ runST $ do
    arr <- newByteArray $ numWord64*8
    forM_ [0..numWord64-1] $ \i -> do
        let ni = n-64*i
        writeByteArray arr (numWord64-i-1) $ word64mask ni
    arr <- unsafeFreezeByteArray arr
    return arr

word64mask :: Int -> Word64
word64mask n
    | n<0  = word64vec V.! 0
    | n>64 = word64vec V.! 64
    | otherwise = word64vec V.! n

word64vec :: V.Vector Word64
word64vec = runST $ do 
    v <- VM.new 65
    VM.write v 0 (0::Word64)
    forM_ [1..64] $ \i -> do
        x <- VM.read v (i-1)
        VM.write v i $ bit (64-i) .|. x
    v <- V.freeze v
    return v


---------------------------------------

data FourD = FourD 
    { _d0 :: !Double
    , _d1 :: !Double
    , _d2 :: !Double
    , _d3 :: !Double
    }
    deriving (Read,Show,Eq,Ord)

makeIndex ''FourD

derivingUnbox "FourD"
    [t| FourD -> (Double,Double,Double,Double) |]
    [| \(FourD a b c d) -> (a,b,c,d) |]
    [| \(a,b,c,d) -> (FourD a b c d) |]

-- derivingUnbox "BitTwiddler"
--     [t| (VU.Unbox a) => BitTwiddler a -> a |]
--     [| \(BitTwiddler arr) -> (indexByteArray 0) |]
--     [| \(a) -> runST $ do arr <- newByteArray 16; writeByteArray arr 0 a; return $ unsafeThawByteArray arr |]

instance NFData FourD where
    rnf a = seq a ()

instance ToBitTwiddler FourD where
    toBitTwiddler (FourD a b c d) = BitTwiddler arr
        where
            (BitTwiddler arr) = bitzip (bitzip a' b') (bitzip c' d')
            a' = toBitTwiddler a
            b' = toBitTwiddler b
            c' = toBitTwiddler c
            d' = toBitTwiddler d
--              toBitTwiddler ((a,b),(c,d))
    fromBitTwiddler (BitTwiddler arr) = FourD (fromBitTwiddler a) (fromBitTwiddler b) (fromBitTwiddler c) (fromBitTwiddler d)
        where
            twid = BitTwiddler arr :: BitTwiddler ((Double,Double),(Double,Double))
            (ab,cd) = bitunzip twid
            (a,b) = bitunzip ab 
            (c,d) = bitunzip cd

--     toBitTwiddler (FourD a b c d) = toBitTwiddler [a,b,c,d]
--     fromBitTwiddler arr = FourD a b c d
--         where
--             a:b:c:d:[] = fromBitTwiddler arr

instance HasRing FourD where
    type Ring FourD = Double

instance MetricSpace FourD where
    distance (FourD a1 b1 c1 d1) (FourD a2 b2 c2 d2) = sqrt $ (a1-a2)^^2 + (b1-b2)^^2 -- + (c1-c2)^^2 + (d1-d2)^^2

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

-------------------------------------------------------------------------------
-- KDVector

newtype KDVector a = KDVector (SortedVector' V.Vector FourD (BitTwiddler a))
    deriving ({-Read,-}Show,Eq,Ord,Monoid,Abelian)

instance NFData (KDVector a) where
    rnf (KDVector sv) = rnf sv

instance (ToBitTwiddler a) => HomTrainer (KDVector a) where
    type Datapoint (KDVector a) = a
    train1dp = KDVector . train1dp . toBitTwiddler 

instance CK.Foldable KDVector where
    type FoldableConstraint KDVector a = (Ord a, ToBitTwiddler a)
    foldr f b (KDVector sv) = CK.foldr f b $ CK.fmap fromBitTwiddler sv
    foldr1 f (KDVector sv) = CK.foldr1 f $ CK.fmap fromBitTwiddler sv

-- instance Prunable KDVector where
--     prunefoldr p f b v@(KDVector (SortedVector' vec))
--         | V.length vec == 1 = f (vec V.! 0) b
--         | otherwise = if p b (KDVector (SortedVector' vec)) TreeLeft
--             then goright 
--             else prunefoldr p f goright (v ! TreeLeft)
-- 
--             where 
--                 goright = if p b (KDVector (SortedVector' vec)) TreeRight
--                     then b
--                     else prunefoldr p f b (v ! TreeRight)

kvfoldr f b (KDVector sv) = CK.foldr f b $ CK.fmap fromBitTwiddler sv

pf :: (ToBitTwiddler a) =>  (b -> KDVector a -> TreeIndex -> Bool) -> (a -> b -> b) -> b -> KDVector a -> b
pf p f b kv@(KDVector sv@(SortedVector' vec))
    | V.length vec == 1 = f (fromBitTwiddler $ vec V.! 0) b
    | otherwise = if p b kv TreeLeft
        then trace "prune TreeLeft" goright
        else pf p f goright (KDVector $ sv ! TreeLeft) 
    
    where
        goright = if p b kv TreeRight
            then trace "prune TreeRight" $ b
            else pf p f b (KDVector $ sv ! TreeRight) 

-- pf2 :: (ToBitTwiddler a) =>  (b -> KDVector a -> Bool) -> (a -> b -> b) -> b -> KDVector a -> b
pf2 p f b kv@(KDVector sv@(SortedVector' vec))
    | V.length vec == 1 = f (fromBitTwiddler $ vec V.! 0) b
    | V.length vec < 100 = F.foldr f b $ fmap fromBitTwiddler vec 
    | otherwise = if p b' kv 
        then {-trace (concat $ -- map (++"\n") 
            ["prune"
            , "  count = "++ show (V.length vec)
--             , "  b=" ++ show b
--             , "  ex_boundingbox=" ++ show (expand_boundingbox_FourD $ boundingbox_FourD (snd b') (fst b'))
--             , "  boundingbox=" ++ show (boundingbox_FourD (snd b') (fst b'))
--             , "  ex_(c,d) = " ++ show (expand_boundingbox_FourD (fromBitTwiddler $ vec V.! 0 :: FourD, fromBitTwiddler $ vec V.! (V.length vec -1) :: FourD))
--             , "  (c,d) = " ++ show ((fromBitTwiddler $ vec V.! 0 :: FourD, fromBitTwiddler $ vec V.! (V.length vec -1) :: FourD))
            ]) $-}  b
        else if distance (snd b) (fromBitTwiddler rval) < distance (snd b) (fromBitTwiddler lval)
            then rightleft
            else leftright 
        where
            
--             rsvec@(SortedVector' rvec) = sv ! TreeRight
--             rsvec@(SortedVector' lvec) = sv ! TreeLeft
            midpt = V.length vec `intdiv` 2
            rsvec@(SortedVector' rvec) = SortedVector' $ V.slice 0 (midpt) vec 
            lsvec@(SortedVector' lvec) = SortedVector' $ V.slice (midpt) (V.length vec-midpt-1) vec
            rmidpt = V.length rvec `intdiv` 2
            lmidpt = V.length lvec `intdiv` 2
            rval = rvec V.! rmidpt
            lval = lvec V.! lmidpt

            rightleft = pf2 p f (pf2 p f b' (KDVector rsvec)) (KDVector lsvec) 
            leftright = pf2 p f (pf2 p f b' (KDVector lsvec)) (KDVector rsvec) 
--             b' = f (fromBitTwiddler $ vec V.! (floor $ (fromIntegral $ V.length vec :: Double) / 2)) b
            b' = f (fromBitTwiddler $ vec V.! midpt) b

-- mindist_prune query (dist,dp) t TreeLeft  = (query ! splitdim t) - (val t ! splitdim t) < dist 
-- mindist_prune query (dist,dp) t TreeRight = (val t ! splitdim t) - (query ! splitdim t) < dist
-- 
-- mindist kv@(KDVector (SortedVector' v)) dp = CK.foldr mindist_cata (distance origin dp1,dp1) kv
--     where
--         dp1 = v ! 0

badfourd = FourD 1e100 1e100 1e100 1e100
inf = 1/0 :: Double

mindist query = pf noprune (mindist_cata query) (inf,badfourd)
mindist' query = pf2 noprune2 (mindist_cata query) (inf,badfourd)
mindist'' query = pf2 mindist_prune (mindist_cata query) (inf,badfourd)

-- mindist_prune (dist,dp) kv TreeLeft = 

noprune b kv ti = False
noprune2 b kv = False

mindist_prune (dist,dp) kv@(KDVector(SortedVector' v)) = not $ isOverlap (a,b) (c,d)
    where
        (a,b) = boundingbox_BitTwiddler (toBitTwiddler dp) dist
        (c,d) = (v V.! 0, v V.! (V.length v-1))

mindist_cata query a (dist,dp) = if dist > dista
    then (dista,a)
    else (dist,dp)
    where
        dista = distance query a

mindist_cata1 query a dp = if distance query dp > distance query a
    then a
    else dp

test4d = FourD 1000 1000 1000 1000

randdata len offset r = replicateM len $ do
    let range = (-r+offset,r+offset)
    a <- randomRIO range
    b <- randomRIO range
    let c = offset -- c <- randomRIO range
    let d = offset -- d <- randomRIO range
    return $ FourD a b c d

randmodel :: Int -> IO (KDVector FourD)
randmodel len = fmap train $ randdata len 0 1000

origin = FourD 0 0 0 0

grid = [FourD a b c d | a<-range, b<-range, c<-[1], d<-[1]]
    where range=[-15..15]

tdata = map (\x -> FourD x x x x) [-1000..1000]
tquery = FourD 900 901 902 903
tmodel = train tdata :: KDVector FourD
tmodelgrid = train grid :: KDVector FourD
(KDVector (SortedVector' v)) = tmodel
(v1,v2) = half v
len = V.length v

midpt :: V.Vector a -> Int
midpt vec = V.length vec `intdiv` 2

half :: V.Vector a -> (V.Vector a,V.Vector a)
half v = (V.slice 0 len1 v, V.slice (midpt v+1) len2 v)
    where
        len1 = midpt v
        len2 = V.length v - midpt v - 1

vec2boundingbox :: V.Vector (BitTwiddler a) -> (BitTwiddler a,BitTwiddler a)
vec2boundingbox v = (v V.! 0, v V.! (V.length v -1))

mindist2prunelist :: (Double,FourD) -> KDVector FourD -> V.Vector Bool
mindist2prunelist (dist,dp) (KDVector (SortedVector' v)) = fmap (\x -> x<a' || x > b') v
    where
        (a,b) = boundingbox_FourD dp dist
        (a',b') = (toBitTwiddler a,toBitTwiddler b)
