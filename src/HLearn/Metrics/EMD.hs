{-# LANGUAGE ForeignFunctionInterface #-}

module HLearn.Metrics.EMD
    where

import SubHask
import SubHask.Compatibility.Vector
import SubHask.TemplateHaskell.Deriving

-- import SubHask.Monad
import Control.Monad

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Generic as VG

import System.IO
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import System.IO.Unsafe
import Unsafe.Coerce

import qualified Prelude as P

import Debug.Trace
import SubHask.Algebra.Container
import SubHask.Compatibility.Vector.Lebesgue
import qualified Data.Vector.Generic as VG

--------------------------------------------------------------------------------

-- |
--
-- See wikipedia on <https://en.wikipedia.org/wiki/Color_difference color difference>
data CIELab a = CIELab
    { l :: !a
    , a :: !a
    , b :: !a
    }
    deriving (Show)

type instance Scalar (CIELab a) = a
type instance Logic (CIELab a) = Logic a

instance ValidEq a => Eq_ (CIELab a) where
    c1==c2 = l c1 == l c2
          && a c1 == a c2
          && b c1 == b c2

instance NFData a => NFData (CIELab a) where
    rnf c = deepseq (l c)
          $ deepseq (a c)
          $ rnf (b c)

instance Storable a => Storable (CIELab a) where
    sizeOf _ = 3*sizeOf (undefined::a)
    alignment _ = alignment (undefined::a)

    peek p = do
        l <- peek $ plusPtr p $ 0*sizeOf (undefined::a)
        a <- peek $ plusPtr p $ 1*sizeOf (undefined::a)
        b <- peek $ plusPtr p $ 2*sizeOf (undefined::a)
        return $ CIELab l a b

    poke p (CIELab l a b) = do
        poke (plusPtr p $ 0*sizeOf (undefined::a)) l
        poke (plusPtr p $ 1*sizeOf (undefined::a)) a
        poke (plusPtr p $ 2*sizeOf (undefined::a)) b

-- | Formulas taken from the opencv page:
-- http://docs.opencv.org/modules/imgproc/doc/miscellaneous_transformations.html?highlight=cvtcolor
--
-- FIXME:
-- We should either:
--  * implement all of the color differences
--  * use the haskell package
--  * use opencv
--
rgb2cielab :: (ClassicalLogic a, Ord a, Floating a) => RGB a -> CIELab a
rgb2cielab (RGB r g b) = CIELab l_ a_ b_
    where
        x = 0.412453*r + 0.357580*g + 0.180423*b
        y = 0.212671*r + 0.715160*g + 0.072169*b
        z = 0.019334*r + 0.119193*g + 0.950227*b

        x' = x / 0.950456
        z' = z / 1.088754

        l_ = if y > 0.008856
            then 116*y**(1/3)-16
            else 903.3*y
        a_ = 500*(f x' - f y ) + delta
        b_ = 200*(f y  - f z') + delta

        f t = if t > 0.008856
            then t**(1/3)
            else 7.787*t+16/116

        delta=0

instance MetricSpace (CIELab Float) where
    distance c1 c2 = sqrt $ (l c1-l c2)*(l c1-l c2)
                          + (a c1-a c2)*(a c1-a c2)
                          + (b c1-b c2)*(b c1-b c2)

--------------------------------------------------------------------------------

data RGB a = RGB
    { red   :: !a
    , green :: !a
    , blue  :: !a
    }
    deriving (Show)

type instance Scalar (RGB a) = a
type instance Logic (RGB a) = Logic a

instance ValidEq a => Eq_ (RGB a) where
    c1==c2 = red   c1 == red   c2
          && green c1 == green c2
          && blue  c1 == blue  c2

instance NFData a => NFData (RGB a) where
    rnf c = deepseq (red c)
          $ deepseq (green c)
          $ rnf (blue c)

instance Storable a => Storable (RGB a) where
    sizeOf _ = 3*sizeOf (undefined::a)
    alignment _ = alignment (undefined::a)

    peek p = do
        r <- peek $ plusPtr p $ 0*sizeOf (undefined::a)
        g <- peek $ plusPtr p $ 1*sizeOf (undefined::a)
        b <- peek $ plusPtr p $ 2*sizeOf (undefined::a)
        return $ RGB r g b

    poke p (RGB r g b) = do
        poke (plusPtr p $ 0*sizeOf (undefined::a)) r
        poke (plusPtr p $ 1*sizeOf (undefined::a)) g
        poke (plusPtr p $ 2*sizeOf (undefined::a)) b

instance MetricSpace (RGB Float) where
    distance c1 c2 = sqrt $ (red   c1-red   c2)*(red   c1-red   c2)
                          + (green c1-green c2)*(green c1-green c2)
                          + (blue  c1-blue  c2)*(blue  c1-blue  c2)

--------------------------------------------------------------------------------

data ColorSig a = ColorSig
    { rgbV    :: !(StorableArray (CIELab a))
    , weightV :: !(StorableArray a)
    }
    deriving (Show)

type instance Scalar (ColorSig a) = Scalar a
type instance Logic (ColorSig a) = Logic a

instance NFData a => NFData (ColorSig a) where
    rnf a = deepseq (rgbV a)
          $ rnf (weightV a)

-- instance (Storable a, ValidEq a) => Eq_ (ColorSig a) where
instance Eq_ (ColorSig Float) where
    sig1==sig2 = rgbV sig1 == rgbV sig2
              && weightV sig1 == weightV sig2

-- FIXME: these are unlawful, but they're needed for the way allknn currently works
-- instance (Storable a, ValidEq a, POrd_ a) => POrd_ (ColorSig a) where
-- instance (Storable a, ValidEq a, Lattice_ a) => Lattice_ (ColorSig a) where
-- instance (Storable a, ValidEq a, Ord_ a) => Ord_ (ColorSig a) where
instance POrd_ (ColorSig Float) where
instance Lattice_ (ColorSig Float) where
instance Ord_ (ColorSig Float) where
    compare sig1 sig2 = compare (Lexical $ weightV sig1) (Lexical $ weightV sig2)

loadColorSig ::
    (
    ) => Bool     -- ^ print debug info?
      -> FilePath -- ^ path of signature file
      -> IO (ColorSig Float)
loadColorSig debug filepath = {-# SCC loadColorSig #-} do
    filedata <- liftM P.lines $ readFile filepath

    let (rgbs,ws) = P.unzip
            $ map (\[b,g,r,v] -> (rgb2cielab $ RGB r g b, v))
            $ map (read.(\x->"["+x+"]")) filedata

    let totalWeight = sum ws
        ws' = map (/totalWeight) ws

    let ret = ColorSig
            { rgbV    = fromList rgbs
            , weightV = fromList ws'
            }

    when debug $ do
        putStrLn $ "filepath="++show filepath
        putStrLn $ "  filedata="++show filedata
        putStrLn $ "signature length=" ++ show (length filedata)

    deepseq ret $ return ret

instance MetricSpace (ColorSig Float) where
    distance = emd_float
    isFartherThan = lb2isFartherThan emlb_float
    isFartherThanWithDistanceCanError = lb2isFartherThanWithDistance emlb_float

foreign import ccall unsafe "emd_float" emd_float_
    :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr Float -> IO Float

{-# INLINABLE emd_float #-}
emd_float :: ColorSig Float -> ColorSig Float -> Float
emd_float (ColorSig rgbV1 (ArrayT weightV1)) (ColorSig rgbV2 (ArrayT weightV2))
    = {-# SCC emd_float #-} unsafeDupablePerformIO $
        withForeignPtr fp1 $ \p1 ->
        withForeignPtr fp2 $ \p2 ->
        withForeignPtr fpcost $ \pcost ->
            emd_float_ p1 n1 p2 n2 pcost

    where
        (fp1,n1) = VS.unsafeToForeignPtr0 weightV1
        (fp2,n2) = VS.unsafeToForeignPtr0 weightV2

        vcost = {-# SCC vcost #-} VS.generate (n1*n2) $ \i -> distance
            (rgbV1 `VG.unsafeIndex` (i`div`n2))
            (rgbV2 `VG.unsafeIndex` (i`mod`n2))

        (fpcost,_) = VS.unsafeToForeignPtr0 vcost

emlb_float :: ColorSig Float -> ColorSig Float -> Float
emlb_float sig1 sig2 = distance (centroid sig1) (centroid sig2)

centroid :: ColorSig Float -> CIELab Float
centroid (ColorSig rgbV weightV) = go (VG.length rgbV-1) (CIELab 0 0 0)
    where
        go (-1) tot = tot
        go i    tot = go (i-1) $ CIELab
            { l = l tot + l (rgbV `VG.unsafeIndex` i) * (weightV `VG.unsafeIndex` i)
            , a = a tot + a (rgbV `VG.unsafeIndex` i) * (weightV `VG.unsafeIndex` i)
            , b = b tot + b (rgbV `VG.unsafeIndex` i) * (weightV `VG.unsafeIndex` i)
            }

lb2isFartherThan ::
    ( MetricSpace a
    , Logic a ~ Bool
    ) => (a -> a -> Scalar a)
      -> (a -> a -> Scalar a -> Bool)
lb2isFartherThan lb p q bound = {-# SCC lb2isFartherThan #-} if lb p q > bound
    then True
    else distance p q > bound

lb2isFartherThanWithDistance ::
    ( MetricSpace a
    , CanError (Scalar a)
    , Logic a ~ Bool
    ) => (a -> a -> Scalar a)
      -> (a -> a -> Scalar a -> Scalar a)
lb2isFartherThanWithDistance lb p q bound = {-# SCC lb2isFartherThanWithDistance #-} if lb p q > bound
    then errorVal
    else if dist' > bound
        then errorVal
        else dist'
    where
        dist' = distance p q

--------------------------------------------------------------------------------

{-
type HistogramSignature_ a = EMD (Lexical (StorableArray Float), Lexical (Array a))
type HistogramSignature = HistogramSignature_ (L2 Vector Float)

loadHistogramSignature
    :: Bool     -- ^ print debug info?
    -> FilePath -- ^ path of signature file
    -> IO HistogramSignature
loadHistogramSignature debug filepath = {-# SCC loadHistogramSignature #-} do
    filedata <- liftM P.lines $ readFile filepath

    let (fs,as) = P.unzip
            $ map (\[b,g,r,v] -> (v,VG.fromList [r,g,b]))
            $ map ((read :: String -> [Float]).(\x->"["+x+"]")) filedata

        ret = (fromList fs, fromList as)

    when debug $ do
        putStrLn $ "filepath="++show filepath
        putStrLn $ "  filedata="++show filedata
        putStrLn $ "signature length=" ++ show (length filedata)

    deepseq ret $ return $ EMD ret


newtype EMD a = EMD a
    deriving (Show,NFData,Arbitrary,Eq_,POrd_,Lattice_,Ord_)

type instance Scalar (EMD a) = Float
type instance Logic (EMD a) = Logic a
type instance Elem (EMD a) = Elem a
-- deriveHierarchy ''EMD [ ''Ord ]

{-# INLINE emd_float #-}
emd_float ::
    ( Scalar a ~ Float
    , MetricSpace a
    ) => (Lexical (StorableArray Float), Lexical (Array a))
      -> (Lexical (StorableArray Float), Lexical (Array a))
      -> Float
emd_float
    (Lexical (ArrayT v1s), Lexical (ArrayT v1a))
    (Lexical (ArrayT v2s), Lexical (ArrayT v2a))
    = {-# SCC emd_float #-} unsafeDupablePerformIO $
        withForeignPtr fp1 $ \p1 ->
        withForeignPtr fp2 $ \p2 ->
        withForeignPtr fpcost $ \pcost ->
            emd_float_ p1 n1 p2 n2 pcost

    where
        (fp1,n1) = VS.unsafeToForeignPtr0 v1s
        (fp2,n2) = VS.unsafeToForeignPtr0 v2s

        vcost = {-# SCC vcost #-} VS.generate (n1*n2) $ \i -> distance
            (v1a V.! (i`div`n2))
            (v2a V.! (i`mod`n2))

        (fpcost,_) = VS.unsafeToForeignPtr0 vcost

emlb_float ::
    ( a ~ EMD (Lexical (StorableArray Float), Lexical (Array (L2 Vector Float)))
    ) => a -> a -> Scalar a
emlb_float (EMD p1) (EMD p2) = {-# SCC emlb_float #-} distance (centroid p1) (centroid p2)

centroid :: (Lexical (StorableArray Float), Lexical (Array (L2 Vector Float))) -> L2 Vector Float
centroid (Lexical (ArrayT ws), Lexical (ArrayT vs)) = {-trace "centroid" $-} go (VG.length ws-1) zero
    where
        go :: Int -> L2 Vector Float -> L2 Vector Float
        go (-1) (L2 tot) = L2 tot
        go i (L2 tot) = -- trace ("  go "++show i) $
                      L2 $ tot + ((ws `VG.unsafeIndex` i) *. (unL2 $ vs `VG.unsafeIndex` i))

-- {-# INLINABLE emd_float #-}
-- emd_float ::
--     ( Scalar a ~ Float
--     , MetricSpace a
--     ) => [(Float,a)]
--       -> [(Float,a)]
--       -> Float
-- emd_float xs ys = unsafeDupablePerformIO $
--     withForeignPtr fp1 $ \p1 ->
--     withForeignPtr fp2 $ \p2 ->
--     withForeignPtr fpcost $ \pcost ->
--         {-# SCC emd_float_ #-}
--         emd_float_ p1 n1 p2 n2 pcost
--
--     where
--         v1s = VS.fromList $ map fst xs
--         v2s = VS.fromList $ map fst ys
--         (fp1,n1) = VS.unsafeToForeignPtr0 v1s
--         (fp2,n2) = VS.unsafeToForeignPtr0 v2s
--
--         v1 = V.fromList $ map snd xs
--         v2 = V.fromList $ map snd ys
--         vcost :: Vector Float
--         vcost =
-- --           trace ("n1="++show n1++"; length v1="++show (VG.length v1)) $
-- --           trace ("n2="++show n2++"; length v2="++show (VG.length v2)) $
--           VS.generate (n1*n2) $ \i -> distance
--             (v1 V.! (i`div`n2))
--             (v2 V.! (i`mod`n2))
--
--         (fpcost,_) = VS.unsafeToForeignPtr0 vcost

emd_float_lb ::
    ( Scalar a ~ Float
    , MetricSpace a
    , VectorSpace a
    ) => [(Float,a)]
      -> [(Float,a)]
      -> Float
emd_float_lb xs ys = {-# SCC emd_float_lb #-} distance xbar ybar
    where
        xbar = (sum $ map (\(r,a) -> r*.a) xs) ./ (sum $ map fst xs)
        ybar = (sum $ map (\(r,a) -> r*.a) ys) ./ (sum $ map fst xs)


-- type instance Scalar (L2 v s) = Scalar (v s)
deriving instance (Scalar (v s) ~ Scalar (L2 v s), Semigroup (v s)) => Semigroup (L2 v s)
deriving instance (Scalar (v s) ~ Scalar (L2 v s), Group (v s)) => Group (L2 v s)
deriving instance (Scalar (v s) ~ Scalar (L2 v s), Monoid (v s)) => Monoid (L2 v s)
deriving instance (Scalar (v s) ~ Scalar (L2 v s), Cancellative (v s)) => Cancellative (L2 v s)
deriving instance (Scalar (v s) ~ Scalar (L2 v s), Abelian (v s)) => Abelian (L2 v s)
deriving instance (Scalar (v s) ~ Scalar (L2 v s), Module (v s)) => Module (L2 v s)
deriving instance (Scalar (v s) ~ Scalar (L2 v s), VectorSpace (v s)) => VectorSpace (L2 v s)


-- instance
--     ( Foldable a
--     , Eq a
--     , Ord (Scalar a)
--     , HasScalar a
--     , Elem a ~ (Float,b)
--     , MetricSpace b
--     , Scalar b ~ Float
--     ) => MetricSpace (EMD a)
-- instance MetricSpace (EMD (Lexical (Array ((Float, L2 Vector Float)))))
instance MetricSpace (EMD (Lexical (StorableArray Float), Lexical (Array (L2 Vector Float))))
        where

    {-# INLINE distance #-}
    distance (EMD a1) (EMD a2) = emd_float a1 a2

    isFartherThan=lb2isFartherThan emlb_float

    isFartherThanWithDistanceCanError = lb2isFartherThanWithDistance emlb_float
-}

---------------------------------------
-- FIXME:
-- why is it *here?

type instance Logic (a,b) = Logic a

instance (Ord a, Ord b) => POrd_ (a,b) where
    inf (a1,b1) (a2,b2) = case compare a1 a2 of
        LT -> (a1,b1)
        GT -> (a2,b2)
        EQ -> (a1,inf b1 b2)

instance (Ord a, Ord b) => Lattice_ (a,b) where
    sup (a1,b1) (a2,b2) = case compare a1 a2 of
        LT -> (a2,b2)
        GT -> (a1,b1)
        EQ -> (a1,sup b1 b2)

instance (Ord a, Ord b) => Ord_ (a,b)
