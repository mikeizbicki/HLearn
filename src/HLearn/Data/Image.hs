{-# LANGUAGE ForeignFunctionInterface #-}

module HLearn.Data.Image
    ( CIELab
    , RGB
    , rgb2cielab
    , ColorSig
    , loadColorSig
    )
    where

import SubHask
import SubHask.TemplateHaskell.Deriving

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Generic as VG

import System.IO
import System.IO.Unsafe

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array

import qualified Prelude as P

--------------------------------------------------------------------------------

-- | An alternative way to represent colors than RGB that is closer to how humans actually perceive color.
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

-- | Implements formulas taken from the opencv page:
-- http://docs.opencv.org/modules/imgproc/doc/miscellaneous_transformations.html?highlight=cvtcolor
--
-- FIXME:
-- We should either:
--  * implement all of the color differences
--  * use the haskell package
--  * use opencv
--
rgb2cielab :: (ClassicalLogic a, Ord a, ExpField a) => RGB a -> CIELab a
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

instance Metric (CIELab Float) where
    {-# INLINABLE distance #-}
    distance c1 c2 = sqrt $ (l c1-l c2)*(l c1-l c2)
                          + (a c1-a c2)*(a c1-a c2)
                          + (b c1-b c2)*(b c1-b c2)

--------------------------------------------------------------------------------

-- | The standard method for representing colors on most computer monitors and display formats.
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

instance Metric (RGB Float) where
    distance c1 c2 = sqrt $ (red   c1-red   c2)*(red   c1-red   c2)
                          + (green c1-green c2)*(green c1-green c2)
                          + (blue  c1-blue  c2)*(blue  c1-blue  c2)

--------------------------------------------------------------------------------

-- | A signature is sparse representation of a histogram.
-- This is used to implement the earth mover distance between images.
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

instance Eq_ (ColorSig Float) where
    sig1==sig2 = rgbV sig1 == rgbV sig2
              && weightV sig1 == weightV sig2

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

instance Metric (ColorSig Float) where
    distance = emd_float
    distanceUB = lb2distanceUB emlb_float

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

