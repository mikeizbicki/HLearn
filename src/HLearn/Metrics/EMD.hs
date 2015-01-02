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

newtype EMD a = EMD a
    deriving (Show,NFData,Arbitrary,Eq_,POrd_,Lattice_,Ord_)

type instance Scalar (EMD a) = Float
type instance Logic (EMD a) = Logic a
type instance Elem (EMD a) = Elem a
-- deriveHierarchy ''EMD [ ''Ord ]

foreign import ccall unsafe "emd_float" emd_float_
    :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr Float -> IO Float

{-# INLINE emd_float #-}
emd_float ::
    ( Scalar a ~ Float
    , MetricSpace a
    ) => (Lexical (StorableArray Float), Lexical (Array a))
      -> (Lexical (StorableArray Float), Lexical (Array a))
      -> Float
emd_float
    (Lexical (ArrayT v1s), Lexical (ArrayT v1a))
    (Lexical (ArrayT v2s), Lexical (ArrayT v2a)) = unsafeDupablePerformIO $
        withForeignPtr fp1 $ \p1 ->
        withForeignPtr fp2 $ \p2 ->
        withForeignPtr fpcost $ \pcost ->
            {-# SCC emd_float_ #-}
            emd_float_ p1 n1 p2 n2 pcost

    where
        (fp1,n1) = VS.unsafeToForeignPtr0 v1s
        (fp2,n2) = VS.unsafeToForeignPtr0 v2s

        vcost = VS.generate (n1*n2) $ \i -> distance
            (v1a V.! (i`div`n2))
            (v2a V.! (i`mod`n2))

        (fpcost,_) = VS.unsafeToForeignPtr0 vcost

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

lb2isFartherThan ::
    ( MetricSpace a
    , Logic a ~ Bool
    ) => (a -> a -> Scalar a)
      -> (a -> a -> Scalar a -> Bool)
lb2isFartherThan lb p q bound = if lb p q > bound
    then True
    else distance p q > bound

lb2isFartherThanWithDistance ::
    ( MetricSpace a
    , CanError (Scalar a)
    , Logic a ~ Bool
    ) => (a -> a -> Scalar a)
      -> (a -> a -> Scalar a -> Scalar a)
lb2isFartherThanWithDistance lb p q bound = if lb p q > bound
    then errorVal
    else if dist' > bound
        then errorVal
        else dist'
    where
        dist' = distance p q

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
--     distance (EMD (Lexical v1f,Lexical v1a)) (EMD (Lexical v2f,Lexical v2a)) = {-# SCC emd_distance #-}
    distance (EMD a1) (EMD a2) =
        emd_float a1 a2

--     distance (EMD (vf1,va1)) (EMD (vf2,va2)) = {-# SCC emd_distance #-} emd_float (toList a1) (toList a2)
--     distance (EMD (Lexical (ArrayT a1))) (EMD (Lexical (ArrayT a2))) = --trace "distance" $
--         emd_float (VG.toList a1) (VG.toList a2)

--     {-# INLINE isFartherThan #-}
--     isFartherThan (EMD p) (EMD q) bound= {-# SCC emd_isFartherThan #-} if emd_float_lb p' q' > bound
--         then True
--         else emd_float p' q' > bound
--         where
--             p' = toList p
--             q' = toList q

--     isFartherThan = error "isFartherThan"
--     isFartherThanWithDistanceCanError = error "isFartherThanWithDistanceCanError"

---------------------------------------
-- FIXME:
-- why is it *here?

type instance Logic (a,b) = Logic a

-- instance (Eq a, Eq b) => Eq_ (a,b) where
--     (a1,b1)==(a2,b2) = a1==a2 && b1==b2

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

-- newtype LexicalTup a = LexicalTup a
--     deriving (Show,NFData)
--
-- type instance Logic (LexicalTup a) = Bool
--
-- instance (Eq a, Eq b) => Eq_ (LexicalTup (a,b)) where
--     (LexicalTup (a1,b1))==(LexicalTup (a2,b2)) = a1==a2 && b1==b2
--
-- instance (Ord a, Ord b) => POrd_ (LexicalTup (a,b)) where
--     inf (LexicalTup (a1,b1)) (LexicalTup (a2,b2)) = LexicalTup $ case compare a1 a2 of
--         LT -> (a1,b1)
--         GT -> (a2,b2)
--         EQ -> (a1,inf b1 b2)
--
-- instance (Ord a, Ord b) => Lattice_ (LexicalTup (a,b)) where
--     sup (LexicalTup (a1,b1)) (LexicalTup (a2,b2)) = LexicalTup $ case compare a1 a2 of
--         LT -> (a2,b2)
--         GT -> (a1,b1)
--         EQ -> (a1,sup b1 b2)
