module HLearn.Metrics.String
    where

import SubHask

import Data.Function.Memoize

-------------------------------------------------------------------------------

newtype Jaccard a = Jaccard a
    deriving (Read,Show,Eq)

type instance Scalar (Jaccard a) = Scalar a

instance
    ( Lattice a
    , Field (Scalar a)
    , Normed a
    , Eq a
    ) => MetricSpace (Jaccard a)
        where
    distance (Jaccard xs) (Jaccard ys) = 1 - abs (xs && ys) / abs (xs || ys)

-------------------------------------------------------------------------------

newtype Hamming a = Hamming a
    deriving (Read,Show,Eq,POrd,Ord,Lattice,NFData)

instance Semigroup a => Semigroup (Hamming a) where
    (Hamming a1)+(Hamming a2)=Hamming (a1+a2)
--     associativeEpsilon a = 0

instance Monoid a => Monoid (Hamming a) where
    zero = Hamming zero

instance Container a => Container (Hamming a) where
    type Elem (Hamming a) = Elem a
    type ElemConstraint (Hamming a) = ElemConstraint a
    elem e (Hamming a) = elem e a

deriving instance (Integral (Scalar a), Unfoldable a) => Unfoldable (Hamming a)
deriving instance (Integral (Scalar a), Foldable a) => Foldable (Hamming a)

instance (Integral (Scalar a), Normed a) => Normed (Hamming a) where
    abs (Hamming a) = fromIntegral $ abs a

-- FIXME
-- type instance Scalar (Hamming a) = Scalar a
type instance Scalar (Hamming a) = Float

instance
    ( Foldable a
    , Eq (Elem a)
    , Eq a
    ) => MetricSpace (Hamming a)
        where

    distance (Hamming xs) (Hamming ys) = {-# distance_Hamming #-} go (toList xs) (toList ys) 0
        where
            go [] [] i = i
            go xs [] i = i + fromIntegral (abs xs)
            go [] ys i = i + fromIntegral (abs ys)
            go (x:xs) (y:ys) i = go xs ys $ i + if x==y
                then 0
                else 1

-------------------------------------------------------------------------------

newtype Levenshtein a = Levenshtein a
    deriving (Read,Show,Eq,POrd,Ord,Lattice,NFData)

instance Semigroup a => Semigroup (Levenshtein a) where
    (Levenshtein a1)+(Levenshtein a2)=Levenshtein (a1+a2)
--     associativeEpsilon a = 0

instance Monoid a => Monoid (Levenshtein a) where
    zero = Levenshtein zero

instance Container a => Container (Levenshtein a) where
    type Elem (Levenshtein a) = Elem a
    type ElemConstraint (Levenshtein a) = ElemConstraint a
    elem e (Levenshtein a) = elem e a

deriving instance (Integral (Scalar a), Unfoldable a) => Unfoldable (Levenshtein a)
deriving instance (Integral (Scalar a), Foldable a) => Foldable (Levenshtein a)

instance (Integral (Scalar a), Normed a) => Normed (Levenshtein a) where
    abs (Levenshtein a) = fromIntegral $ abs a

-- FIXME
-- type instance Scalar (Levenshtein a) = Scalar a
type instance Scalar (Levenshtein a) = Float

instance
    ( Foldable a
    , Memoizable (Elem a)
    , Eq (Elem a)
    , Eq a
    ) => MetricSpace (Levenshtein a)
        where
    distance (Levenshtein xs) (Levenshtein ys) = {-# distance_Levenshtein #-} fromIntegral $ go (toList xs) (toList ys)
        where
            go = memoize2 go'
                where
                    go' [] [] = 0
                    go' xs [] = length xs
                    go' [] ys = length ys
                    go' (x:xs) (y:ys) = minimum
                        [ go xs (y:ys) + 1
                        , go (x:xs) ys + 1
                        , go xs ys + if (x/=y) then 1 else 0
                        ]
