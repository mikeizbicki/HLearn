{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-# LANGUAGE TemplateHaskell #-}

module HLearn.Algebra.Types.HList
    ( 
    -- * Heterogenous List
    HList (..)
    , HLength (..)
    , List2HList (..)
    , HList2List (..)
    , HTake1 (..)
    , HDrop1 (..)
    , HMap (..)

    -- ** Typeable
    , TypeList(..)
    
    -- * Downcasting
    , ConstraintBox (..)
    , Downcast (..)
    
    -- * Boxes
    , ShowBox (..)
    , AnyBox (..)
    
    -- * Type functions
    
    -- ** HList
    , HCons (..)
    , UnHList (..)
    , HAppend
    
    -- ** Type Lists
    , Distribute (..)
    , Replicate (..)
    , Map (..)
    , Reverse (..)
    , (:!) (..)
--     , Index (..)
    , (++) (..)
    , ($) (..)
    , Concat (..)
    , Length (..)
    , Length1 (..)
    
    -- ** Type Nats
    , Nat1(..)
    , Nat1Box(..)
    , ToNat1
    , FromNat1
    )
    where

import Data.Dynamic
import Data.Monoid
import GHC.TypeLits
import Unsafe.Coerce

import HLearn.Algebra.Functions
import HLearn.Algebra.Types.Nat1

-------------------------------------------------------------------------------
-- HList

-- | The heterogenous list
data HList :: [*] -> * where
    HNil :: HList '[]
    (:::) :: t -> HList ts -> HList (t ': ts)
  
infixr 5 :::

instance Show (HList '[]) where
    show _ = "HNil"
instance (Show x, Show (HList xs)) => Show (HList (x ': xs)) where
    show (x:::xs) = show x ++":::"++show xs

instance Eq (HList '[]) where
    xs==ys = True
instance (Eq x, Eq (HList xs)) => Eq (HList (x ': xs)) where
    (x:::xs)==(y:::ys) = (x==y)&&(xs==ys)
    
instance Ord (HList '[]) where
    compare HNil HNil = EQ
instance (Ord x, Ord (HList xs)) => Ord (HList (x ': xs)) where
    compare (x:::xs) (y:::ys) = 
        case compare x y of
             EQ -> compare xs ys
             LT -> LT
             GT -> GT

instance Monoid (HList '[]) where
    mempty = HNil
    HNil `mappend` HNil = HNil
instance (Monoid x, Monoid (HList xs)) => Monoid (HList (x ': xs)) where
    mempty = mempty:::mempty
    (x:::xs) `mappend` (y:::ys) = (x `mappend` y):::(xs `mappend` ys)

-- | Typeable is scary and I don't understand what's going on.  Hopefully this is correct :)

{-# NOINLINE hlistTyCon #-}
hlistTyCon :: [TypeRep] -> TyCon
hlistTyCon xs = mkTyCon3 "vector-heterogenous" "Data.Vector.Heterogenous.HList" ("HList '"++show xs)

instance (TypeList (HList xs)) => Typeable (HList xs) where
    typeOf _ = mkTyConApp (hlistTyCon $ typeList (undefined::HList xs)) []
    
class TypeList t where
    typeList :: t -> [TypeRep]
    
instance TypeList (HList '[]) where
    typeList _ = []
instance (TypeList (HList xs), Typeable x) => TypeList (HList (x ': xs)) where
    typeList _ = (typeOf (undefined::x)):(typeList (undefined::HList xs))

-- | Used only for the HList class to determine its length

class HLength xs where
    hlength :: xs -> Int
instance HLength (HList '[]) where
    hlength _ = 0
instance (HLength (HList xs)) => HLength (HList (x ': xs)) where
    hlength (x:::xs) = 1+hlength xs

-- | For converting into a list

class HList2List xs a | xs -> a where
    hlist2list :: xs -> [a]
instance HList2List (HList '[]) a where
    hlist2list xs = []
instance (HList2List (HList xs) a) => HList2List (HList (a ':xs)) a where
    hlist2list (x:::xs) = x:(hlist2list xs)    

-- | For construction from lists

class List2HList x xs where
    list2hlist :: [x] -> HList (x ': xs)
    
instance List2HList x '[] where
    list2hlist []       = error "List2HList x HNil: cannot append empty list"
    list2hlist (x:[])   = x:::HNil
    list2hlist _        = error "List2HList x HNil: too many elements in list"

instance (List2HList x xs) => List2HList x (x ': xs) where
    list2hlist []       = error "List2HList x HNil: cannot append empty list"
    list2hlist (x:xs)   = x:::(list2hlist xs)

-- | Equivalent to prelude's "drop"
class HDrop1 n xs1 xs2 | n xs1 -> xs2 where
    hdrop1 :: n -> xs1 -> xs2
    
instance HDrop1 (Nat1Box Zero) (HList xs1) (HList xs1) where
    hdrop1 n xs = xs
    
instance (HDrop1 (Nat1Box n) (HList xs1) (HList xs2)) => 
    HDrop1 (Nat1Box (Succ n)) (HList (x ': xs1)) (HList xs2) where
    hdrop1 _ (x:::xs) = hdrop1 (Nat1Box :: Nat1Box n) xs

-- | Equivalent to prelude's "take"
class HTake1 n xs1 xs2 | n xs1 -> xs2 where
    htake1 :: n -> xs1 -> xs2

instance HTake1 (Nat1Box Zero) (HList xs1) (HList '[]) where
    htake1 _ _ = HNil
    
instance (HTake1 (Nat1Box n) (HList xs1) (HList xs2)) => HTake1 (Nat1Box (Succ n)) (HList (x ': xs1)) (HList (x ': xs2)) where
    htake1 _ (x:::xs) = x:::(htake1 (Nat1Box :: Nat1Box n) xs)

-- | Equivalent to prelude's "map"
class HMap f xs ys | f xs -> ys where
    hmap :: f -> xs -> ys

instance HMap f (HList '[]) (HList '[]) where
    hmap _ _ = HNil

instance 
    ( Function f x y
    , HMap f (HList xs) (HList ys)
    ) => HMap f (HList (x ': xs)) (HList (y ': ys)) where
    hmap f (x:::xs) = function f x:::hmap f xs

-- class HMap f xs1 xs2 | f xs1 -> xs2 where
--     hmap :: f -> xs1 -> xs2
-- 
-- instance HMap f (HList '[]) (HList '[]) where
--     hmap _ HNil = HNil
-- 
-- instance 
--     ( HMap (x1 -> x2) (HList xs1) (HList xs2)
--     ) => HMap (x1 -> x2) (HList (x1 ': xs1)) (HList (x2 ': xs2))
--         where
--     hmap f (x:::xs) = f x ::: hmap f xs
    
-- instance (HTake1 (Nat1Box (ToNat1 n)) (HList xs1) (HList xs2)) => HTake1 (Sing n) (HList xs1) (HList xs2) where
--     htake1 _ xs = htake1 (Nat1Box :: Nat1Box (ToNat1 n)) xs

-- type family HTake (n::Nat) xs
-- type instance HTake n (HList xs) = HList (Take n xs)


-------------------------------------------------------------------------------
-- downcasting HList -> []

class ConstraintBox box a where
    box :: a -> box
    unsafeUnbox :: box -> a
    
class Downcast h box where
    downcast :: h -> [box]
    
    downcastAs :: (a->box) -> h -> [box]
    downcastAs box = downcast

instance Downcast (HList '[]) a where
    downcast HNil = []

instance (ConstraintBox box x, Downcast (HList xs) box) => Downcast (HList (x ': xs)) box where
    downcast (x:::xs) = (box x):(downcast xs)

-------------------------------------------------------------------------------
-- boxes

-- | Most generic box, can be used on any type.
data AnyBox = forall a. AnyBox !a

-- | Use this box unless you know for certain that your types won't have a show instance.
data ShowBox = forall a. (Show a) => ShowBox !a

instance Show ShowBox where
    show (ShowBox a) = show a

instance (Show a) => ConstraintBox ShowBox a where
    box a = ShowBox a
    unsafeUnbox (ShowBox a) = unsafeCoerce a


-------------------------------------------------------------------------------
-- type functions


type family HCons (x :: *) (xs :: *) :: *
type instance HCons x (HList xs) = HList (x ': xs)

type family UnHList (xs :: *) :: [a]
type instance UnHList (HList xs) = xs

type family HAppend xs ys :: *
type instance HAppend (HList xs) (HList ys) = HList (xs ++ ys)

---------------------------------------

type family Distribute (xs::[a->b]) (t::a) :: [b]
type instance Distribute '[] a = '[]
type instance Distribute (x ': xs) a = (x a) ': (Distribute xs a)

type family Replicate (n::Nat) (x::a) :: [a]
type instance Replicate n x = Replicate1 (ToNat1 n) x
type family Replicate1 (n::Nat1) (x::a) :: [a]
type instance Replicate1 Zero x = '[]
type instance Replicate1 (Succ n) x = x ': (Replicate1 n x)

type family Map (f :: a -> b) (xs::[a]) :: [b]
type instance Map f '[] = '[]
type instance Map f (x ': xs) = (f x) ': (Map f xs)

-- type family Length (xs::[a]) :: Nat
-- type instance Length '[] = 0
-- type instance Length (a ': xs) = 1 + (Length xs)

type family Length (xs :: [a]) :: Nat
type instance Length xs = FromNat1 ( Length1 xs )

type family Length1 (xs::[a]) :: Nat1
type instance Length1 '[] = Zero
type instance Length1 (x ': xs) = Succ (Length1 xs)

type family Reverse (xs::[a]) :: [a]
type instance Reverse '[] = '[]
type instance Reverse (x ': xs) = Reverse xs ++ '[x]

type family (:!) (xs::[a]) (i::Nat) :: a
type instance (:!) xs n = Index xs (ToNat1 n)

type family Index (xs::[a]) (i::Nat1) :: a
type instance Index (x ': xs) Zero = x
type instance Index (x ': xs) (Succ i) = Index xs i

type family ($) (f :: a -> b) (a :: a) :: b
type instance f $ a = f a
    
infixr 0 $
     
type family (xs :: [a]) ++ (ys :: [a]) :: [a]
type instance '[] ++ ys = ys
type instance (x ': xs) ++ ys = x ': (xs ++ ys)
     
type family Concat (xs :: [[a]]) :: [a]
type instance Concat '[] = '[]
type instance Concat (x ': xs) = x ++ Concat xs

type Take (n::Nat) (xs :: [*]) = Take1 (ToNat1 n) xs

type family Take1 (n::Nat1) (xs::[*]) :: [*]
type instance Take1 Zero xs = '[]
type instance Take1 (Succ n) (x ': xs) = x ': (Take1 n xs)

---------------------------------------

-- data Nat1Box (n::Nat1) = Nat1Box
-- data Nat1 = Zero | Succ Nat1
--     deriving (Read,Show,Eq,Ord)
-- 
-- type family FromNat1 (n :: Nat1) :: Nat
-- type instance FromNat1 Zero     = 0
-- type instance FromNat1 (Succ Zero) = 1
-- type instance FromNat1 (Succ (Succ Zero)) = 2
-- type instance FromNat1 (Succ (Succ (Succ Zero))) = 3
-- type instance FromNat1 (Succ (Succ (Succ (Succ Zero)))) = 4
-- type instance FromNat1 (Succ (Succ (Succ (Succ (Succ Zero))))) = 5
-- type instance FromNat1 (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))) = 6
-- 
-- type family ToNat1 (n :: Nat) :: Nat1
-- type instance ToNat1 0 = Zero
-- type instance ToNat1 1 = Succ (ToNat1 0)
-- type instance ToNat1 2 = Succ (ToNat1 1)
-- type instance ToNat1 3 = Succ (ToNat1 2)
-- type instance ToNat1 4 = Succ (ToNat1 3)
-- type instance ToNat1 5 = Succ (ToNat1 4)
-- type instance ToNat1 6 = Succ (ToNat1 5)
-- type instance ToNat1 7 = Succ (ToNat1 6)
-- type instance ToNat1 8 = Succ (ToNat1 7)
-- type instance ToNat1 9 = Succ (ToNat1 8)
-- type instance ToNat1 10 = Succ (ToNat1 9)
-- type instance ToNat1 11 = Succ (ToNat1 10)
-- type instance ToNat1 12 = Succ (ToNat1 11)
-- type instance ToNat1 13 = Succ (ToNat1 12)
-- type instance ToNat1 14 = Succ (ToNat1 13)
-- type instance ToNat1 15 = Succ (ToNat1 14)
-- type instance ToNat1 16 = Succ (ToNat1 15)
-- type instance ToNat1 17 = Succ (ToNat1 16)
-- type instance ToNat1 18 = Succ (ToNat1 17)
-- type instance ToNat1 19 = Succ (ToNat1 18)
-- type instance ToNat1 20 = Succ (ToNat1 19)
-- 
-- int2Nat1 :: Int -> Nat1
-- int2Nat1 0 = Zero
-- int2Nat1 i = Succ $ int2Nat1 (i-1)
-- 
-- string_ToNat1 :: Int -> String
-- string_ToNat1 i = "type instance ToNat1 "++show i++" = "++show (int2Nat1 i)
-- 
-- string_FromNat1 :: Int -> String
-- string_FromNat1 i = "type instance FromNat1 "++show (int2Nat1 i)++" = "++show i
