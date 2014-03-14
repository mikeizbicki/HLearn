module HLearn.Algebra.Structures.Monad
    ( Functor (..)
    , ValidFunctor
    , (<$>)
    , Applicative (..)
    , ValidApplicative
    , Monad (..)
    , ValidMonad
    , ifThenElse
    , module Prelude
    )
    where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS

import GHC.Prim
import Prelude hiding (Functor(..), Monad(..))

-------------------------------------------------------------------------------

ifThenElse a b c = if a then b else c

-------------------------------------------------------------------------------

class Functor f where
    type C_fmap_a f a :: Constraint
    type C_fmap_a f a = ()
    type C_fmap_b f b :: Constraint
    type C_fmap_b f b = ()
    fmap :: (C_fmap_a f a, C_fmap_b f b) => (a -> b) -> f a -> f b

type ValidFunctor f a = 
    ( Functor f
    , C_fmap_a f a
    , C_fmap_b f a
    )

---------------------------------------

(<$>) :: (ValidFunctor f a, ValidFunctor f b) => f a -> (a -> b) -> f b
(<$>) = flip fmap

---------------------------------------

instance Functor [] where
    fmap = map

instance Functor V.Vector where
    fmap = V.map

instance Functor VU.Vector where
    type C_fmap_a VU.Vector a = VU.Unbox a
    type C_fmap_b VU.Vector b = VU.Unbox b
    fmap = VU.map

instance Functor VS.Vector where
    type C_fmap_a VS.Vector a = VS.Storable a
    type C_fmap_b VS.Vector b = VS.Storable b
    fmap = VS.map 

-------------------------------------------------------------------------------

class Functor f => Applicative f where
    type C_pure f a :: Constraint
    type C_pure f a = ()
    pure :: C_pure f a => a -> f a

    type C_ap_a f a :: Constraint
    type C_ap_a f a = ()
    type C_ap_b f b :: Constraint
    type C_ap_b f b = ()
    (<*>) :: (C_ap_a f a, C_ap_b f b) => f (a -> b) -> f a -> f b
--     (*>) :: (C_ap_a f a, C_ap_b f b) => f a -> f b -> f b
--     (<*) :: (C_ap_a f a, C_ap_b f b) => f b -> f a -> f b

type ValidApplicative f a = 
    ( ValidFunctor f a
    , Applicative f
    , C_pure f a
    , C_ap_a f a
    , C_ap_b f a
    )

---------------------------------------

(<**>) :: (ValidApplicative f a, ValidApplicative f b) => f a -> f (a -> b) -> f b
(<**>) = flip (<*>)

liftA2 f a b = f <$> a <*> b

---------------------------------------

instance Applicative [] where
    pure a = [a]
    fs <*> xs = concat $ map (\f -> map f xs) fs

instance Applicative V.Vector where
    pure a = V.singleton a
    fs <*> xs = V.fromList $ V.toList fs <*> V.toList xs

-------------------------------------------------------------------------------

class Applicative f => Monad f where

    fail :: String -> f a
    fail = error

    type C_return f a :: Constraint
    type C_return f a = C_pure f a
    return :: C_return f a => a -> f a

    type C_join f a :: Constraint
    type C_join f a = ()
    join :: C_join f a => f (f a) -> f a

    type C_bind_a f a :: Constraint
    type C_bind_a f a = ()
    type C_bind_b f b :: Constraint
    type C_bind_b f b = ()
    (>>=) :: (C_bind_a f a, C_bind_b f b) => f a -> (a -> f b) -> f b

    {-# INLINE (>>) #-}
    (>>) :: (C_bind_a f a, C_bind_b f b) => f a -> f b -> f b
    m >> k = m >>= \_ -> k

type ValidMonad f a =
    ( ValidApplicative f a
    , C_fmap_b f (f a)
    , Monad f
    , C_return f a
    , C_join f a
    , C_bind_a f a
    , C_bind_b f a
    )

---------------------------------------

{-# INLINE default_bind #-}
default_bind :: (ValidMonad f a, ValidMonad f b) => f a -> (a -> f b) -> f b
default_bind m f = join $ fmap f m

---------------------------------------

instance Monad [] where
    return = pure
    join = concat
    (>>=) = default_bind

instance Monad V.Vector where
    return = pure
    join v = V.fromList $ concat $ V.toList $ fmap (V.toList) v
    (>>=) = default_bind
