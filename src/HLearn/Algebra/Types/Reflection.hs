{-# LANGUAGE DataKinds,PolyKinds,QuasiQuotes,RankNTypes,MultiParamTypeClasses,FlexibleContexts,UndecidableInstances #-}

module HLearn.Algebra.Types.Reflection
    where

import Data.Constraint
import Data.Constraint.Unsafe
import Data.Reflection
import Data.Proxy

import HLearn.Algebra


-------------------------------------------------------------------------------

newtype Lift (p :: * -> Constraint) (a :: *) (s :: *) = Lift { lower :: a }

class ReifiableConstraint p where
    data Def (p :: * -> Constraint) (a:: *) :: *
    reifiedIns :: Reifies s (Def p a) :- p (Lift p a s)

instance ReifiableConstraint Eq where
    data Def Eq a = Eq { eq_ :: a -> a -> Bool }
    reifiedIns = Sub Dict

instance Reifies s (Def Eq a) => Eq (Lift Eq a s) where
    a == b = eq_ (reflect a) (lower a) (lower b)

instance ReifiableConstraint Ord where
    data Def Ord a = Ord { compare_ :: a -> a -> Ordering }
    reifiedIns = Sub Dict

instance Reifies s (Def Ord a) => Eq (Lift Ord a s) where
    a == b = isEq $ compare_ (reflect a) (lower a) (lower b)
        where
            isEq EQ = True
            isEq _ = False

instance Reifies s (Def Ord a) => Ord (Lift Ord a s) where
    compare a b = compare_ (reflect a) (lower a) (lower b)

instance ReifiableConstraint Monoid where
    data Def Monoid a = Monoid { mappend_ :: a -> a -> a, mempty_ :: a }
    reifiedIns = Sub Dict

instance Reifies s (Def Monoid a) => Monoid (Lift Monoid a s) where
    mappend a b = Lift $ mappend_ (reflect a) (lower a) (lower b) 
    mempty = a where a = Lift $ mempty_ (reflect a)

asProxyOf :: f s -> Proxy s -> f s
asProxyOf v _ = v

with :: Def p a -> (forall s. Reifies s (Def p a) => Lift p a s) -> a
with d v = reify d (lower . asProxyOf v) 

using :: forall p a. ReifiableConstraint p => Def p a -> (p a => a) -> a
using d m = reify d $ \(_ :: Proxy s) ->
    let replaceProof :: Reifies s (Def p a) :- p a
        replaceProof = trans proof reifiedIns
            where proof = unsafeCoerceConstraint :: p (Lift p a s) :- p a
    in m \\ replaceProof
    
using2 :: (ReifiableConstraint p1, ReifiableConstraint p2) => 
    (Def p1 a, Def p2 a) -> ((p1 a, p2 a) => a) -> a
using2 (p1,p2) f = using p1 $ using p2 $ f

---------------------------------------

class SetParam p m where
    data DefParam p m :: *
    setParam :: DefParam p m -> (p m => m) -> m

a = DefParam_ParamA . ParamA
instance SetParam ParamA (ReflectionTest Nothing b) where
    data DefParam ParamA (ReflectionTest Nothing b) = 
            DefParam_ParamA { unDefParam :: Def ParamA (ReflectionTest Nothing b) }
    setParam p a = using (unDefParam p) a

b = DefParam_ParamB . ParamB
instance SetParam ParamB (ReflectionTest a Nothing) where
    data DefParam ParamB (ReflectionTest a Nothing) = 
            DefParam_ParamB { unDefParamB :: Def ParamB (ReflectionTest a Nothing ) }
    setParam p a = using (unDefParamB p) a


setParam2 :: (SetParam p1 m, SetParam p2 m) => 
    (DefParam p1 m, DefParam p2 m) -> ((p1 m, p2 m) => m) -> m
setParam2 (p1,p2) f = setParam p1 $ setParam p2 $ f

---------------------------------------

data ReflectionTest (a::Maybe Nat) (b::Maybe Nat) = ReflectionTest Int Int Int
    deriving (Read,Show,Eq,Ord)

instance (ParamA (ReflectionTest a b), ParamB (ReflectionTest a b)) => Monoid (ReflectionTest a b) where
    mempty = ReflectionTest a b $ a+b
        where
            a = paramA (undefined::ReflectionTest a b)
            b = paramB (undefined::ReflectionTest a b)
    mappend a b = a

instance (ParamA (ReflectionTest a b), ParamB (ReflectionTest a b)) => HomTrainer (ReflectionTest a b) where
    type Datapoint (ReflectionTest a b) = ()
    train1dp dp = mempty

class ParamA p where paramA :: p -> Int

instance ReifiableConstraint ParamA where
    data Def ParamA a = ParamA { paramA_ :: Int }
    reifiedIns = Sub Dict

instance Reifies s (Def ParamA a) => ParamA (Lift ParamA a s) where
    paramA a = paramA_ (reflect a)

class ParamB p where paramB :: p -> Int

instance ReifiableConstraint ParamB where
    data Def ParamB a = ParamB { paramB_ :: Int }
    reifiedIns = Sub Dict

instance Reifies s (Def ParamB a) => ParamB (Lift ParamB a s) where
    paramB a = paramB_ (reflect a)

mkTest :: (ParamA (ReflectionTest a b), ParamB (ReflectionTest a b)) => ReflectionTest a b
mkTest = train1dp ()

instance SingI a => ParamA (ReflectionTest (Just a) b) where
    paramA _ = fromIntegral $ fromSing (sing :: Sing a)

instance SingI b => ParamB (ReflectionTest a (Just b)) where
    paramB _ = fromIntegral $ fromSing (sing :: Sing b)

-------------------------------------------------------------------------------

{-
class Param_Reg_eq (domain:: *) where
    param_Reg_eq :: domain -> Int

instance Param_Reg_eq (Sing 1) where
    param_Reg_eq _ = 1

instance Param_Reg_eq (Sing 2) where
    param_Reg_eq _ = 2

-- ex :: Int
-- ex = param_Reg_eq (undefined :: Sing 1)
--    + param_Reg_eq (undefined :: Sing 2)

newtype O a (s:: *) = O { runO :: a }
    deriving (Read,Show)

instance Monoid a => Monoid (O a s) where
    mempty = O mempty
    mappend (O a) (O b) = O $ a<>b

instance HomTrainer a => HomTrainer (O a s) where
    type Datapoint (O a s) = Datapoint a
    train1dp dp = O $ train1dp dp

newtype Ord_ a = Ord_ { compare_ :: a -> a -> Ordering }

isEq :: Ordering -> Bool
isEq EQ = True
isEq _ = False

instance Reifies s (Ord_ a) => Eq (O a s) where
    a == b = isEq $ compare_ (reflect a) (runO a) (runO b)

instance (Eq (O a s), Reifies s (Ord_ a)) => Ord (O a s) where
    compare a b = compare_ (reflect a) (runO a) (runO b)

withOrd :: (a -> a -> Ordering) -> (forall s. Reifies s (Ord_ a) => O a s) -> a
withOrd f v = reify (Ord_ f) (runO . asProxyOf v)

asProxyOf :: f s -> Proxy s -> f s
asProxyOf v _ = v

newtype Param_Reg_eq_ a = Param_Reg_eq_ { param_Reg_eq_ :: a -> Int }

instance Reifies s (Param_Reg_eq_ a) => Param_Reg_eq (O a s) where
    param_Reg_eq a = param_Reg_eq_ (reflect a) (runO a)

setParamEq :: Int -> (forall s. Reifies s (Param_Reg_eq_ a) => O a s) -> a
setParamEq f v = reify (Param_Reg_eq_ (\a -> f)) (runO . asProxyOf v)
--     where
--         asProxyOf :: f s -> Proxy s -> f (s::k)
--         asProxyOf v _ = v
-- setParamEq' :: Int -> a -> a
setParamEq' i f = setParamEq i . f 

newtype ReflectionTest (n::Nat) = ReflectionTest { unRefl :: Int }
    deriving (Read,Show,Eq,Ord)

class ParamA p where
    paramA :: p -> Int

-- newtype ParamA_ a = ParamA_ { paramA_ :: a -> Int }
-- 
-- instance Reifies s (ParamA_ a) => ParamA (O a s) where
--     paramA a = paramA_ (reflect a) (runO a)
-- 
-- withParamA :: (a -> Int) -> (forall s. Reifies s (ParamA_ a) => O a s) -> a
-- withParamA f v = reify (ParamA_ f) (runO . asProxyOf v)

-- instance SingI n => ParamA (ReflectionTest n) where
--     paramA _ = fromIntegral $ fromSing (sing :: Sing n)

instance ParamA (ReflectionTest n) => Monoid (ReflectionTest n) where
    mempty = ReflectionTest $ paramA (undefined::ReflectionTest n)
    mappend a b = a

instance ParamA (ReflectionTest n) => HomTrainer (ReflectionTest n) where
    type Datapoint (ReflectionTest n) = ()
    train1dp dp = mempty

ex = setParamEq 10 $ O 1
-- ex = withOrd (flip compare) $ head $ sort [ (O 1),  (O 2), O 3 ]

example :: Int
example = reify 10 $ \p -> reflect p + reflect p

-- newtype S a s = S { unS :: a }
-- 
-- newtype Sing_ (a::Frac) = Sing_ a
-- 
-- instance Reifies s (Sing_ a) => SingI a
--     sing _ 
-}
