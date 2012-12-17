{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module HLearn.Algebra.FunctionTypes
    where

import Data.Semigroup
import GHC.TypeLits


-- class Surjective input output function
-- class Injective input output function
-- class (Surjective input output function, Injective input output function) => 
--     Bijective input output function
-- 
-- class (objtype input, objtype output) => 
--     Homomorphism (objtype :: * -> Constraint) input output function
-- 
-- class ( Surjective input output function, Homomorphism objtype input output function) => 
--     Epimorphism objtype input output function
-- 
-- class PseudoInverse inverse function
-- class Inverse inverse function
-- 
-- instance (Inverse inverse function) => PseudoInverse inverse function


--

-- class Arity0 f a where
--     eval0 :: f -> a
-- 
-- newtype Arity0Box a = ArityBox a
-- instance Arity0

--

class Arity1 f a b where
    eval1 :: f -> a -> b

-- class Arity1 f a b where
--     eval1 :: f -> a -> b
-- 
-- class (Arity1 f a b) => Homomorphism f a b
-- class (Arity1 f a b) => Surjective f a b
-- class (Arity1 f a b) => Injective f a b
-- 
-- newtype Arity1Box a b = Arity1Box (a->b)
-- instance Arity1 (Arity1Box a b) a b where
--     eval1 (Arity1Box f) = f
-- 
-- instance (Arity1 fg a c, Homomorphism f a b, Homomorphism g b c) => Homomorphism fg a c
-- 
-- --
-- 
-- class Arity2 f a b c where
--     eval2 :: f -> a -> b -> c
-- 
-- class (Arity2 f a b c) => Associative f a b c
-- class (Arity2 f a b c) => Commutative f a b c
-- 
-- newtype Arity2Box a b c = Arity2Box (a->b->c)
-- 
-- instance Arity2 (Arity2Box a b c) a b c where
--     eval2 (Arity2Box f) = f
-- --
-- 
-- data Add = Add
-- instance (Num n) => Arity2 Add n n n where
--     eval2 Add = (+)
-- instance (Num n) => Associative Add n n n
-- instance (Num n) => Commutative Add n n n
-- 
-- data Sub = Sub
-- instance (Num n) => Arity2 Sub n n n where
--     eval2 Sub = \x y -> eval2 Add x (eval1 Negate (y::n) :: n)
-- 
-- data SGOp = SGOp
-- instance (Semigroup sg) => Arity2 SGOp sg sg sg where
--     eval2 SGOp = (<>)
-- 
-- -- 
-- 
-- data Negate = Negate
-- instance (Num n) => Arity1 Negate n n where
--     eval1 Negate = negate 
-- instance (Num n) => Homomorphism Negate n n
-- instance (Num n) => Surjective Negate n n
-- instance (Num n) => Injective Negate n n
-- 
-- data Length = Length
-- instance Arity1 Length [a] Int where
--     eval1 Length = length
-- instance Homomorphism Length [a] Int
-- instance Surjective Length [a] Int
-- 
-- data Reduce = Reduce
-- instance (Semigroup sg) => Arity1 Reduce [sg] sg where
--     eval1 Reduce = reduce
-- instance (Semigroup sg) => Homomorphism Reduce [sg] sg
-- 
-- reduce :: (Semigroup sg) => [sg] -> sg
-- reduce []  = error "reduce: cannot reduce empty list"
-- reduce [x] = x
-- reduce xs  = reduce $ itr xs
--     where
--         itr :: (Semigroup sg) => [sg] -> [sg]
--         itr []            = []
--         itr [x]           = [x]
--         itr (x1:x2:xs)    = (x1<>x2):(itr xs)
-- 
-- instance Semigroup Int where
--     (<>) = (+)
-- 
-- -- data Parallelize 
-- -- 
-- -- parallel :: 
-- --     ( Monoid model
-- --     , NFData model
-- --     , CK.Partitionable container
-- --     , CK.PartitionableConstraint container datapoint
-- --     , CK.FoldableConstraint container model
-- --     , CK.Functor container
-- --     , CK.FunctorConstraint container model
-- --     , CK.FunctorConstraint container datapoint
-- --     , CK.Applicative container
-- --     , CK.ApplicativeConstraint container datapoint
-- --     , CK.Traversable container
-- --     , CK.TraversableConstraint container model
-- --     , CK.TraversableConstraint container datapoint
-- --     ) => (container datapoint -> model) -> (container datapoint -> model)
-- -- parallel train = \datapoint ->
-- --     F.foldl' (mappend) mempty $ parMap strat train (CK.partition n datapoint)
-- --     where
-- --         strat = rdeepseq
-- --         n = unsafePerformIO $ getNumCapabilities
