-- | Modules are a generalization of vector spaces

module HLearn.Algebra.Structures.Modules
    where

import Data.List
import HLearn.Algebra.Structures.Groups

-------------------------------------------------------------------------------
-- Modules

type family Scalar m :: *
type instance Scalar Int = Int
type instance Scalar Integer = Integer
type instance Scalar Float = Float
type instance Scalar Double = Double
type instance Scalar Rational = Rational

-- | Bug: The module classes have the constraint that r be of type Num.  Technically, this should be a Ring.  But creating a Ring class would be awkward because it would conflict with the Num class and require importing a different Prelude.
class (Abelian m, Group m) => Module m where    
    infixl 7 .*
    (.*) :: Scalar m -> m -> m
    
    {-# INLINE (*.) #-}
    infixl 7 *.
    (*.) :: m -> Scalar m -> m
    m *. r = r .* m

-------------------------------------------------------------------------------
-- Vector Spaces

class 
    ( Module m
    , Module (Scalar m)
    , Fractional (Scalar m)
    ) => VectorSpace m where
    infixl 7 /.
    {-# INLINE (/.) #-}
    (/.) :: m -> Scalar m -> m
    m /. r = m *. (1/r)

-- instance (Module m, Fractional (Scalar m)) => VectorSpace m

-------------------------------------------------------------------------------
-- Inner Product spaces

class VectorSpace v => InnerProduct v where
    inner :: v -> v -> Scalar v

innerProductNorm :: (InnerProduct v, Floating (Scalar v)) => v -> Scalar v
innerProductNorm v = sqrt $ inner v v
