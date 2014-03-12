-- | Modules are a generalization of vector spaces

module HLearn.Algebra.Structures.Modules
    where

import Data.List
import HLearn.Algebra.Structures.Groups

-------------------------------------------------------------------------------
-- Modules

class Num (Ring m) => HasRing m where
    type Ring m

-- | Bug: The module classes have the constraint that r be of type Num.  Technically, this should be a Ring.  But creating a Ring class would be awkward because it would conflict with the Num class and require importing a different Prelude.
class (HasRing m, Abelian m, Group m) => Module m where    
    infix 7 .*
    (.*) :: Ring m -> m -> m
    
    {-# INLINE (*.) #-}
    infix 7 *.
    (*.) :: m -> Ring m -> m
    m *. r = r .* m

-------------------------------------------------------------------------------
-- Vector Spaces

class (Module m, Fractional (Ring m)) => VectorSpace m where
    infix 7 /.
    {-# INLINE (/.) #-}
    (/.) :: m -> Ring m -> m
    m /. r = m *. (1/r)

instance (Module m, Fractional (Ring m)) => VectorSpace m

-------------------------------------------------------------------------------
-- Inner Product spaces

class VectorSpace v => InnerProduct v where
    inner :: v -> v -> Ring v

