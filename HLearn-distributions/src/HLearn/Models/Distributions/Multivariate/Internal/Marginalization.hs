-- | Marginalization is a tricky procedure involving a lot of type hackery.  All user-facing interfaces use the "Marginalize" class, and all internal interfaces use the "Marginalize'" class.  Essentially, "TypeLens" class converts the "Type lens" for our user's data type into a type level number.  The  "Marginalize'" class then takes this type number and uses it as an index into an appropriate type list that represents the data type.
--
-- The TemplateHaskell module has more info.

module HLearn.Models.Distributions.Multivariate.Internal.Marginalization
    where

import GHC.TypeLits
import HLearn.Algebra
import HLearn.Models.Distributions.Common

-------------------------------------------------------------------------------
-- external type classes

class 
    ( Marginalize' (dist `DepIndexResult` index) dist
    , DepIndex dist index
    ) => Marginalize index dist 
        where

    type Margin index dist 
    getMargin :: index -> dist -> Margin index dist
    
    type MarginalizeOut index dist 
    marginalizeOut :: index -> dist -> MarginalizeOut index dist
    
    condition :: index 
              -> Datapoint (Margin' (dist `DepIndexResult` index) dist) 
              -> dist 
              -> MarginalizeOut' (dist `DepIndexResult` index) dist

instance 
    ( Marginalize' (dist `DepIndexResult` index) dist
    , DepIndex dist index
    ) => Marginalize index dist 
        where
    
    type Margin index dist = Margin' (dist `DepIndexResult` index) dist
    getMargin _ dist = getMargin' (undefined :: (dist `DepIndexResult` index)) dist
    
    type MarginalizeOut index dist = MarginalizeOut' (dist `DepIndexResult` index) dist
    marginalizeOut _ dist = marginalizeOut' (undefined :: (dist `DepIndexResult` index)) dist
    condition _ dp dist = condition' (undefined :: (dist `DepIndexResult` index)) dist dp

-------------------------------------------------------------------------------
-- internal type classes

-- type family GetIndex (xs::[a]) (val::a) :: Nat1
-- type instance GetIndex (x ': xs) x = Zero
-- type instance GetIndex (x ': xs) y = Succ (GetIndex xs y)

class Marginalize' index dist where
    type Margin' index dist
    getMargin' :: index -> dist -> Margin' index dist
    
    type MarginalizeOut' index dist 
    marginalizeOut' :: index -> dist -> MarginalizeOut' index dist
    
    condition' :: index -> dist -> Datapoint (Margin' index dist) -> MarginalizeOut' index dist
    
--     conditionAllButOne :: index -> dist -> Datapoint dist -> MarginalizeOut index dist
