{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module HLearn.Algebra.Functions
    where

import Control.Applicative
import Control.Parallel.Strategies
import Data.Traversable
import qualified Data.Foldable as F
import GHC.Exts (Constraint)

import HLearn.Algebra.Models
import HLearn.Algebra.Structures

-------------------------------------------------------------------------------
-- Functions

class Surjective input output function
class Injective input output function
class (Surjective input output function, Injective input output function) => 
    Bijective input output function

class (objtype input, objtype output) => 
    Homomorphism (objtype :: * -> Constraint) input output function

class ( Surjective input output function, Homomorphism objtype input output function) => 
    Epimorphism objtype input output function

class PseudoInverse inverse function
class Inverse inverse function

instance (Inverse inverse function) => PseudoInverse inverse function

-------------------------------------------------------------------------------
-- 2-morphisms

toOnline :: 
    ( Model modelparams model
    , Epimorphism Semigroup inputdata model (modelparams -> inputdata -> model)
    ) => 
    (modelparams -> inputdata -> model) -> (model -> inputdata -> model)
toOnline train = \model inputdata -> model <> (train (params model) inputdata)

toSemigroup :: 
    ( Semigroup inputdata
    , PseudoInverse (model -> inputdata) (modelparams -> inputdata -> model)
    ) =>
    (model -> inputdata -> model) -> (model -> inputdata) -> (model -> model -> model)
toSemigroup trainonline pseudoinverse = \model1 model2 -> trainonline model1 (pseudoinverse model2)

toParallel :: 
    ( Homomorphism Semigroup datapoint model (modelparams -> datacontainer datapoint -> model)
    , Applicative datacontainer
    , Traversable datacontainer
    ) => 
    (modelparams -> datacontainer datapoint -> model) 
        -> Strategy model 
        -> (modelparams -> datacontainer datapoint -> model)
        
toParallel train strat = \modelparams inputdata -> 
    F.foldl1 (<>) $ (withStrategy (parTraversable strat) . fmap (train modelparams . pure)) inputdata