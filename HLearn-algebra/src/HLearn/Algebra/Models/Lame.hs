{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE OverlappingInstances #-}

-- | Lame trainers are trainers that are crippled---They are not instances of Semigroup/Monoid, and training their models is not a homomorphism.  This means we can't do any of the cool manipulations automatically that we can do with the HomTrainer class.

module HLearn.Algebra.Models.Lame
    ( 
    -- * Classes
    LameTrainer (..)
    , LameTrainerOnline (..)
    )
    where
          
import qualified Control.ConstraintKinds as CK
import HLearn.Algebra.Models

class (ModelParams model) => LameTrainer modelparams container datapoint model where
    lame_train' :: Params model -> container datapoint -> model

-- instance 
--     ( HomTrainer modelparams datapoint model
--     , CK.Functor container
--     , CK.FunctorConstraint container model
--     , CK.FunctorConstraint container datapoint
--     , CK.Foldable container
--     , CK.FoldableConstraint container model
--     ) => LameTrainer modelparams container datapoint model 
--         where
--     lame_train' modelparams dataset = train' modelparams dataset

{-    lame_train :: 
        ( CK.Functor container
        , CK.FunctorConstraint container model
        , CK.FunctorConstraint container datapoint
        , CK.Foldable container
        , CK.FoldableConstraint container model
        , CK.FoldableConstraint container datapoint
        , CK.FoldableConstraint container [datapoint]
        , Sizable container
        ) => modelparams -> container datapoint -> model-}
    
class (ModelParams model) => LameTrainerOnline datapoint model where
    lame_add1dp :: model -> datapoint -> model
    
    lame_addBatch :: model -> [datapoint] -> model
    lame_addBatch = add1dp2addBatch lame_add1dp
        
add1dp2addBatch :: (model -> dp -> model) -> (model -> [dp] -> model)
add1dp2addBatch add1dp = \model xs -> case xs of
    []   -> model
    x:xs -> (add1dp2addBatch add1dp) (add1dp model x) xs
