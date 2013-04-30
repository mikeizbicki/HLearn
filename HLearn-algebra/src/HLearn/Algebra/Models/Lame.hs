{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE OverlappingInstances #-}

-- | Lame trainers are trainers that are crippled---They are not Monoids, and training their models is not a homomorphism.  This means we can't do any of the cool manipulations automatically that we can do with the HomTrainer class.  These classes are provided mostly for development and testing purposes.  It is not recommended that you use any of their instances.

module HLearn.Algebra.Models.Lame
    ( 
    -- * Classes
    LameTrainer (..)
    , LameTrainerOnline (..)
    )
    where
          
import HLearn.Algebra.Models.HomTrainer

-- | Provides a non-homomorphic batch trainer
class LameTrainer model where
    type LameDatapoint model :: *
    type LameContainer model :: * -> *
    
    lame_train :: (LameContainer model) (LameDatapoint model) -> model

instance 
    ( HomTrainer model
    ) => LameTrainer model 
        where
    type LameDatapoint model = Datapoint model
    type LameContainer model = []
    
    lame_train dataset = train dataset

-- | Provides a non-homomorphic online trainer
class LameTrainerOnline model where
    type LameDatapointOnline model :: *
    lame_add1dp :: model -> LameDatapointOnline model -> model
    
--     lame_addBatch :: model -> [datapoint] -> model
--     lame_addBatch = add1dp2addBatch lame_add1dp
--         
-- add1dp2addBatch :: (model -> dp -> model) -> (model -> [dp] -> model)
-- add1dp2addBatch add1dp = \model xs -> case xs of
--     []   -> model
--     x:xs -> (add1dp2addBatch add1dp) (add1dp model x) xs
