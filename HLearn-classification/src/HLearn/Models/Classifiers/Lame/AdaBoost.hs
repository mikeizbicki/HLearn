{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module HLearn.Models.Classifiers.Lame.AdaBoost
    where

import HLearn.Algebra
import HLearn.Models.Ensemble
import qualified Control.ConstraintKinds as CK

import qualified Data.Vector.Unboxed as VU

-------------------------------------------------------------------------------
-- AdaBoost

data AdaBoostParams basemodelparams prob

-- instance Model (AdaBoostParams basemodelparams prob) (Ensemble (AdaBoostParams basemodelparams prob) model prob) where
instance Model modelparams (Ensemble modelparams model prob) where
    getparams (SGJust ens') = params ens'
    
-------------------------------------------------------------------------------
-- Training

instance 
    ( --HomTrainer modelparams datapoint basemodel
    VU.Unbox prob
    , Fractional prob
    ) => LameTrainer 
        (AdaBoostParams basemodelparams prob) 
        datapoint 
        (Ensemble (AdaBoostParams basemodelparams prob) basemodel prob) 
            where
    
    lame_train params ds = go _D0 _F0
--         let --m = length ds
--             _D0 = VU.replicate m (1/(fromIntegral m))
            {-_F0 =-} 
            
        where
            m = size ds
            _D0 = VU.replicate m (1/fromIntegral m) :: VU.Vector prob
            _F0 = SGJust $ Ensemble' params mempty
            go _D _F = let
                f_t1 = train 
                
