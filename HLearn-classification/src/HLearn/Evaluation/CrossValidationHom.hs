{-# LANGUAGE ConstraintKinds,Rank2Types,KindSignatures #-}
module HLearn.Evaluation.CrossValidationHom
    where

import qualified Data.Vector as V

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common

-------------------------------------------------------------------------------
-- data types

data CrossValidation model result (infer::a) lossfunction = CrossValidation
    { dataL :: V.Vector (Datapoint model)
    , resultL :: V.Vector result
    , model :: model
    }

data Inference (a::a)

instance 
    ( HomTrainer model
    , PDF model
    , dp ~ Datapoint model
    , prob ~ Probability model
    ) => Function (Inference PDF) (model,dp) prob where
    function _ (m,dp) = pdf m dp 

class LossFunction f where
    loss :: f -> model -> Datapoint model -> Ring f

-------------------------------------------------------------------------------
-- algebra

class (Monoid model) => Hom container model where
    type HomDP model :: *
--     type HomDomain model :: * -> *

    hom1dp :: HomDP model -> model
    hom :: container (HomDP model) -> model
    homAdd1dp :: model -> HomDP model -> model
    homAddBatch :: model -> container (HomDP model) -> model

instance 
    ( HomTrainer model
    , Monoid result
    ) => Monoid (CrossValidation model result inference lossfunction) 
        where
    mempty = CrossValidation mempty mempty mempty
    mappend cv1 cv2 = CrossValidation
        { dataL = dataL cv1 <> dataL cv2
        , resultL = resultL1 <> resultL2 
        , model = model cv1 <> model cv2
        }
        where
            resultL1 = fmap (\(x,r) -> infer (model cv2) x <> r) $ V.zip (dataL cv1) (resultL cv1)
--             resultL1 = fmap (\(x,r) -> infer (model cv2) x <> r) $ V.zip (dataL cv1) (resultL cv1)
            resultL2 = fmap (\(x,r) -> infer (model cv1) x <> r) $ V.zip (dataL cv2) (resultL cv2)

-- class Inference model result where
--     infer1dp :: model -> Datapoint model -> result
--     infer :: model -> container (Datapoint model) -> result
    
infer' ::
    ( HomTrainer (model dp)
    , Hom model result
    ) => model dp -> dp -> result
infer' = undefined

infer :: HomTrainer model => model -> Datapoint model -> result
infer = undefined
