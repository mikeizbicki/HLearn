{-# LANGUAGE ConstraintKinds,Rank2Types,KindSignatures #-}
module HLearn.Evaluation.CrossValidationHom
    where

import qualified Data.Vector as V
import qualified Data.Sequence as Seq

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common

import HLearn.DataStructures.CoverTree
import HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor
import HLearn.Metrics.Lebesgue
import qualified Data.Vector.Unboxed as VU

-------------------------------------------------------------------------------
-- data types

data CrossValidation model result (infer:: k) lossfunction lossresult = CrossValidation
    { dataL   :: Seq.Seq (Datapoint model)
    , resultL :: Seq.Seq result
    , lossL   :: Seq.Seq lossresult
    , loss    :: Normal lossresult lossresult
    , model   :: model
    }

type family Get_Model (a:: *)
type instance Get_Model (CrossValidation model result infer lossfunction lossresult) = model

type family Set_Model (a:: *) (model':: *)
type instance Set_Model (CrossValidation model result infer lossfunction lossresult) model' = 
    CrossValidation model' result infer lossfunction lossresult


type DP = L2 (VU.Vector Double)
type Test = CrossValidation (CoverTree DP) (KNN2 1 DP) KNN ErrorRate Double 

---------------------------------------

data InferType (a::a)

instance
    ( HomTrainer model
    , PDF model
    , dp ~ Datapoint model
    , prob ~ Probability model
    ) => Function (InferType PDF) (model,dp) prob where
    function _ (m,dp) = pdf m dp

function2 a = curry $ function a

---------------------------------------

data SquaredLoss 
instance Num a => Function SquaredLoss (a,a) a where
    function _ (result,target) = (target-result)*(target-result)

data ErrorRate
instance (Num a, Eq a) => Function ErrorRate (a,a) a where
    function _ (result,target) = indicator $ target==result

-- -------------------------------------------------------------------------------
-- algebra

instance
    ( HomTrainer model
    , Labeled (Datapoint model)
    , Monoid result
    , Function (InferType infertype) (model,Datapoint model) result
    , Function lossfunction (Label (Datapoint model),Label (Datapoint model)) lossresult
    , result ~ Label (Datapoint model)
    , Num lossresult
    ) => Monoid (CrossValidation model result infertype lossfunction lossresult)
        where
    mempty = CrossValidation 
        { dataL   = mempty 
        , resultL = mempty 
        , lossL   = mempty
        , loss    = mempty
        , model   = mempty
        }

    mappend cv1 cv2 = CrossValidation
        { dataL   = dataL'
        , resultL = resultL'
        , lossL   = lossL' 
        , loss    = train lossL'
        , model   = model cv1 <> model cv2
        }
        where
            dataL'   = dataL cv1 <> dataL cv2
            resultL' = resultL1 <> resultL2
            lossL'   = fmap loss $ Seq.zip resultL' $ fmap getLabel dataL'
            resultL1 = fmap (\(x,r) -> infer (model cv2) x <> r) $ Seq.zip (dataL cv1) (resultL cv1)
            resultL2 = fmap (\(x,r) -> infer (model cv1) x <> r) $ Seq.zip (dataL cv2) (resultL cv2)

            infer = function2 (undefined :: InferType infertype)
            loss  = function (undefined :: lossfunction)

-- data CoverTree
-- data KNN
-- 
-- data UndefinedModel
-- 
-- type DefaultCV = CrossValidation UndefinedModel KNN KNN ErrorRate Double
-- 
-- train xs :: DefaultCV [tlr| model=NearestNeighbor [tlr| k=5 |], lossfunction=ErrorRate |] 

instance 
    ( Monoid (CrossValidation model result infertype lossfunction lossresult)
    , HomTrainer model
    , Num lossresult
    ) => HomTrainer (CrossValidation model result infertype lossfunction lossresult)
        where

    type Datapoint (CrossValidation model result infertype lossfunction lossresult) = Datapoint model

    train1dp dp = CrossValidation
        { dataL   = Seq.singleton dp
        , resultL = mempty
        , lossL   = mempty
        , loss    = mempty
        , model   = train1dp dp
        }

-------------------------------------------------------------------------------
-- Junk

class (Monoid model) => Hom container model where
    type HomDP model :: *
-- type HomDomain model :: * -> *

    hom1dp :: HomDP model -> model
    hom :: container (HomDP model) -> model
    homAdd1dp :: model -> HomDP model -> model
    homAddBatch :: model -> container (HomDP model) -> model


-- class LossFunction f where
--     loss :: f -> model -> Datapoint model -> Ring f
