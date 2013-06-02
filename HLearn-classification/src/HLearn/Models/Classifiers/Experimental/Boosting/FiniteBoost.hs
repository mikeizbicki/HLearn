module HLearn.Models.Classifiers.Experimental.Boosting.FiniteBoost
    where

import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common

-------------------------------------------------------------------------------
-- data structures

class FiniteBoostParams p where
    type BaseModel p
    getModelL :: p -> [BaseModel p]
    numModels :: p -> Int

newtype FiniteBoost params = FiniteBoost
    { weights :: V.Vector (Ring (BaseModel params))
    }
--     deriving (Read,Show,Eq,Ord)

-- type FiniteBoost weight basemodel = RegSG2Group (FiniteBoost' weight basemodel)

-------------------------------------------------------------------------------
-- algebra

instance
    ( FiniteBoostParams params
    , HasRing (BaseModel params)
    ) => Monoid (FiniteBoost params) 
        where
    mempty = FiniteBoost $ V.replicate (numModels (undefined::params)) 0
    b1 `mappend` b2 = FiniteBoost $ V.zipWith (+) (weights b1) (weights b2)

instance 
    ( FiniteBoostParams params
    , HasRing (BaseModel params)
    ) => HasRing (FiniteBoost params) 
        where
    type Ring (FiniteBoost params) = Ring (BaseModel params)

instance     
    ( FiniteBoostParams params
    , HasRing (BaseModel params)
    ) => Group (FiniteBoost params) 
        where
    inverse b = FiniteBoost $ V.map negate (weights b)

-------------------------------------------------------------------------------
-- model
    
instance 
    ( ProbabilityClassifier (BaseModel params)
    , HomTrainer (BaseModel params)
    , FiniteBoostParams params
    , HasRing (BaseModel params)
    , Ring (BaseModel params) ~ Probability (ResultDistribution (BaseModel params))
    , Datapoint (ResultDistribution (BaseModel params)) ~ Label (Datapoint (BaseModel params))
    , PDF (ResultDistribution (BaseModel params))
    ) => HomTrainer (FiniteBoost params)
        where
    type Datapoint (FiniteBoost params) = Datapoint (BaseModel params)
              
    train1dp dp = FiniteBoost
        { weights = V.generate (V.length modelV) (\i -> pdf (probabilityClassify (modelV V.! i) attr) label)
        }
        where
            attr = getAttributes dp
            label = getLabel dp
            modelV = V.fromList $ getModelL (undefined :: params)
    
-------------------------------------------------------------------------------
-- classification

instance 
    ( ProbabilityClassifier (BaseModel params)
    , Ring (BaseModel params) ~ Ring (ResultDistribution (FiniteBoost params))
    , Module (ResultDistribution (FiniteBoost params))
    , FiniteBoostParams params
    ) => ProbabilityClassifier (FiniteBoost params)
        where
    type ResultDistribution (FiniteBoost params) = ResultDistribution (BaseModel params)
              
    probabilityClassify (FiniteBoost weights) dp = 
        reduce $ V.zipWith (.*) weights (fmap (\model -> probabilityClassify model dp) modelL)
        where
            modelL = V.fromList $ getModelL (undefined :: params)