{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Classification
import HLearn.Models.Classifiers.Bayes
import HLearn.Models.Classifiers.Lame.AdaBoost
import HLearn.Evaluation.CrossValidation

import qualified Control.ConstraintKinds as CK

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

instance Semigroup (V.Vector a) where
    (<>) = mappend

instance 
    ( HomTrainer modelparams datapoint model
    , CK.Functor container
    , CK.FunctorConstraint container model
    , CK.FunctorConstraint container datapoint
    , CK.Foldable container
    , CK.FoldableConstraint container model
    ) => LameTrainer modelparams container datapoint model 
        where
    lame_train' modelparams dataset = train' modelparams dataset


main :: IO ()
main = do
    Right rawdata' <- fmap (decode True) $ BL.readFile "examples/datasets/haberman.data"
        :: IO (Either String (V.Vector (Double, Double, Double, Int)))
    let rawdata = V.map (\(a1,a2,a3,l) -> (l,(a1:::a2:::a3))) rawdata'
        
    let model = train rawdata :: Bayes (Moments:::.Moments:::.Moments) Int Double
    putStr "model="
    print model

    let cv = {-lame_-}crossvalidation 
{-                (AdaBoostParams (Weighted $ BayesParams $ (MomentsParams:::MomentsParams:::MomentsParams)
                    :: Weighted (BayesParams ((MomentsParams:::.MomentsParams:::.MomentsParams) Double))
                    )) -- -}
--                 (undefined :: AdaBoostParams (Weighted (BayesParams ((GaussianParams:::.GaussianParams:::.GaussianParams) Double) )) Double)
                (undefined :: BayesParams ((GaussianParams:::.GaussianParams:::.GaussianParams) Double))
                rawdata 
                accuracy 
                10
                
    putStr "cv="
    print cv
    
    putStrLn "Done."
    
{-instance (HLearn.Models.Classification.ProbabilityClassifier
         (RegSG2Group
            (Bayes'
               ((GaussianParams Double ::: GaussianParams Double)
                ::: GaussianParams Double)
               ((Gaussian Double ::: Gaussian Double) ::: Gaussian Double)
               Int
               Double))
         ((Double ::: Double) ::: Double)
         Int
         Double)
                       where
    probabilityClassify (SGJust bayes) dp = 
        Categorical $ Map.mapWithKey (\label dist -> pdf dist dp) $ attrDist bayes-}
   

-- test = crossValidation 
--         (BayesParams $ MultivariateParams 
--             (  (GaussianParams :: GaussianParams Double)
--             :::(GaussianParams :: GaussianParams Double)
--             :::(GaussianParams :: GaussianParams Double)
--             ))
--         ([]:: [(Int,(Double:::Double:::Double))])
--         accuracy 
--         10
--         :: Gaussian Double