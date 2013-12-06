module HLearn.Evaluation.CrossValidationData
    where

import GHC.TypeLits
import HLearn.Algebra

data FoldType = LeaveOneOut
              | KFold Nat

data CrossValidation model (fold :: FoldType) = CrossValidation
    { modelL 
