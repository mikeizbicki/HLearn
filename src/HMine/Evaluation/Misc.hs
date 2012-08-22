{-# LANGUAGE FlexibleContexts #-}

module HMine.Evaluation.Misc
    where
          
import qualified Data.Map as Map
          
import HMine.Base
import HMine.Classifiers.TypeClasses
import HMine.DataContainers
          
newtype ConfusionMatrix label = ConfusionMatrix [(label,(Int,Int))]
    deriving (Show,Read,Eq)
    
genConfusionMatrix ::
    ( DataSparse label ds (LDPS label)
    , Classifier model label
    ) =>
    model -> ds (LDPS label) -> ConfusionMatrix label
genConfusionMatrix model lds = ConfusionMatrix . Map.toList $ Map.fromListWith addL $ map mergeL $ zip labelL labelL'
    where
        addL (a1,b1) (a2,b2) = (a1+a2,b1+b2)
        mergeL (l,l') = (l,(indicator $ l==l',indicator $ l/=l'))
        labelL  = map fst $ getDataL lds
        labelL' = map (classify model . snd) $ getDataL lds