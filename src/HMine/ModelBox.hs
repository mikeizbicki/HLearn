module HMine.ModelBox
    where


-------------------------------------------------------------------------------
-- ModelBox

data ModelBox label = 
    forall model .
        ( ProbabilityClassifier model label
        ) => 
    ModelBox model

instance (Label label) => ProbabilityClassifier (ModelBox label) label where
    probabilityClassify (ModelBox model) = probabilityClassify model
