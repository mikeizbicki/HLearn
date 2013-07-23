{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Classifiers
import HLearn.Models.Classifiers.Bayes
import HLearn.Models.Classifiers.Common


data Output = A | B
            deriving (Read, Show, Eq, Ord)

data Point = Point
  { _output   :: Output
  , _features :: [Double]
  } deriving (Read, Show, Eq, Ord)

makeTypeLenses ''Point


instance Labeled Point where
  type Label      Point = Output
  type Attributes Point = Point
  getLabel = _output
  getAttributes p = p


type NB = Bayes TH_output (Multivariate Point
                            '[ MultiCategorical   '[Output]
                             , Independent Normal (Replicate 2 Double)
                             ]
                             Double
                          )


p1 = Point A [1,2]
p2 = Point A [2,3]
p3 = Point B [3,4]
p4 = Point B [2,1]

ps = [p1, p2, p3, p4]

toClassify = Point A [2,2]


-- Train
classifier1 = train ps :: NB


x = classify classifier1 (getAttributes toClassify)
