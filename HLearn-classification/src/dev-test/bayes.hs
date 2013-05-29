{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

import HLearn.Algebra
import HLearn.Models.Classifiers
import HLearn.Models.Distributions

data Sex = Male | Female
    deriving (Read,Show,Eq,Ord)

data Human = Human
    { _sex :: Sex
    , _weight :: Double
    , _height :: Double
    , _shoesize :: Double
    }
makeTypeLenses ''Human

instance Labeled Human where
    type Label Human = Sex
    type Attributes Human = Human
    
    getLabel h = _sex h
    getAttributes h = h
    
ds = 
    [ Human Male   6    180 12
    , Human Male   5.92 190 11
    , Human Male   5.58 170 12
    , Human Male   5.92 165 10
    , Human Female 5    100 6
    , Human Female 5.5  150 8
    , Human Female 5.42 130 7
    , Human Female 5.75 150 9
    ]

dp = Human Female 6 130 8

model = train ds :: Bayes TH_sex MultiDist

type MultiDist = Multivariate Human
       '[ MultiCategorical '[Sex]
        , Independent Normal '[Double,Double,Double]
        ] Double

{-dist = train (map snd ds) :: Multivariate (HList '[Double,Double,Double])
   '[ Independent Normal '[Double]
    , Dependent MultiNormal '[Double,Double]
    ]
    Double-}
    