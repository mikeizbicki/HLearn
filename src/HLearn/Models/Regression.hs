module HLearn.Models.Regression
    where

data X

data Add a b

-- type (+) a b = Add a b

type family (+) a b
type instance (+) a b = Add a b

-- train [1,5,2,9,2,-1,6,4] :: Regression (X^2 + X + Log X)
