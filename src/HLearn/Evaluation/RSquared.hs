module HLearn.Evaluation.RSquared
    where

import HLearn.Algebra
import HLearn.Models.Classifiers.Common

rsquared :: 
    ( Floating (Ring model)
    , NumDP model
    , Regression model
    ) => model -> [Datapoint model] -> Ring model
rsquared m xs = 1-ss_err/ss_tot
    where
        ss_err = sum $ map (\dp -> ((getLabel dp)-(classify m (getAttributes dp)))^^2) xs
        ss_tot = sum $ map (\dp -> ((getLabel dp)-yave)^^2) xs
        yave = (sum $ map getLabel xs)/(numdp m)
