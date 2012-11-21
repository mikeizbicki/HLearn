import Criterion.Main

-- import qualified Control.ConstraintKinds as CK

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as G

import HLearn.Algebra
import HLearn.Models.Classifiers.NBayes

import HLearn.DataContainers

dp = (1::Int,[(0::Int,Discrete "poop"), (1::Int,Continuous 5)])

desc = DataDesc
    { numLabels = 3
    , labelL = [0,1,2]
    , numAttr = 5
    }
    

-- main = defaultMain 
--     [ 
--     ]