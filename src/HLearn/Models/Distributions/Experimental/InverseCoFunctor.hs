module HLearn.Models.Distributions.InverseCoFunctor
    where
          
import Control.Monad.Random
import qualified Data.Vector as V
import GHC.TypeLits

import HLearn.Algebra
import HLearn.Models.Distributions.Common

import HLearn.Models.Distributions.Univariate.LogNormal
import HLearn.Models.Distributions.Univariate.Normal

-------------------------------------------------------------------------------
-- data types

data InverseCoFunctor basemodel domain = InverseCoFunctor
    { base :: basemodel
    , trans :: domain -> Datapoint basemodel
    }

-------------------------------------------------------------------------------
-- algebra

instance (Monoid basemodel) => Monoid (InverseCoFunctor basemodel domain) where
--     mempty = InverseCoFunctor mempty Nothing
--     mappend x y = InverseCoFunctor
--         { base = base x <> base y
--         , trans = trans x
--         }

---------------------------------------

class CoFunctor f where
    comap :: (a -> b) -> f b -> f a

instance CoFunctor (InverseCoFunctor basemodel) where
    comap f x = InverseCoFunctor
        { base = base x
        , trans = trans x . f
        }

-------------------------------------------------------------------------------
-- training

instance (HasRing basemodel) => HasRing (InverseCoFunctor basemodel domain) where
    type Ring (InverseCoFunctor basemodel domain) = Ring basemodel

instance (HomTrainer basemodel) => HomTrainer (InverseCoFunctor basemodel domain) where
    type Datapoint (InverseCoFunctor basemodel domain) = domain
--     train1dp = InverseCoFunctor . train1dp
--     train    = InverseCoFunctor . train
--     add1dp   (InverseCoFunctor m) x  = InverseCoFunctor $ add1dp   m x
--     addBatch (InverseCoFunctor m) xs = InverseCoFunctor $ addBatch m xs

-------------------------------------------------------------------------------
-- distribution

instance (Probabilistic basemodel) => Probabilistic (InverseCoFunctor basemodel domain) where
    type Probability (InverseCoFunctor basemodel domain) = Probability basemodel

instance (PDF basemodel) => PDF (InverseCoFunctor basemodel domain) where
    pdf (InverseCoFunctor basemodel f) dp = pdf basemodel $ f dp
    
-------------------------------------------------------------------------------
-- testing


normaldata = zip (map (pdf (train [1..100] :: Normal Double Double)) [1..100]) ([1..100])

xs  = normaldata
xs' = map (\(w,x) -> (w,x*x)) xs
n = trainW xs :: Normal Double Double
n' = trainW xs' :: Normal Double Double
n1 = InverseCoFunctor n (sqrt)
n2 = InverseCoFunctor n (+10)