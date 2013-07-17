module HLearn.Models.Regression.Logarithmic
    where

import Control.DeepSeq

import HLearn.Algebra
import HLearn.Models.Classifiers.Common
import HLearn.Evaluation.RSquared

-------------------------------------------------------------------------------
-- data types

-- collect the term of sum in least square formula
data Logarithmic ring = Logarithmic 
    { n      :: !ring -- quantity of sample
    , ylnx   :: !ring
    , y1     :: !ring
    , lnx    :: !ring
    , lnx2   :: !ring -- [ln(x)]^2
    }
    deriving (Read,Show,Eq,Ord)

-- exhaust data
instance (NFData ring) => NFData (Logarithmic ring) where
    rnf pl = deepseq (n pl)
           $ deepseq (ylnx pl)
           $ deepseq (y1 pl)
           $ deepseq (lnx pl)
           $ rnf (lnx2 pl)

-------------------------------------------------------------------------------
-- algebra

instance (Num ring) => Monoid (Logarithmic ring) where
    mempty = Logarithmic 0 0 0 0 0
    a `mappend` b = Logarithmic 
        { n    = n    a + n    b
        , ylnx = ylnx a + ylnx b
        , y1   = y1   a + y1   b
        , lnx  = lnx  a + lnx  b
        , lnx2 = lnx2 a + lnx2 b
        }

instance (Num ring) => Abelian (Logarithmic ring)

instance (Num ring) => Group (Logarithmic ring) where
    inverse a = Logarithmic
        { n    = -(n    a)
        , ylnx = -(ylnx a)
        , y1   = -(y1   a)
        , lnx  = -(lnx  a)
        , lnx2 = -(lnx2 a)
        }

instance (Num ring) => HasRing (Logarithmic ring) where
    type Ring (Logarithmic ring) = ring

instance (Num ring) => Module (Logarithmic ring) where
    r .* pl = Logarithmic
        { n    = r * (n    pl)
        , ylnx = r * (ylnx pl)
        , y1   = r * (y1   pl)
        , lnx  = r * (lnx  pl)
        , lnx2 = r * (lnx2 pl)
        }

-------------------------------------------------------------------------------
-- training

data Coord ring = Coord {x::ring,y::ring}

instance Labeled (Coord ring) where
    type Label (Coord ring)= ring
    type Attributes (Coord ring)= ring
    getLabel = y
    getAttributes = x

instance (Num ring) => NumDP (Logarithmic ring) where
    numdp = n

instance (Floating ring) => HomTrainer (Logarithmic ring) where
    type Datapoint (Logarithmic ring) = Coord ring -- (ring,ring)
    train1dp dp = Logarithmic
        { n    = 1
        , ylnx = y * log x
        , y1   = y
        , lnx  = log x
        , lnx2 = log x ** 2
        }
        where
            x = getAttributes dp
            y = getLabel dp

-------------------------------------------------------------------------------
-- classification

instance (Floating ring) => Classifier (Logarithmic ring) where
    classify m x = a + b * log x
        where
            b = ( n m * ylnx m - y1 m * lnx m ) / (n m * lnx2 m - lnx m * lnx m )
            a = (y1 m - b * lnx m) / n m

-------------------------------------------------------------------------------
-- examples

-- this example follows a perfect Exponential, so result1 == 1.0
dataset1 = [Coord x (1 + 2 * log x) | x<-[1..100]]
model1 = train dataset1 :: Logarithmic Double
result1 = rsquared model1 dataset1

-- mostly Exponential, but not exact; done in parallel just for fun
dataset2 =  [Coord x (1 + 2 * log (x + 0.1*(-1)**x) ) | x<-[1..100]]
model2 = parallel train dataset2 :: Logarithmic Double
result2 = rsquared model2 dataset2


