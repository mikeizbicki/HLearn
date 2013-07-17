-- | Exponential regression weights all data points equally.  The "ModifiedExponential" type implements a regression  that weights larger data points more heavily.  In some cases, this can result in a better fit.  See <http://mathworld.wolfram.com/LeastSquaresFittingExponential.html> for more details.
module HLearn.Models.Regression.Exponential
    where

import Control.DeepSeq

import HLearn.Algebra
import HLearn.Models.Classifiers.Common
import HLearn.Evaluation.RSquared

-------------------------------------------------------------------------------
-- data types

-- collect the term of sum in least square formula
data Exponential ring = Exponential
    { n      :: !ring -- quantity of sample
    , lny    :: !ring
    , x2    :: !ring
    , x1    :: !ring
    , xlny  :: !ring
    }
    deriving (Read,Show,Eq,Ord)

-- exhaust data
instance (NFData ring) => NFData (Exponential ring) where
    rnf pl = deepseq (n pl)
           $ deepseq (lny pl)
           $ deepseq (x2 pl)
           $ deepseq (x1 pl)
           $ rnf (xlny pl)

-------------------------------------------------------------------------------
-- algebra

instance (Num ring) => Monoid (Exponential ring) where
    mempty = Exponential 0 0 0 0 0
    a `mappend` b = Exponential
        { n   = n a + n b
        , lny = lny a + lny b
        , x2 = x2 a + x2 b
        , x1   = x1 a + x1 b
        , xlny = xlny a + xlny b
        }

instance (Num ring) => Abelian (Exponential ring)

instance (Num ring) => Group (Exponential ring) where
    inverse a = Exponential
        { n   = -(n a)
        , lny = -(lny a)
        , x2 = -(x2 a)
        , x1  = -(x1 a)
        , xlny = -(xlny a)
        }

instance (Num ring) => HasRing (Exponential ring) where
    type Ring (Exponential ring) = ring

instance (Num ring) => Module (Exponential ring) where
    r .* pl = Exponential
        { n   = r * (n pl)
        , lny = r * (lny pl)
        , x2 = r * (x2 pl)
        , x1  = r * (x1 pl)
        , xlny = r * (xlny pl)
        }

-------------------------------------------------------------------------------
-- training

data Coord ring = Coord {x::ring,y::ring}

instance Labeled (Coord ring) where
    type Label (Coord ring)= ring
    type Attributes (Coord ring)= ring
    getLabel = y
    getAttributes = x

instance (Num ring) => NumDP (Exponential ring) where
    numdp = n

instance (Floating ring) => HomTrainer (Exponential ring) where
    type Datapoint (Exponential ring) = Coord ring -- (ring,ring)
    train1dp dp = Exponential
        { n   = 1
        , lny = log y
        , x2 = x ** 2
        , x1  = x
        , xlny = x * log y
        }
        where
            x = getAttributes dp
            y = getLabel dp

-------------------------------------------------------------------------------
-- classification

instance (Floating ring) => Classifier (Exponential ring) where
    classify m x = a * exp (b * x)
        where
            c = n m * x2 m - x1 m ** 2
            b = ( n m * xlny m - x1 m * lny m ) / c
            a = exp ( ( lny m * x2 m - x1 m * xlny m ) / c )

-------------------------------------------------------------------------------
-- examples

-- this example follows a perfect Exponential, so result1 == 1.0
dataset1 = [Coord x (exp (3*x)) | x<-[1..100]]
model1 = train dataset1 :: Exponential Double
result1 = rsquared model1 dataset1

-- mostly Exponential, but not exact; result2 == 0.943; done in parallel just for fun
dataset2 = [Coord 1 2.7, Coord 1.5 4.47, Coord 5.5 244.7, Coord 2 7.38, Coord 4 54.6]
model2 = parallel train dataset2 :: Exponential Double
result2 = rsquared model2 dataset2


