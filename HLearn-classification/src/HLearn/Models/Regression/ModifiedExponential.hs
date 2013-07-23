-- | Exponential regression weights all data points equally, whereas this modified version places more emphasis on the larger values.  In some cases, this can result in a better fit.  See <http://mathworld.wolfram.com/LeastSquaresFittingExponential.html> for more details.
module HLearn.Models.Regression.ModifiedExponential
    where

import Control.DeepSeq

import HLearn.Algebra
import HLearn.Models.Classifiers.Common
import HLearn.Evaluation.RSquared
import HLearn.Models.Regression.Common

-- ModifiedExponential is similar from Exponential
-- Exponential ==> ModifiedExponential
-- n ==> y1
-- lny ==> ylny
-- x2 ==> yx2
-- x1 ==> yx1
-- xlny ==> xylny
-------------------------------------------------------------------------------
-- data types

-- collect the term of sum in least square formula
data ModifiedExponential ring = ModifiedExponential
    { y1     :: !ring -- quantity of sample
    , ylny    :: !ring
    , yx2    :: !ring
    , yx1    :: !ring
    , xylny  :: !ring
    }
    deriving (Read,Show,Eq,Ord)

-- exhaust data
instance (NFData ring) => NFData (ModifiedExponential ring) where
    rnf pl = deepseq (y1pl)
           $ deepseq (ylny pl)
           $ deepseq (yx2 pl)
           $ deepseq (yx1 pl)
           $ rnf (xylny pl)

-------------------------------------------------------------------------------
-- algebra

instance (Num ring) => Monoid (ModifiedExponential ring) where
    mempty = ModifiedExponential 0 0 0 0 0
    a `mappend` b = ModifiedExponential
        { y1  = y1a + y1b
        , ylny = ylny a + ylny b
        , yx2 = yx2 a + yx2 b
        , yx1   = yx1 a + yx1 b
        , xylny = xylny a + xylny b
        }

instance (Num ring) => Abelian (ModifiedExponential ring)

instance (Num ring) => Group (ModifiedExponential ring) where
    inverse a = ModifiedExponential
        { y1  = -(y1a)
        , ylny = -(ylny a)
        , yx2 = -(yx2 a)
        , yx1  = -(yx1 a)
        , xylny = -(xylny a)
        }

instance (Num ring) => HasRing (ModifiedExponential ring) where
    type Ring (ModifiedExponential ring) = ring

instance (Num ring) => Module (ModifiedExponential ring) where
    r .* pl = ModifiedExponential
        { y1  = r * (y1pl)
        , ylny = r * (ylny pl)
        , yx2 = r * (yx2 pl)
        , yx1  = r * (yx1 pl)
        , xylny = r * (xylny pl)
        }

-------------------------------------------------------------------------------
-- training

instance (Num ring) => NumDP (ModifiedExponential ring) where
    numdp = n

instance (Floating ring) => HomTrainer (ModifiedExponential ring) where
    type Datapoint (ModifiedExponential ring) = Coord ring -- (ring,ring)
    train1dp dp = ModifiedExponential
        { y1  = y
        , ylny = y * log y
        , yx2 = y * x ** 2
        , yx1  = y * x
        , xylny = x * y * log y
        }
        where
            x = getAttributes dp
            y = getLabel dp

-------------------------------------------------------------------------------
-- classification

instance (Floating ring) => Classifier (ModifiedExponential ring) where
    classify m x = a * exp (b * x)
        where
            c = y1 m * yx2 m - yx1 m ** 2
            b = ( y1 m * xylny m - yx1 m * ylny m ) / c
            a = exp ( ( ylny m * yx2 m - yx1 m * xylny m ) / c )

-------------------------------------------------------------------------------
-- examples

-- this example follows a perfect Exponential
dataset1 = [Coord x (exp (3*x)) | x<-[1..100]]
model1 = train dataset1 :: Exponential Double
result1 = rsquared model1 dataset1

-- mostly Exponential, but not exact;
dataset2 = [Coord 1 2.7, Coord 1.5 4.47, Coord 5.5 244.7, Coord 2 7.38, Coord 4 54.6]
model2 = parallel train dataset2 :: Exponential Double
result2 = rsquared model2 dataset2
