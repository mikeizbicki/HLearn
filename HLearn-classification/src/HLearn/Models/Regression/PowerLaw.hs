module HLearn.Models.Regression.PowerLaw
    where

import Control.DeepSeq

import HLearn.Algebra
import HLearn.Models.Classifiers.Common
import HLearn.Evaluation.RSquared

-------------------------------------------------------------------------------
-- data types

data PowerLaw ring = PowerLaw
    { n      :: !ring
    , lnxlny :: !ring
    , lnx    :: !ring
    , lnx2   :: !ring
    , lny    :: !ring
    }
    deriving (Read,Show,Eq,Ord)
    
instance (NFData ring) => NFData (PowerLaw ring) where
    rnf pl = deepseq (n pl)
           $ deepseq (lnxlny pl)
           $ deepseq (lnx pl)
           $ deepseq (lnx2 pl)
           $ rnf (lny pl)
    
-------------------------------------------------------------------------------
-- algebra

instance (Num ring) => Monoid (PowerLaw ring) where
    mempty = PowerLaw 0 0 0 0 0
    a `mappend` b = PowerLaw
        { n      = n a + n b
        , lnxlny = lnxlny a + lnxlny b
        , lnx    = lnx a + lnx b
        , lnx2   = lnx2 a + lnx2 b
        , lny    = lny a + lny b
        }

instance (Num ring) => Abelian (PowerLaw ring)

instance (Num ring) => Group (PowerLaw ring) where
    inverse a = PowerLaw
        { n      = -(n a)
        , lnxlny = -(lnxlny a)
        , lnx    = -(lnx a)
        , lnx2   = -(lnx2 a)
        , lny    = -(lny a)
        }
        
instance (Num ring) => HasRing (PowerLaw ring) where
    type Ring (PowerLaw ring) = ring

instance (Num ring) => Module (PowerLaw ring) where
    r .* pl = PowerLaw
        { n      = r*(n pl)
        , lnxlny = r*(lnxlny pl)
        , lnx    = r*(lnx pl)
        , lnx2   = r*(lnx2 pl)
        , lny    = r*(lny pl)
        }

-------------------------------------------------------------------------------
-- training
    
data Coord ring = Coord {x::ring,y::ring}
    
instance Labeled (Coord ring) where
    type Label (Coord ring)= ring
    type Attributes (Coord ring)= ring
    getLabel = y
    getAttributes = x

instance (Num ring) => NumDP (PowerLaw ring) where
    numdp = n

instance (Floating ring) => HomTrainer (PowerLaw ring) where
    type Datapoint (PowerLaw ring) = Coord ring -- (ring,ring)
    train1dp dp = PowerLaw
        { n      = 1
        , lnxlny = log x *  log y
        , lnx    = log x
        , lnx2   = (log x)^2
        , lny    = log y
        }
        where
            x = getAttributes dp
            y = getLabel dp
        
-------------------------------------------------------------------------------
-- classification

instance (Floating ring) => Classifier (PowerLaw ring) where
    classify m x = (exp a)*(x**b)
        where
            b = ((n m)*(lnxlny m)-(lnx m)*(lny m))/((n m)*(lnx2 m)-(lnx m)^2)
            a = ((lny m)-b*(lnx m))/(n m)
            
-------------------------------------------------------------------------------
-- examples
    
-- this example follows a perfect power law, so result1 == 1.0
dataset1 = [Coord x (x^3) | x<-[1..100]] 
model1 = train dataset1 :: PowerLaw Double
result1 = rsquared model1 dataset1

-- mostly powerlaw, but not exact; result2 == 0.943; done in parallel just for fun
dataset2 = [Coord 1 2, Coord 1.5 3, Coord 5.5 15, Coord 2 3, Coord 4 13]
model2 = parallel train dataset2 :: PowerLaw Double
result2 = rsquared model2 dataset2