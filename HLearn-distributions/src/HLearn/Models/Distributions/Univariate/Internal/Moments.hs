{-# LANGUAGE TemplateHaskell #-}
   
-- | The method of moments can be used to estimate a number of commonly used distributions.  This module is still under construction as I work out the best way to handle morphisms from the Moments3 type to types of other distributions.  For more information, see the wikipedia entry: <https://en.wikipedia.org/wiki/Method_of_moments_(statistics)>

module HLearn.Models.Distributions.Univariate.Internal.Moments
    ( Moments3(..)
    )
    where

import Control.DeepSeq
import GHC.TypeLits
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Deriving

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Visualization.Gnuplot

-------------------------------------------------------------------------------
-- Moments3

data Moments3 prob = Moments3 
    { m0 :: !prob
    , m1 :: !prob
    , m2 :: !prob
    }
    deriving (Read,Show,Eq,Ord)

instance (NFData prob) => NFData (Moments3 prob) where
    rnf m = seq m ()

derivingUnbox "Moments3"
    [t| (U.Unbox a) => (Moments3 a) -> (a, a, a) |]
    [| \ (Moments3 m0 m1 m2) -> (m0,m1,m2) |]
    [| \ (m0,m1,m2) -> (Moments3 m0 m1 m2) |]

-------------------------------------------------------------------------------
-- Algebra

instance (Num prob) => Abelian (Moments3 prob)
instance (Num prob) => Monoid (Moments3 prob) where
    mempty = Moments3 0 0 0 
    ma `mappend` mb = Moments3 
        { m0 = m0 ma + m0 mb
        , m1 = m1 ma + m1 mb
        , m2 = m2 ma + m2 mb
        }
    
instance (Num prob) => Group (Moments3 prob ) where
    inverse !m = Moments3 (negate $ m0 m) (negate $ m1 m) (negate $ m2 m)

instance (Num prob) => HasRing (Moments3 prob) where
    type Ring (Moments3 prob) = prob
    
instance (Num prob) => Module (Moments3 prob) where
    r .* dist = Moments3
        { m0 = r * m0 dist
        , m1 = r * m1 dist
        , m2 = r * m2 dist
        }
        

-- instance (Fractional prob, VU.Unbox prob, SingI n) => LeftModule prob (Moments3 prob n)
-- instance (Fractional prob, VU.Unbox prob) => LeftOperator prob (Moments3 prob n) where
--     (.*) !p !(Moments3 vec) = Moments3 $ VU.map (*p) vec
-- 
-- instance (Fractional prob, VU.Unbox prob, SingI n) => RightModule prob (Moments3 prob n)
-- instance (Fractional prob, VU.Unbox prob) => RightOperator prob (Moments3 prob n) where
--     (*.) = flip (.*)
--     
-------------------------------------------------------------------------------
-- Training
    
instance (Num prob) => HomTrainer (Moments3 prob) where
    type Datapoint (Moments3 prob) = prob
    train1dp dp = Moments3 1 dp (dp*dp)
    
instance (Num prob) => NumDP (Moments3 prob) where
    numdp = m0

-------------------------------------------------------------------------------
-- LogNormal

-- newtype LogNormal prob = LogNormal (Moments3 prob)
--     deriving (Read,Show,Eq,Ord,Semigroup,Monoid,RegularSemigroup)
--     
-- instance ModelParams (LogNormal prob) where
--     type Params (LogNormal prob) = NoParams
--     getparams _ = NoParams
-- 
-- instance (Num prob) => HomTrainer (LogNormal prob) where
--     type Datapoint (LogNormal prob) = prob
--     train1dp' params dp = LogNormal $ train1dp' params dp
-- 
-- instance (Num prob) => Distribution (LogNormal prob) where
--     type Probability (LogNormal prob) = prob
-- 
-- instance (Eq prob, Floating prob) => PDF (LogNormal prob) where
--     pdf (LogNormal dist) 0  = 0
--     pdf (LogNormal dist) dp = (1/(s*dp*(sqrt $ 2*pi)))*(exp $ (-1)*((log dp)-m)**2/(2*s*s))
--         where
--             m = 0
--             s = 1

-------------------------------------------------------------------------------
-- Moments4

{--
data Moments4 prob = Moments4 
    { m0 :: !prob
    , m1 :: !prob
    , m2 :: !prob
    , m3 :: !prob
    }
    deriving (Read,Show,Eq,Ord)

instance (NFData prob) => NFData (Moments4 prob) where
    rnf m = seq m ()

derivingUnbox "Moments4"
    [t| (U.Unbox a) => (Moments4 a) -> (a, a, a ,a) |]
    [| \ (Moments4 m0 m1 m2 m3) -> (m0,m1,m2,m3) |]
    [| \ (m0,m1,m2,m3) -> (Moments4 m0 m1 m2,m3) |]

--}


