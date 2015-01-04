module HLearn.DataStructures.SpaceTree.Algorithms.Correlation
    where

import Debug.Trace

import Control.DeepSeq
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Strict.Either as Strict
import qualified Data.Strict.Maybe as Strict
import qualified Data.Strict.Tuple as Strict
import GHC.TypeLits

import HLearn.Algebra
import HLearn.DataStructures.SpaceTree

-------------------------------------------------------------------------------
-- Correlation

data Correlation dp = Correlation 
    { unCorrelation :: !(Ring dp)
    , range :: !(Ring dp)
    }
    deriving (Read,Show,Eq,Ord)

mkCorrelation :: HasRing dp => Ring dp -> Correlation dp
mkCorrelation r = Correlation 0 r

instance NFData (Correlation dp) where
    rnf c = seq c ()

---------------------------------------

instance (Eq (Ring dp), HasRing dp) => Abelian (Correlation dp)
instance (Eq (Ring dp), HasRing dp) => Monoid (Correlation dp) where
    mempty = Correlation 0 0
    mappend (Correlation c1 r1) (Correlation c2 r2) = if r1 /= r2
        then error "Correlation.Monoid./="
        else Correlation (r1+r2) r1

instance (Eq (Ring dp), HasRing dp) => Group (Correlation dp) where
    inverse (Correlation c r) = Correlation (-c) r

instance (Eq (Ring dp), HasRing dp) => HasRing (Correlation dp) where
    type Ring (Correlation dp) = Ring dp

instance (Eq (Ring dp), HasRing dp) => Module (Correlation dp) where
    a .* (Correlation c r) = Correlation (c*a) r

---------------------------------------

findCorrelationSingle :: SpaceTree t dp => t dp -> Ring dp -> dp -> Correlation dp 
findCorrelationSingle st range dp = prunefoldC (cor_catadp dp) (cor_cata dp) mempty st

cor_catadp :: MetricSpace dp => dp -> dp -> Correlation dp -> Correlation dp
cor_catadp query dp cor = if distance query dp < range cor
    then cor { unCorrelation = unCorrelation cor+1 }
    else cor

cor_cata :: SpaceTree t dp => dp -> t dp -> Correlation dp -> Strict.Either (Correlation dp) (Correlation dp) 
cor_cata query st cor = case stIsMinDistanceDpFartherThanWithDistance st query (range cor) of
    Strict.Nothing -> Strict.Left cor
    Strict.Just dist -> if stMaxDistanceDp st query < range cor
        then Strict.Left $ cor { unCorrelation = unCorrelation cor + numdp st }
        else Strict.Right $ if dist < range cor
            then cor { unCorrelation = unCorrelation cor + stWeight st }
            else cor
        
findCorrelationDual :: (Eq dp, SpaceTree t dp) => DualTree (t dp) -> Ring dp -> Correlation dp
findCorrelationDual dual range = reduce $
    map (\dp -> findCorrelationSingle (reference dual) range dp) (stToList $ query dual)
