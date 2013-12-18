module HLearn.DataStructures.SpaceTree.Algorithms.KernelDensityEstimation
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
import HLearn.Models.Distributions.Kernels

-------------------------------------------------------------------------------
-- Density

newtype Density kernel dp = Density { getdensity :: (Ring dp) }
    deriving (Read,Show,Eq,Ord)

deriving instance NFData (Ring dp) => NFData (Density kernel dp)

---------------------------------------

instance HasRing dp => Abelian (Density kernel dp)
instance HasRing dp => Monoid (Density kernel dp) where
    {-# INLINE mempty #-}
    {-# INLINE mappend #-}
    mempty = Density 0
    mappend (Density a) (Density b) = Density $ a+b

instance HasRing dp => Group (Density kernel dp) where
    {-# INLINE inverse #-}
    inverse (Density a) = Density (-a)

instance HasRing dp => HasRing (Density kernel dp) where
    type Ring (Density kernel dp) = Ring dp

instance HasRing dp => Module (Density kernel dp) where
    r .* (Density a) = Density $ r * a

---------------------------------------

findDensity :: 
    ( SpaceTree t dp
    , Function kernel (Ring (t dp)) (Ring (t dp))
    ) => 
    t dp -> Ring dp -> dp -> Density kernel dp 
findDensity st epsilon query = prunefoldC (kde_catadp bound query) (kde_cata bound query) mempty st
    where
        bound = epsilon/numdp st

kde_catadp :: forall dp kernel.
    ( MetricSpace dp
    , Function kernel (Ring dp) (Ring dp)
    ) => Ring dp -> dp -> dp -> Density kernel dp -> Density kernel dp
kde_catadp bound query dp kde = kde <> density'
    where
        f = function (undefined::kernel)
        density' = Density $ f $ distance dp query

kde_cata :: forall t dp kernel.
    ( SpaceTree t dp
    , Function kernel (Ring dp) (Ring dp)
    ) => Ring dp -> dp -> t dp -> Density kernel dp -> Strict.Either (Density kernel dp) (Density kernel dp)
kde_cata bound query st kde = if kmin-kmax>bound
    then Strict.Left $ kde <> numdp st .* density'
    else Strict.Right $ kde <> stWeight st .* density'
    where
        f = function (undefined::kernel)
        density' = Density $ f $ distance (stNode st) query

        kmin = f $ stMinDistanceDp st query
        kmax = f $ stMaxDistanceDp st query 

-------------------------------------------------------------------------------
-- DensityMap

newtype DensityMap kernel dp = DensityMap { dm2map :: Map.Map dp (Density kernel dp) } 

deriving instance (NFData dp, NFData (Ring dp)) => NFData (DensityMap kernel dp)

instance (Ord dp, HasRing dp, Function kernel (Ring dp) (Ring dp)) => Monoid (DensityMap kernel dp) where
    mempty = DensityMap mempty
    mappend !(DensityMap rm1) !(DensityMap rm2) = DensityMap $ Map.unionWith (<>) rm1 rm2

---------------------------------------

findDensityMap :: 
    ( SpaceTree t dp
    , Function kernel (Ring dp) (Ring dp)
    , Ord dp
    ) => Ring dp -> DualTree (t dp) -> DensityMap kernel dp
findDensityMap epsilon dual = reduce $ 
    map (\dp -> DensityMap $ Map.singleton dp $ findDensity (reference dual) epsilon dp) (stToList $ query dual)

