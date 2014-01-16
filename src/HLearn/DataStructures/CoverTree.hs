{-# LANGUAGE NoMonomorphismRestriction,DataKinds,PolyKinds #-}

module HLearn.DataStructures.CoverTree
    ({- CoverTree
    , -}CoverTree'

    , insertBatch
    , insertBatchVU

    -- * unsafe
--     , ctmap
--     , unsafeMap
--     , recover
--     , trainct_insert
    , setNodeV
    , rmGhostSingletons
    , packCT
    , packCT2
    , packCT3

--     , sepdistL

    -- * drawing
    , draw
    , draw'
    , IntShow (..)

    -- * QuickCheck properties
    , property_separating
    , property_covering
    , property_leveled
    , property_maxDescendentDistance
    )
    where

import GHC.TypeLits
import Control.Monad
import Control.Monad.Random hiding (fromList)
import Control.Monad.ST
import Control.DeepSeq
import Data.List hiding (insert)
import Data.Maybe
import Data.Monoid hiding ((<>))
import qualified Data.Foldable as F
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import qualified Data.Strict.Tuple as Strict
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import Test.QuickCheck
import Debug.Trace

import Diagrams.Prelude hiding (distance,trace,query,connect)
import Diagrams.Backend.SVG.CmdLine

import qualified Control.ConstraintKinds as CK
import HLearn.Algebra hiding ((#),(<>),(|>),numdp)
import qualified HLearn.Algebra
import HLearn.DataStructures.SpaceTree
import HLearn.DataStructures.SpaceTree.DualTreeMonoids
import HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor hiding (weight)
import HLearn.DataStructures.SpaceTree.Algorithms.RangeSearch
import qualified HLearn.DataStructures.StrictList as Strict
import HLearn.DataStructures.StrictList (List (..))

import HLearn.Models.Classifiers.Common
import HLearn.Metrics.Lebesgue

-------------------------------------------------------------------------------
-- data types

type CoverTree dp = AddUnit (CoverTree' (2/1) V.Vector V.Vector) () dp

data CoverTree' (base::Frac) childContainer nodeVvec tag dp = Node 
    { nodedp                :: !dp
    , level                 :: !Int
    , weight                :: !(Ring dp)
    , numdp                 :: !(Ring dp)
    , maxDescendentDistance :: !(Ring dp)
    , children              :: !(childContainer (CoverTree' base childContainer nodeVvec tag dp))
    , nodeV                 :: !(nodeVvec dp)
    , tag                   :: !tag
    }

data instance VU.Vector (CoverTree' base childContainer nodeVvec tag dp) = TreeVector
    { uvec         :: !(VU.Vector (dp,Int,Ring dp, Ring dp, Ring dp))
    , vec_children :: !(V.Vector (childContainer (CoverTree' base childContainer nodeVvec tag dp)))
    , vec_nodeV    :: !(V.Vector (nodeVvec dp))
    , vec_tag      :: !(V.Vector tag)
    }

-- instance (VUM.Unbox (Ring dp), VUM.Unbox dp) => VUM.Unbox (CoverTree' base childContainer nodeVvec tag dp)
instance (VUM.Unbox (Ring dp), VUM.Unbox dp) => VUM.Unbox (CoverTree' base VU.Vector VU.Vector () dp)
-- asd = insertBatch [(1,2),(3,4)] :: CoverTree' (2/1) VU.Vector VU.Vector () (Double,Double)

instance 
    ( VUM.Unbox (Ring dp)
    , VUM.Unbox dp
--     ) => VG.Vector VU.Vector (CoverTree' base childContainer nodeVvec tag dp) 
    ) => VG.Vector VU.Vector (CoverTree' base VU.Vector VU.Vector () dp) 
        where
    
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze v = do
        uvec' <- VG.unsafeFreeze $ uvecM v
        vec_children' <- VG.unsafeFreeze $ vecM_children v
        vec_nodeV' <- VG.unsafeFreeze $ vecM_nodeV v
        vec_tag' <- VG.unsafeFreeze $ vecM_tag v
        return $ TreeVector uvec' vec_children' vec_nodeV' vec_tag'

    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw v = do
        uvecM' <- VG.unsafeThaw $ uvec v
        vecM_children' <- VG.unsafeThaw $ vec_children v
        vecM_nodeV' <- VG.unsafeThaw $ vec_nodeV v
        vecM_tag' <- VG.unsafeThaw $ vec_tag v
        return $ TreeMVector uvecM' vecM_children' vecM_nodeV' vecM_tag'
    
    {-# INLINE basicLength #-}
    basicLength v = VG.basicLength (uvec v)

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i len v = TreeVector
        { uvec = VG.unsafeSlice i len $ uvec v
        , vec_children = VG.unsafeSlice i len $ vec_children v
        , vec_nodeV = VG.unsafeSlice i len $ vec_nodeV v
        , vec_tag = VG.unsafeSlice i len $ vec_tag v
        }

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM v i = do
        (nodedp',level',weight',numdp',maxDescendentDistance') <- VG.basicUnsafeIndexM (uvec v) i
        children' <- VG.unsafeIndexM (vec_children v) i
        nodeV' <- VG.unsafeIndexM (vec_nodeV v) i
        tag' <- VG.unsafeIndexM (vec_tag v) i
        return $ Node nodedp' level' weight' numdp' maxDescendentDistance' children' nodeV' tag'

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy mv v = do
        VG.basicUnsafeCopy (uvecM mv) (uvec v)
        VG.basicUnsafeCopy (vecM_children mv) (vec_children v)
        VG.basicUnsafeCopy (vecM_nodeV mv) (vec_nodeV v)
        VG.basicUnsafeCopy (vecM_tag mv) (vec_tag v)
        
    {-# INLINE elemseq #-}
    elemseq v = seq

data instance VUM.MVector s (CoverTree' base childContainer nodeVvec tag dp) = TreeMVector
    { uvecM         :: !(VUM.MVector s (dp,Int,Ring dp, Ring dp, Ring dp))
    , vecM_children :: !(VM.MVector s (childContainer (CoverTree' base childContainer nodeVvec tag dp)))
    , vecM_nodeV    :: !(VM.MVector s (nodeVvec dp))
    , vecM_tag      :: !(VM.MVector s tag)
    }

instance
    ( VUM.Unbox (Ring dp)
    , VUM.Unbox dp
    ) => VGM.MVector VUM.MVector (CoverTree' base VU.Vector VU.Vector () dp) 
        where
    {-# INLINE basicLength #-} 
    basicLength v = VGM.basicLength (uvecM v)

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i len v = TreeMVector
        { uvecM = VGM.unsafeSlice i len $ uvecM v
        , vecM_children = VGM.unsafeSlice i len $ vecM_children v
        , vecM_nodeV = VGM.unsafeSlice i len $ vecM_nodeV v
        , vecM_tag = VGM.unsafeSlice i len $ vecM_tag v
        }

    {-# INLINE basicOverlaps #-}
    basicOverlaps v1 v2 
        =  VGM.basicOverlaps (uvecM v1) (uvecM v2)
        || VGM.basicOverlaps (vecM_children v1) (vecM_children v2)
        || VGM.basicOverlaps (vecM_nodeV v1) (vecM_nodeV v2)
        || VGM.basicOverlaps (vecM_tag v1) (vecM_tag v2)
    
    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew i = do
        uvecM' <- VGM.unsafeNew i
        vecM_children' <- VGM.unsafeNew i
        vecM_nodeV' <- VGM.unsafeNew i
        vecM_tag' <- VGM.unsafeNew i
        return $ TreeMVector uvecM' vecM_children' vecM_nodeV' vecM_tag'

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead v i = do
        (nodedp',level',weight',numdp',maxDescendentDistance') <- VGM.basicUnsafeRead (uvecM v) i
        children' <- VGM.unsafeRead (vecM_children v) i
        nodeV' <- VGM.unsafeRead (vecM_nodeV v) i
        tag' <- VGM.unsafeRead (vecM_tag v) i
        return $ Node nodedp' level' weight' numdp' maxDescendentDistance' children' nodeV' tag'

    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite v i ct = do
        VGM.unsafeWrite (uvecM v) i (nodedp ct, level ct, weight ct, numdp ct, maxDescendentDistance ct)
        VGM.unsafeWrite (vecM_children v) i (children ct)
        VGM.unsafeWrite (vecM_nodeV v) i (nodeV ct)
        VGM.unsafeWrite (vecM_tag v) i (tag ct)

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy v1 v2 = do
        VGM.basicUnsafeCopy (uvecM v1) (uvecM v2)
        VGM.basicUnsafeCopy (vecM_children v1) (vecM_children v2)
        VGM.basicUnsafeCopy (vecM_nodeV v1) (vecM_nodeV v2)
        VGM.basicUnsafeCopy (vecM_tag v1) (vecM_tag v2)

    {-# INLINE basicUnsafeMove #-}
    basicUnsafeMove v1 v2 = do
        VGM.basicUnsafeMove (uvecM v1) (uvecM v2)
        VGM.basicUnsafeMove (vecM_children v1) (vecM_children v2)
        VGM.basicUnsafeMove (vecM_nodeV v1) (vecM_nodeV v2)
        VGM.basicUnsafeMove (vecM_tag v1) (vecM_tag v2)

    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeGrow v i = do
        uvecM' <- VGM.basicUnsafeGrow (uvecM v) i
        vecM_children' <- VGM.basicUnsafeGrow (vecM_children v) i
        vecM_nodeV' <- VGM.basicUnsafeGrow (vecM_nodeV v) i
        vecM_tag' <- VGM.basicUnsafeGrow (vecM_tag v) i
        return $ TreeMVector uvecM' vecM_children' vecM_nodeV' vecM_tag'

deriving instance (Read (Ring dp), Read (childContainer (CoverTree' base childContainer nodeVvec tag dp)), Read (nodeVvec dp), Read tag, Read dp) => Read (CoverTree' base childContainer nodeVvec tag dp)
deriving instance (Show (Ring dp), Show (childContainer (CoverTree' base childContainer nodeVvec tag dp)),Show (nodeVvec dp), Show tag, Show dp) => Show (CoverTree' base childContainer nodeVvec tag dp)

-- instance (Eq dp, Eq (Ring dp), Floating (Ring dp), SingI base) => Eq (CoverTree' base childContainer nodeVvec tag dp) where
--     ct1 == ct2 = sepdist ct1 == sepdist ct2 && nodedp ct1 == nodedp ct2
-- 
-- instance (Ord dp, Ord (Ring dp), SingI base) => Ord (CoverTree' base childContainer nodeVvec tag dp) where
--     compare ct1 ct2 = compare (sepdist ct1) (sepdist ct2)
--                    <> compare (nodedp ct1) (nodedp ct2)

instance (NFData dp,NFData (Ring dp),NFData tag) => NFData (CoverTree' base childContainer nodeVvec tag dp) where
    rnf ct = deepseq (numdp ct) 
           $ deepseq (maxDescendentDistance ct)
           $ seq ct
           $ ()

instance 
--     ( HasRing dp
--     , MetricSpace dp
--     , VG.Vector nodeVvec dp
--     , Monoid (nodeVvec dp)
--     , VG.Vector childContainer dp
--     , VG.Vector childContainer (CoverTree' base childContainer nodeVvec tag dp)
--     , SingI base
    ( ValidCT base childContainer nodeVvec tag dp
    ) => SpaceTree (CoverTree' base childContainer nodeVvec tag) dp
        where

    type LeafVector (CoverTree' base childContainer nodeVvec tag) = nodeVvec 
    type ChildContainer (CoverTree' base childContainer nodeVvec tag) = childContainer

    {-# INLINABLE stMinDistance #-}
    {-# INLINABLE stMaxDistance #-}
    {-# INLINABLE stMinDistanceDpWithDistance #-}
    {-# INLINABLE stMaxDistanceDpWithDistance #-}
    {-# INLINABLE stMinDistanceDpFromDistance #-}
    {-# INLINABLE stMaxDistanceDpFromDistance #-}
    {-# INLINABLE stIsMinDistanceDpFartherThanWithDistance #-}
    {-# INLINABLE stIsMaxDistanceDpFartherThanWithDistance #-}
    {-# INLINABLE stChildren #-}
    {-# INLINABLE stChildren' #-}
    {-# INLINABLE stChildren_ #-}
    {-# INLINABLE stNode #-}
    {-# INLINABLE stNodeV #-}
    {-# INLINABLE stHasNode #-}
    {-# INLINABLE stIsLeaf #-}

    stMinDistanceWithDistance !ct1 !ct2 = 
        dist-(maxDescendentDistance ct1)-(maxDescendentDistance ct2) Strict.:!: dist
        where dist = distance (nodedp ct1) (nodedp ct2) 
    stMaxDistanceWithDistance !ct1 !ct2 = 
        dist+(maxDescendentDistance ct1)+(maxDescendentDistance ct2) Strict.:!: dist
        where dist = distance (nodedp ct1) (nodedp ct2) 

    stMinDistanceDpWithDistance !ct !dp = dist - maxDescendentDistance ct Strict.:!: dist
        where dist = distance (nodedp ct) dp
    stMaxDistanceDpWithDistance !ct !dp = dist + maxDescendentDistance ct Strict.:!: dist
        where dist = distance (nodedp ct) dp

    stIsMinDistanceDpFartherThanWithDistance !ct !dp !b = 
        case isFartherThanWithDistance (nodedp ct) dp (b+maxDescendentDistance ct) of
            Strict.Nothing -> Strict.Nothing
            Strict.Just dist -> Strict.Just $ dist

    stIsMaxDistanceDpFartherThanWithDistance !ct !dp !b = 
        isFartherThanWithDistance (nodedp ct) dp (b-maxDescendentDistance ct)

    {-# INLINABLE stIsMinDistanceDpFartherThanWithDistanceCanError #-}
    stIsMinDistanceDpFartherThanWithDistanceCanError !ct !dp !b = 
        isFartherThanWithDistanceCanError (nodedp ct) dp (b+maxDescendentDistance ct)

    {-# INLINABLE stIsMaxDistanceDpFartherThanWithDistanceCanError #-}
    stIsMaxDistanceDpFartherThanWithDistanceCanError !ct !dp !b = 
        isFartherThanWithDistanceCanError (nodedp ct) dp (b-maxDescendentDistance ct)

    stMinDistanceDpFromDistance !ct !dp !dist = dist-maxDescendentDistance ct
    stMaxDistanceDpFromDistance !ct !dp !dist = dist+maxDescendentDistance ct

    stChildren  = toList . children
    stChildren' = Strict.list2strictlist . toList . children
    stChildren_ = children
    stNodeV     = nodeV
    stNode      = nodedp
    stWeight    = weight
    stHasNode _ = True
    stIsLeaf ct = null $ toList $ children ct

    ro _ = 0
    lambda !ct = maxDescendentDistance ct 

rmGhostSingletons :: 
    ( Eq (Ring dp)
    , Eq dp
    , Num (Ring dp)
--     , Container childContainer
    , VG.Vector childContainer (CoverTree' base childContainer nodeVvec tag dp)
    ) => 
    CoverTree' base childContainer nodeVvec tag dp -> CoverTree' base childContainer nodeVvec tag dp
rmGhostSingletons ct = if length (VG.toList $ children ct) == 1 && weight ct == 0
    then child
        { level = level ct
        , children = VG.map rmGhostSingletons $ children child
        }
    else ct
        { children = VG.map rmGhostSingletons $ children ct
        }
    where
        child = head $ VG.toList $ children ct

-- rmGhostSingletons ct = if V.length (children ct) == 1 {-&& nodedp ct == nodedp child -} && weight child==0
--     then rmGhostSingletons $ ct { children = children $ V.head $ children ct }
--     else ct { children = fmap rmGhostSingletons $ children ct }
--     where
--         child = V.head $ children ct

packCT3 :: 
    ( ValidCT base childContainer nodeVvec tag dp
    , nodeVvec ~ VU.Vector
    , VU.Unbox dp
    ) => CoverTree' base childContainer nodeVvec tag dp -> CoverTree' base childContainer nodeVvec tag dp
packCT3 ct = snd $ go 0 ct
    where
        go i t = (i',t
            { nodedp = v VU.! i
            , children = fromList children'
            })
            where
                (i',children') = mapAccumL go (i+1) (dosort ct $ toList $ children t)

        v = fromList $ mkNodeList ct

        mkNodeList ct = [nodedp ct] 
                     ++ (concatMap mkNodeList $ dosort ct $ toList $ children ct)

        dosort node xs = sortBy (\ct1 ct2 -> sortchildren ct1 ct2 <> sortdistance node ct1 ct2) xs
        sortchildren ct1 ct2 = if length (toList $ children ct1) == 0 || length (toList $ children ct2) == 0
            then if length (toList $ children ct1) == length (toList $ children ct2)
                then EQ
                else if length (toList $ children ct1) == 0
                    then GT
                    else LT
            else EQ
        sortdistance node ct1 ct2 = compare (distance (nodedp node) (nodedp ct2)) (distance (nodedp node) (nodedp ct1))

packCT2 :: 
    ( ValidCT base childContainer nodeVvec tag dp
    , nodeVvec ~ VU.Vector
    , VU.Unbox dp
    ) => Int -> CoverTree' base childContainer nodeVvec tag dp -> CoverTree' base childContainer nodeVvec tag dp
packCT2 n ct = snd $ go 1 $ ct' { nodedp = v VG.! 0 }
    where
        go i t = (i',t
            { nodeV = VU.slice i (VG.length $ nodeV t) v
            , children = fromList children'
            })
            where
                (i',children') = mapAccumL go 
                    (i+length (toList $ nodeV t)+length (toList $ children t)) 
                    (fmap (\(j,x) -> if nodedp x /= v VG.! j then error ("/="++show j) else x { nodedp = v VG.! j } ) 
                        $ zip [i+length (toList $ nodeV t) .. ] $ toList $ children t)

        ct' = setNodeV n ct
        v = fromList $ mkNodeList ct'

        mkNodeList ct = [nodedp ct]++go_mkNodeList ct
        go_mkNodeList ct = (toList $ nodeV ct) 
                        ++ (map nodedp $ toList $ children ct)
                        ++ (concatMap go_mkNodeList $ toList $ children ct)

packCT :: 
    ( ValidCT base childContainer nodeVvec tag dp
    , nodeVvec ~ VU.Vector
    , VU.Unbox dp
    ) => Int -> CoverTree' base childContainer nodeVvec tag dp -> CoverTree' base childContainer nodeVvec tag dp
packCT n ct = snd $ go 0 ct'
    where
        go i t = (i',t
            { nodedp = v VU.! i
            , nodeV = VU.slice (i+1) (VG.length $ nodeV t) v
            , children = fromList children'
            })
            where
                (i',children') = mapAccumL go (i+1+length (toList $ nodeV t)) (toList $ children t)

        ct' = setNodeV n ct
        v = fromList $ mkNodeList ct'

        mkNodeList ct = [nodedp ct] 
                     ++ (toList $ nodeV ct) 
                     ++ (concatMap mkNodeList $ toList $ children ct)

setNodeV :: 
    ( ValidCT base childContainer nodeVvec tag dp
    ) => Int -> CoverTree' base childContainer nodeVvec tag dp -> CoverTree' base childContainer nodeVvec tag dp
setNodeV n ct = if stNumNodes ct > n
    then ct
        { children = fromList $ fmap (setNodeV n) $ filter (not . stIsLeaf) $ toList $ children ct
        , nodeV = fromList $ fmap nodedp $ filter stIsLeaf $ toList $ children ct
        }
    else ct
        { children = mempty
        , nodeV = fromList $ stToList ct
        }

---------------------------------------

---------------------------------------
-- insertion as described in the paper

class 
    ( MetricSpace dp
    , Ord (Ring dp)
    , Floating (Ring dp)
    , Monoid tag
    , Monoid (nodeVvec dp)
    , Monoid (childContainer dp)
    , Monoid (childContainer (CoverTree' base childContainer nodeVvec tag dp))
    , FromList nodeVvec dp
    , FromList childContainer dp
    , FromList childContainer (Ring dp)
    , FromList childContainer (CoverTree' base childContainer nodeVvec tag dp)
--     , VG.Vector nodeVvec dp
--     , VG.Vector childContainer dp
--     , VG.Vector childContainer (CoverTree' base childContainer nodeVvec tag dp)
--     , VG.Vector childContainer (Ring dp)
    , SingI base
    , Show (Ring dp)
    , Show dp
    , Ord dp
    ) => ValidCT base childContainer nodeVvec tag dp

instance 
    ( MetricSpace dp
    , Ord (Ring dp)
    , Floating (Ring dp)
    , Monoid tag
    , Monoid (nodeVvec dp)
    , Monoid (childContainer dp)
    , Monoid (childContainer (CoverTree' base childContainer nodeVvec tag dp))
    , FromList nodeVvec dp
    , FromList childContainer dp
    , FromList childContainer (Ring dp)
    , FromList childContainer (CoverTree' base childContainer nodeVvec tag dp)
--     , VG.Vector nodeVvec dp
--     , VG.Vector childContainer dp
--     , VG.Vector childContainer (Ring dp)
--     , VG.Vector childContainer (CoverTree' base childContainer nodeVvec tag dp)
    , SingI base
    , Show (Ring dp)
    , Show dp
    , Ord dp
    ) => ValidCT base childContainer nodeVvec tag dp

safeInsert :: forall base childContainer nodeVvec tag dp.
    ( ValidCT base childContainer nodeVvec tag dp
    ) => CoverTree' base childContainer nodeVvec tag dp 
      -> Weighted dp 
      -> CoverTree' base childContainer nodeVvec tag dp
safeInsert node (0,_) = node
safeInsert !node !(w,dp) = case insert node (w,dp) of
    Strict.Just x -> x
    Strict.Nothing -> Node
        { nodedp    = dp
        , level     = dist2level_up (sing::Sing base) dist
        , weight    = w
        , numdp     = numdp node+1
        , tag       = mempty
        , children  = fromList [node]
        , nodeV     = mempty
        , maxDescendentDistance = maximum $ map (distance dp) $ stToList node
        }
        where
            dist = distance (nodedp node) dp

--     Strict.Nothing -> Node
--         { nodedp    = nodedp node
--         , level     = dist2level_up (sing::Sing base) dist
--         , weight    = 0 
--         , numdp     = 0 + Strict.sum (fmap numdp children')
--         , tag       = mempty
--         , children  = fromList $ Strict.strictlist2list children'
--         , nodeV     = mempty
--         , maxDescendentDistance = max
--             (maxDescendentDistance node)
--             (distance (nodedp node) dp)
--         }
--         where
--             children' :: Strict.List (CoverTree' base childContainer nodeVvec tag dp)
--             children' = (growct node (dist2level_down (sing::Sing base) dist))
--                       :.(Node 
--                             { nodedp    = dp 
--                             , weight    = w
--                             , numdp     = w
--                             , level     = dist2level_down (sing::Sing base) dist
--                             , tag       = mempty
--                             , children  = mempty
--                             , nodeV     = mempty
--                             , maxDescendentDistance = 0
--                             })
--                       :.Strict.Nil
-- 
--             dist = distance (nodedp node) dp


insert :: forall base childContainer nodeVvec tag dp.
    ( ValidCT base childContainer nodeVvec tag dp
--     ( MetricSpace dp
--     , Ord (Ring dp)
--     , Monoid (nodeVvec dp)
--     , Floating (Ring dp)
--     , Monoid tag
--     , SingI base
--     , Monoid (childContainer (CoverTree' base childContainer nodeVvec tag dp))
-- --     , Container childContainer
--     , VG.Vector childContainer (CoverTree' base childContainer nodeVvec tag dp) 
--     , VG.Vector childContainer dp
    ) => CoverTree' base childContainer nodeVvec tag dp -> Weighted dp -> Strict.Maybe (CoverTree' base childContainer nodeVvec tag dp)
insert !node !(!w,!dp) = if isFartherThan dp (nodedp node) (sepdist node)
    then Strict.Nothing
    else Strict.Just $ Node
        { nodedp    = nodedp node
        , level     = level node
        , weight    = weight node
        , numdp     = weight node + Strict.sum (fmap numdp children')
        , tag       = tag node
        , children  = fromList $ sortBy sortgo $ Strict.strictlist2list children'
        , nodeV     = mempty
        , maxDescendentDistance = max
            (maxDescendentDistance node)
            (distance (nodedp node) dp)
        }

    where 
        sortgo ct1 ct2 = compare (distance (nodedp node) (nodedp ct2)) (distance (nodedp node) (nodedp ct1))

        children' :: List (CoverTree' base childContainer nodeVvec tag dp)
        children' = go $ Strict.list2strictlist $ toList $ children node

        go Nil = (Node 
                    { nodedp = dp
                    , level = level node-1
                    , weight = w
                    , numdp = w
                    , maxDescendentDistance = 0
                    , children = mempty
                    , nodeV = mempty
                    , tag = mempty
                    })
               :.Nil
        go (x:.xs) = if isFartherThan (nodedp x) dp (sepdist x)
            then x:.go xs
            else case insert x (w,dp) of
                Strict.Just x' -> x':.xs

insertBatch :: forall base childContainer nodeVvec tag dp.
    ( ValidCT base childContainer nodeVvec tag dp
    ) => [dp] -> CoverTree' base childContainer nodeVvec tag dp
insertBatch ((!dp):dps) = go dps $ Node 
    { nodedp    = dp
    , level     = minBound
    , weight    = 1
    , numdp     = 1
    , tag       = mempty
    , children  = mempty
    , nodeV     = mempty
    , maxDescendentDistance = 0
    }
    where
        go [] tree = tree
        go (x:xs) tree = go xs $ safeInsert tree (1,x)

insertBatchVU :: forall base childContainer nodeVvec tag dp.
    ( ValidCT base childContainer nodeVvec tag dp
    , VG.Vector VU.Vector dp
    ) => VU.Vector dp -> CoverTree' base childContainer nodeVvec tag dp
insertBatchVU v = go 0 $ Node 
    { nodedp    = v VG.! 0 
    , level     = minBound
    , weight    = 1
    , numdp     = 1
    , tag       = mempty
    , children  = mempty
    , nodeV     = mempty
    , maxDescendentDistance = 0
    }
    where
        go i tree = if i >= VG.length v
            then tree
            else go (i+1) $ safeInsert tree (1,v VG.! i)

-------------------------------------------------------------------------------
-- algebra

-- instance VG.Foldable (CoverTree' tag) where
--     foldr f i ct = if Map.size (childrenMap ct) == 0
--         then f (nodedp ct) i
--         else foldr (\ct' i' -> VG.foldr f i' ct') i' (Map.elems $ childrenMap ct)
--         where
--             i' = if nodedp ct `Map.member` childrenMap ct
--                 then i
--                 else f (nodedp ct) i

instance 
    ( ValidCT base childContainer nodeVvec tag dp
    , VG.Vector childContainer (CoverTree' base childContainer nodeVvec tag dp)
    ) => Comonoid (CoverTree' base childContainer nodeVvec tag dp) 
        where
    partition n ct = [ takeFromTo (fromIntegral i*splitlen) splitlen ct | i <- [0..n-1] ] 
        where
            splitlen = fromIntegral $ ceiling $ toRational (numdp ct) / toRational n

-- split :: CoverTree' base childContainer nodeVvec tag dp -> (CoverTree' base childContainer nodeVvec tag dp,CoverTree' base childContainer nodeVvec tag dp)
-- split ct = (ct {children=childrenL}, ct { weight=0, children=childrenR })
--     where
--         scan
--         
--         midpt = ceiling $ toRational (numdp ct) / toRational n
        

-- partitionSize :: [Int] -> [CoverTree' base childContainer nodeVvec tag dp]
-- partitionSize xs = undefined
 
takeFromTo :: 
    ( ValidCT base childContainer nodeVvec tag dp
    , VG.Vector childContainer (CoverTree' base childContainer nodeVvec tag dp)
    ) => Ring dp -> Ring dp -> CoverTree' base childContainer nodeVvec tag dp -> CoverTree' base childContainer nodeVvec tag dp
takeFromTo from len ct = 
--   trace ("from="++show from++"; len="++show len++"; weight="++show (weight ct)++"; nodedp="++show (nodedp ct))$
  if len <= weight ct - from
    then ct 
        { weight = len-from
        , numdp = len-from
        , children = mempty 
        }
    else ct 
        { weight = weight' 
        , numdp = weight'+min (numdp ct-weight ct) (len-from) 
        , children = go (VG.length (children ct)-1) (from-weight ct) (len-weight') mempty 
        }
    where
        weight' = max 0 $ weight ct-from

        go i from' len' ret = --trace ("  go: i="++show i++"; from'="++show from'++"; len'="++show len') $ 
          if i<0 || len'<=0
            then ret
            else go (i-1) (max 0 $ from'-numdp child) (len'') (child' `mappend` ret)
                
            where 
                child = children ct VG.! i

                (child',len'') = if from' > numdp child
                    then (mempty,len')
                    else (VG.singleton $ takeFromTo from' len' child, len'-(numdp child-from'))

                

instance HasRing dp => HasRing (CoverTree' base childContainer nodeVvec tag dp) where
    type Ring (CoverTree' base childContainer nodeVvec tag dp) = Ring dp

instance
    ( ValidCT base childContainer nodeVvec tag dp
    ) => Semigroup (CoverTree' base childContainer nodeVvec tag dp) 
        where
    {-# INLINE (<>) #-}
    ct1 <> ct2 = case ctmerge' ct1' ct2' of
        Just (ct, []) -> ct
        Just (ct, xs) -> foldl' (<>) ct xs
        Nothing -> (growct ct1' (dist2level_down (sing::Sing base) $ distance (nodedp ct1') (nodedp ct2'))) <> ct2'
        where
            ct1' = growct ct1 maxlevel
            ct2' = growct ct2 maxlevel
            maxlevel = maximum [level ct1, level ct2, dist2level_down (sing::Sing base) $ distance (nodedp ct1) (nodedp ct2)]

ctmerge' :: forall base childContainer nodeVvec tag dp.
    ( ValidCT base childContainer nodeVvec tag dp
    ) => CoverTree' base childContainer nodeVvec tag dp 
      -> CoverTree' base childContainer nodeVvec tag dp 
      -> Maybe (CoverTree' base childContainer nodeVvec tag dp, [CoverTree' base childContainer nodeVvec tag dp])
ctmerge' ct1 ct2 = assert (level ct2 == level ct1) ("level ct2 == level ct1:" ++ show (sepdist ct1) ++","++show(sepdist ct2)) $
  if isFartherThan (nodedp ct1) (nodedp ct2) (sepdist ct1)
    then Nothing
    else Just 
        ( 
          flip safeInsert (stNodeW ct2) $ 
          ct1
            { children = children'
            , numdp = sum $ map numdp $ toList children'
            , maxDescendentDistance 
--                 = sepdist_parent ct1
--                 = maximum $ map (distance (nodedp ct1)) $ (nodedp ct2:stDescendents ct2++stDescendents ct1)
                = maximum $ map (distance (nodedp ct1)) $ (stToList ct2++stToList ct1)
--                 = maximum $ map (distance (nodedp ct1) . nodedp) $ Map.elems childrenMap'
            }
        , invalidchildren++invalid_newleftovers
        )
    where
        children' = fromList $ Strict.strictlist2list $ Strict.list2strictlist $ Map.elems childrenMap' 

        childrenMap ct = Map.fromList $ map (\v -> (nodedp v,v)) $ stChildren ct

        childrenMap' = newchildren `Map.union` Map.fromList 
--             (map (\x -> (nodedp x,growct x $ sepdist_child ct1)) valid_newleftovers)
            (map (\x -> (nodedp x,growct x $ level ct1-1)) valid_newleftovers)


        validchild x = not $ isFartherThan (nodedp ct1) (nodedp x) (sepdist ct1)
        (validchildren,invalidchildren) = Data.List.partition validchild $ Map.elems $ childrenMap ct2

        (newchildren,newleftovers) = 
            go (childrenMap ct1,[]) validchildren
        (valid_newleftovers,invalid_newleftovers) = Data.List.partition validchild newleftovers

        go (childmap,leftovers) [] = 
            (childmap,leftovers)
        go (childmap,leftovers) (x:xs) = 
--             case filter (isJust . snd) $ map (\(k,v) -> (k,ctmerge' v x)) $ Map.assocs childmap of
            case   
                filter (isJust . snd) $ map (\(k,v) -> (k,ctmerge'' v x)) $ Map.assocs childmap of
                    [] -> go 
--                         ( Map.insert (nodedp x) (x { sepdist = sepdist_child ct1 }) childmap
                        ( Map.insert (nodedp x) (x { level = level ct1-1 }) childmap
                        , leftovers
                        ) xs

                    (old, Just (new,leftovers')):ys ->
--                         go ( Map.insert (nodedp new) (new { sepdist = sepdist_child ct1 })
                        go ( Map.insert (nodedp new) (new { level = level ct1-1 })
                             $ Map.delete old childmap
                           , leftovers'++leftovers
                           ) xs
            where
                ctmerge'' ct1 ct2 = ctmerge' ct1 ct2
                    where
                        ct1' = growct ct1 maxlevel
                        ct2' = growct ct2 maxlevel
                        maxlevel = maximum [level ct1, level ct2, dist2level_down (sing::Sing base) $ distance (nodedp ct1) (nodedp ct2)]

--                     where
--                         a' = growct a maxsepdist
--                         b' = growct b maxsepdist
--                         maxsepdist = max (sepdist a) (sepdist b)

assert :: Bool -> String -> a -> a
assert b str a = (if b then id else trace ("assert: "++str)) a
-- assert b str a = if not b
--     then error $ "assert: "++str
--     else a


growct :: forall base childContainer nodeVvec tag dp.
    ( ValidCT base childContainer nodeVvec tag dp
    ) => CoverTree' base childContainer nodeVvec tag dp -> Int -> CoverTree' base childContainer nodeVvec tag dp
growct ct d = if sepdist ct==0 || stIsLeaf ct
    then ct { level=d }
    else if d <= level ct
        then ct
        else newleaf
            { level     = d
            , numdp     = numdp ct
            , children  = fromList [newct]
            , maxDescendentDistance = maximum $ map (distance (nodedp newleaf)) $ stToList newct
            }
    where
        (newleaf,newct) = rmleaf ct

rmleaf ct = if stIsLeaf (head childL)
    then (head childL, ct
        { numdp = numdp ct-1
        , children = fromList $ tail childL
        })
    else (itrleaf, ct
        { numdp = numdp ct-1
        , children = fromList $ itrtree:tail childL
        })
    where
        (itrleaf,itrtree) = rmleaf $ head childL
        childL = toList $ children ct
         
-- growct ct d = if sepdist ct==0 || stIsLeaf ct 
--     then ct { level=d }
--     else if d > level ct
--         then growct (Node
--             { nodedp    = nodedp ct
--             , level     = level ct+1
--             , weight    = 0 -- weight ct
--             , numdp     = numdp ct
--             , tag       = mempty
--             , children  = fromList $ Strict.strictlist2list $ ct:.Strict.Nil
--             , nodeV     = mempty
--             , maxDescendentDistance = maxDescendentDistance ct
--             }
--             ) d
--         else ct
--     where
--         coverfactor = fromSing (sing :: Sing base)

level2sepdist :: forall base num. (SingI (base::a), SingE (Kind::a) num, Num num, Floating num) => 
    Sing base -> Int -> num
level2sepdist _ l = (fromSing (sing :: Sing base))**(fromIntegral l)

dist2level_down :: forall base num. (SingI (base::a), SingE (Kind::a) num, RealFrac num, Floating num) => 
    Sing base -> num -> Int
dist2level_down _ d = floor $ log d / log (fromSing (sing::Sing base))

dist2level_up :: forall base num. (SingI (base::a), SingE (Kind::a) num, RealFrac num, Floating num) => 
    Sing base -> num -> Int
dist2level_up _ d = ceiling $ log d / log (fromSing (sing::Sing base))

sepdist :: forall base childContainer nodeVvec tag dp. (SingI base, Floating (Ring dp)) => 
    CoverTree' base childContainer nodeVvec tag dp -> Ring dp
sepdist ct = level2sepdist (undefined::Sing base) (level ct)

{-# INLINE coverDist #-}
coverDist :: forall base childContainer nodeVvec tag dp.
    ( Floating (Ring dp)
    , SingI base
    ) => CoverTree' base childContainer nodeVvec tag dp -> Ring dp
coverDist node = sepdist node*coverfactor 
    where
        coverfactor = fromSing (sing :: Sing base)

{-# INLINE sepdist_child #-}
sepdist_child :: forall base childContainer nodeVvec tag dp. (SingI base, MetricSpace dp, Floating (Ring dp)) => 
    CoverTree' base childContainer nodeVvec tag dp -> Ring dp
sepdist_child ct = next -- rounddown (sing::Sing base) $ next --next/10
    where next = sepdist ct/(fromSing (sing :: Sing base)) 

-- {-# INLINE sepdist_parent #-}
-- sepdist_parent :: forall base childContainer nodeVvec tag dp. (SingI base, MetricSpace dp) => 
--     CoverTree' base childContainer nodeVvec tag dp -> Ring dp
-- sepdist_parent ct = sepdist ct*(fromSing (sing :: Sing base))

{-# INLINE roundup #-}
roundup :: forall base d. 
    ( Floating d
    , RealFrac d
    , SingI base
    ) => Sing (base::Frac) -> d -> d
roundup s d = rounddown s $ d * coverfactor
    where
        coverfactor = fromSing (sing :: Sing base)

{-# INLINE rounddown #-}
rounddown :: forall base d. 
    ( Floating d
    , RealFrac d
    , SingI base
    ) => Sing (base::Frac) -> d -> d
-- rounddown _ d
--     | d == 1 = 1
--     | d >  1 = goup 1
--     | d <  1 = godown 1
--     where
--         godown i = if i>d/coverfactor
--             then godown (i/coverfactor)
--             else i
--         goup i = if i<d*coverfactor
--             then goup (i*coverfactor)
--             else i
--         coverfactor = fromSing (sing :: Sing base)
rounddown _ d = coverfactor^^(floor $ log d / log coverfactor :: Int)
    where
        coverfactor = fromSing (sing :: Sing base)

-- sepdistL :: Ord (Ring dp) => CoverTree' base childContainer nodeVvec tag dp -> [Ring dp]
-- sepdistL ct = Set.toList $ Set.fromList
--     $ sepdist ct : (mconcat $ map sepdistL $ VG.toList $ children ct)

---------------------------------------

{-
recover ct = foldl' safeInsert ct' xs
    where
        (ct', xs) = recover' ct

recover' :: forall base childContainer nodeVvec tag dp.
    ( MetricSpace dp
    
    , SingI base
    ) => CoverTree' base childContainer nodeVvec tag dp -> (CoverTree' base childContainer nodeVvec tag dp, [Weighted dp])
recover' ct = (ct', failed)
    where
        ct' = ct
            { childrenMap = pass
            , childrenList = Map.elems pass
            }
        
        (fail,pass) = Map.partition 
            (\c -> not $ isFartherThan (nodedp ct) (nodedp c) (coverDist ct)) 
            (childrenMap ct)
        

        failed = concatMap stToListW $ Map.elems fail 

-- unsafeMap :: forall base childContainer nodeVvec tag dp1 dp2.
--     ( MetricSpace dp2
--     , Ring dp1 ~ Ring dp2
--     2
--     , SingI base
--     ) => (dp1 -> dp2) -> AddUnit (CoverTree' base) tag dp1 -> AddUnit (CoverTree' base) tag dp2
unsafeMap f Unit = Unit
unsafeMap f (UnitLift ct) = UnitLift $ unsafeMap' f ct

unsafeMap' :: forall base childContainer nodeVvec tag dp1 dp2.
    ( MetricSpace dp2
    , Ring dp1 ~ Ring dp2
    2
    , SingI base
    ) => (dp1 -> dp2) -> CoverTree' base childContainer nodeVvec tag dp1 -> CoverTree' base childContainer nodeVvec tag dp2
unsafeMap' f ct = Node
    { nodedp = nodedp'
    , weight = weight ct
    , numdp = numdp ct
    , sepdist = sepdist ct
    , tag = tag ct
    , childrenMap = childrenMap'
    , childrenList = childrenList'
--     , maxDescendentDistance = maxDescendentDistance ct
    , maxDescendentDistance = maximum $ 0:map (\c -> distance (nodedp c) nodedp' + maxDescendentDistance c) childrenList'
    }
    where
        nodedp' = f $ nodedp ct
        childrenMap' = Map.fromList $ map (\c -> (nodedp c,c)) $ map (unsafeMap' f) $ childrenList ct 
        childrenList' = Map.elems childrenMap'

ctmap f Unit = Unit
ctmap f (UnitLift ct) = UnitLift $ ctmap' f ct

ctmap' :: forall base childContainer nodeVvec tag dp1 dp2.
    ( MetricSpace dp2
    , Ring dp1 ~ Ring dp2
    , Floating (Ring dp1)
    2
    , Monoid tag
    , SingI base
    ) => (dp1 -> dp2) -> CoverTree' base childContainer nodeVvec tag dp1 -> CoverTree' base childContainer nodeVvec tag dp2
ctmap' f ct = recover $ unsafeMap' f ct
 

implicitChildrenMap ct = Map.union (childrenMap ct) (Map.singleton (nodedp ct) $ ct
    { nodedp = nodedp ct
    , sepdist = sepdist_child ct
    , weight = 0
    , numdp = 0
    , tag = tag ct
    , childrenMap = mempty
    , childrenList = mempty
    , maxDescendentDistance = 0
    })

extractLeaf :: forall base childContainer nodeVvec tag dp.
    ( MetricSpace dp 
    
    , SingI base
    ) => CoverTree' base childContainer nodeVvec tag dp -> (Weighted dp, Maybe (CoverTree' base childContainer nodeVvec tag dp))
extractLeaf ct = if stIsLeaf ct
    then (stNodeW ct, Nothing)
    else (leaf, Just $ ct
            { childrenMap = childrenMap'
            , childrenList = Map.elems childrenMap'
            }
         )
        where
            (leaf,c) = extractLeaf . head . Map.elems $ childrenMap ct

            childrenMap' = case c of
                Nothing -> Map.fromList $ tail $ Map.toList $ childrenMap ct
                Just c' -> Map.fromList $ (nodedp c', c'):(tail $ Map.toList $ childrenMap ct)
-}

-- setLeafSize :: (MetricSpace dp, SingI base) => Int -> CoverTree' base childContainer nodeVvec tag dp -> CoverTree' base childContainer nodeVvec tag dp
-- setLeafSize n ct = if stNumNodes ct < n
--     then ct { children = fmap singleton $ Strict.list2strictlist $ stToListW ct } 
--     else ct { children = fmap (setLeafSize n) $ children ct }
--     where
-- --         singleton :: Weighted dp -> CoverTree' base childContainer nodeVvec tag dp
--         singleton (w,dp) = Node
--             { nodedp = dp
--             , weight = w
--             , numdp = w
--             , sepdist = sepdist_child ct
--             , maxDescendentDistance = 0
--             , children = mempty
--             , tag = tag ct
--             }
             

-------------------------------------------------------------------------------
-- training

instance HasRing dp => NumDP (CoverTree' base childContainer nodeVvec tag dp) where
    numdp = numdp

instance 
    ( ValidCT base childContainer nodeVvec tag dp
--     ( MetricSpace dp
--     , Ord (Ring dp)
--     , Floating (Ring dp)
--     , Ord dp
--     , Show dp
--     , Show tag
--     , Show (Ring dp)
--     , Monoid (nodeVvec dp)
--     , Monoid tag
--     , SingI base
--     , Monoid (childContainer (CoverTree' base childContainer nodeVvec tag dp))
--     , Container childContainer
-- --     , Container nodeVvec
--     , VG.Vector nodeVvec dp
    ) => HomTrainer (AddUnit (CoverTree' base childContainer nodeVvec) tag dp) 
        where
    type Datapoint (AddUnit (CoverTree' base childContainer nodeVvec) tag dp) = dp

    {-# INLINE train1dp #-}
    train1dp dp = UnitLift $ Node 
        { nodedp    = dp
        , level     = minBound
        , weight    = 1
        , numdp     = 1
        , tag       = mempty
        , children  = mempty
        , nodeV     = mempty
        , maxDescendentDistance = 0
        }

    {-# INLINABLE train #-}
    train = UnitLift . insertBatch . F.toList

trainct_insert = UnitLift . insertBatch . VG.toList

-------------------------------------------------------------------------------
-- tests

instance 
    ( ValidCT base childContainer nodeVvec tag (Double,Double)
    ) => Arbitrary (AddUnit (CoverTree' base childContainer nodeVvec) tag (Double,Double)) 
        where
    arbitrary = do
        num :: Int <- choose (1,100)
--         xs <- replicateM num arbitrary
        xs <- replicateM num $ do
--             x <- arbitrary
--             y <- arbitrary
            x <- choose (-2^^50,2^^50)
            y <- choose (-2^^50,2^^50)
            return (x,y)
--         return $ unUnit $ train xs 
        return $ train xs 


-- property_all :: CoverTree (Double,Double) -> Bool
property_all ct = and $ map (\x -> x ct)
    [ property_covering
    , property_leveled
    , property_separating
    , property_maxDescendentDistance
    ]

-- property_covering :: (MetricSpace dp) => CoverTree dp -> Bool
property_covering Unit = True
property_covering (UnitLift node) = if not $ stIsLeaf node
    then VG.maximum (fmap (distance (nodedp node) . nodedp) $ children node) < coverDist node 
      && VG.and (fmap (property_covering . UnitLift) $ children node)
    else True

-- property_leveled  :: MetricSpace dp => CoverTree dp -> Bool
property_leveled (Unit) = True
property_leveled (UnitLift node)
    = VG.all (\val -> val - head (VG.toList xs) < 0.0000001) xs
--     = VG.all (== VG.head xs) xs
   && VG.and (fmap (property_leveled . UnitLift) $ children node)
    where
        xs = fmap sepdist $ children node

-- property_separating  :: (Ord dp, MetricSpace dp) => CoverTree dp -> Bool
property_separating Unit = True
property_separating (UnitLift node) = if length (VG.toList $ children node) > 1 
    then VG.foldl1 min ((mapFactorial stMaxDistance) $ children node) > sepdist_child node
      && VG.and (fmap (property_separating . UnitLift) $ children node)
    else True
    where
--         mapFactorial :: Container v =>(a -> a -> b) -> v a -> v b
        mapFactorial :: (VG.Vector v a, VG.Vector v b) =>(a -> a -> b) -> v a -> v b
        mapFactorial f = VG.fromList . Strict.strictlist2list . mapFactorial' f . Strict.list2strictlist . VG.toList
        mapFactorial' :: (a -> a -> b) -> List a -> List b
        mapFactorial' f xs = go xs Nil
            where
                go Nil ys = ys
                go (x:.xs) ys = go xs (fmap (f x) xs `mappend` ys)
-- property_separating (UnitLift node) = if Strict.length (children node) > 1 
--     then VG.foldl1 min ((mapFactorial stMaxDistance) $ children node) > sepdist_child node
--       && VG.and (fmap (property_separating . UnitLift) $ children node)
--     else True
--     where
--         mapFactorial :: (a -> a -> b) -> List a -> List b
--         mapFactorial f xs = go xs Nil
--             where
--                 go Nil ys = ys
--                 go (x:.xs) ys = go xs (fmap (f x) xs `mappend` ys)

-- property_maxDescendentDistance  :: (Ord dp, MetricSpace dp) => CoverTree dp -> Bool
property_maxDescendentDistance Unit = True
property_maxDescendentDistance (UnitLift node) 
    = and (map (property_maxDescendentDistance . UnitLift) $ stChildren node)
   && and (map (\dp -> distance dp (nodedp node) <= maxDescendentDistance node) $ stDescendents node)
-- property_maxDescendentDistance (UnitLift node) = if stIsLeaf node
--     then True
--     else and (map (property_maxDescendentDistance . UnitLift) $ stChildren node)
--       && and (map (\dp -> weight node == 1 
--                        || distance dp (nodedp node) <= maxDescendentDistance node) $ stDescendents node)

-- property_validmerge :: 
--     (CoverTree (Double,Double) -> Bool) -> CoverTree (Double,Double) -> CoverTree (Double,Double) -> Bool
property_validmerge prop (UnitLift ct1) (UnitLift ct2) = prop . UnitLift $ ct1 <> ct2

property_lossless :: [(Double,Double)] ->  Bool
property_lossless [] = True
property_lossless xs = Set.fromList xs == dpSet ct
    where
        UnitLift ct = train xs :: AddUnit (CoverTree' (2/1) V.Vector V.Vector) () (Double,Double)

        dpSet :: (Ord dp) => CoverTree' base V.Vector V.Vector tag dp -> Set.Set dp
        dpSet = Set.fromList . dpList
            where
                dpList :: CoverTree' base V.Vector V.Vector tag dp -> [dp]
--                 dpList node = nodedp node:(Strict.concat . fmap dpList . children node)
                dpList node = nodedp node:(concat . fmap dpList . V.toList $ children node)

-- property_numdp :: (Ord dp, MetricSpace dp) => CoverTree dp -> Bool
property_numdp Unit = True
property_numdp (UnitLift node) = numdp node == sum (map fst $ stToListW node)


---------------------------------------

instance MkCentroid (Double,Double) where
    mkCentroid (a1,a2) (b1,b2) = ((a1+b1)/2,(a2+b2)/2)

randL :: Int -> IO [(Double,Double)]
randL n = replicateM n $ do
    x <- randomRIO (-100,100)
    y <- randomRIO (-100,100)
    return (fromIntegral (x :: Int), fromIntegral (y :: Int))

dp1 = (1,1) :: (Double,Double)
dp2 = (2,1) :: (Double,Double)
-- m1 = unUnit (train1dp dp1 :: CoverTree (Double,Double))
-- m2 = unUnit (train1dp dp2 :: CoverTree (Double,Double))

ys :: [(Double,Double)]
-- ys = [(-2,2),(1,1),(0,0),(1,-1),(0,1),(1,0)]
-- ys = [(0,0),(0,10),(10,10),(10,0),(10,-10),(0,-10),(-10,-10),(-10,0),(-10,10)]
ys = [(0,0),(0,10),(8,8),(10,0),(8,-8),(0,-10),(-8,-8),(-10,0),(-8,8)]
-- my = train ys :: CoverTree (Double,Double)
-- my2 = train $ take 3 ys :: CoverTree (Double,Double)
-- my = prunect $ insertBatch ys

ys' :: [(Double,Double)]
ys' = [(1,2),(2,1)]
-- my' = train ys' :: CoverTree (Double,Double)
-- my' = prunect $ insertBatch ys'

zs :: [(Double,Double)]
-- zs = [(20,21),(22,23),(21,22),(30,20),(20,20),(19,20),(20,10),(22,21)]
zs = [(20,21),(22,23),(21,22),(30,20),(20,20),(20,10),(22,21)]
mz = train zs :: CoverTreeVU (Double,Double)

type CoverTreeVU dp = AddUnit (CoverTree' (2/1) V.Vector VU.Vector) () dp

-- mq = mz `mappend` my

grid :: Int -> Int -> [(Double,Double)]
grid n 2 = take n [(x,y) | x <- [1..w], y <- [1..w]]
    where
        n' = fromIntegral n
        w = fromIntegral $ ceiling $ n' ** (1/2)

gs = grid 9 2
gs' i = take i gs
-- mg = train gs :: CoverTree (Double,Double)
-- mg' i = train $ gs' i :: CoverTree (Double,Double)

-- mg3 = train1dp (1,1) `mappend` ( train1dp (1,2) `mappend` train1dp (1,3) ) :: CoverTree (Double,Double)

-------------------------------------------------------------------------------
-- diagrams

drawT ct1 ct2 = draw ct1 
            ||| (text "<>" <> strutX 1.5)
            ||| draw ct2 
            ||| (text "=" <> strutX 1) 
            ||| (draw $ ct1 `mappend` ct2)

draw node = draw' 0 node
draw' depth tree = mkConnections $ 
                   (named (label++show depth) $ fontSize 0.5 $ 
                        (
                             (text label <> strutY 0.5) 
                         === (text (show (sepdist tree)) <> strutY 0.5) 
--                          === (text (show (maxDescendentDistance tree)) <> strutY 0.5)
                        ) 
                          <> circle 1 # fc nodecolor) 
               === (pad 1.05 $ centerName (label++show (depth+1)) $ 
                   VG.foldr (|||) mempty $ fmap (draw' (depth+1)) $ children tree)
                
    where
        label = intShow $ nodedp tree
        nodecolor = if weight tree > 0
            then red
            else lightblue

        mkConnections = 
             connect (label++show depth) (label++show (depth+1)) 
             . apList (fmap (\key -> connect (label++show depth) (intShow key++show (depth+1))) (fmap nodedp $ Strict.list2strictlist $ VG.toList $ children tree))
            
justdouble :: Maybe Double -> String
justdouble Nothing = "0"
justdouble (Just x) = show x

apList :: List (a -> a) -> a -> a
apList Strict.Nil a = a
apList (x:.xs) a = apList xs (x a)

centerName name = withName name $ \b a -> moveOriginTo (location b) a

connect n1 n2
    = withName n1 $ \b1 ->
      withName n2 $ \b2 ->
        atop ((location b1 ~~ location b2) # lc green # lw 0.03)
--          ((location b1 ~~ location b2) # lc green # lw 0.03)


class IntShow a where
    intShow :: a -> String

instance IntShow (Double,Double) where
    intShow (x,y) = show (floor x::Int,floor y::Int)

instance (VG.Vector (L2 v) r, RealFrac r) => IntShow (L2 v r) where
    intShow v = show (map floor $ VG.toList v)

instance (IntShow attr,Show label) => IntShow (MaybeLabeled label attr) where
    intShow dp = "("++label++","++intShow (getAttributes dp)++")"
        where
            label = case getLabel dp of
                Just x -> show x
                Nothing -> "_"
