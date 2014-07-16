{-# LANGUAGE NoMonomorphismRestriction,DataKinds,PolyKinds,MagicHash,UnboxedTuples #-}

module HLearn.DataStructures.CoverTree
    ({- CoverTree
    , -}CoverTree'

    , insertBatch

    -- * unsafe
--     , ctmap
--     , unsafeMap
--     , recover
--     , trainct_insert
    , packCT
    , packCT2
    , packCT3

    -- * drawing
--     , draw
--     , draw'
--     , IntShow (..)

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
import Data.Semigroup
import Data.Primitive
import Data.Proxy
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
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Primitive.Mutable as VPM

import Test.QuickCheck
import Debug.Trace

-- import Diagrams.Prelude hiding (distance,trace,query,connect)
-- import Diagrams.Backend.SVG.CmdLine
-- import Diagrams.Backend.Postscript.CmdLine

import qualified Control.ConstraintKinds as CK
import HLearn.Algebra hiding ((<>),(|>),numdp)
import qualified HLearn.Algebra
import HLearn.DataStructures.SpaceTree
import HLearn.DataStructures.SpaceTree.DualTreeMonoids
import HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor hiding (weight)
import HLearn.DataStructures.SpaceTree.Algorithms.RangeSearch
import qualified HLearn.DataStructures.StrictList as Strict
import HLearn.DataStructures.StrictList (List (..))

import HLearn.UnsafeVector
import HLearn.Models.Classifiers.Common
import HLearn.Metrics.Lebesgue

-------------------------------------------------------------------------------
-- data types

type CoverTree dp = AddUnit (CoverTree' (2/1) V.Vector V.Vector) () dp

data CoverTree' 
        ( base                  :: Frac ) 
        ( childContainer        :: * -> * ) 
        ( nodeContainer         :: * -> * ) 
        ( tag                   :: * )
        ( dp                    :: * )
    = Node 
        { nodedp                :: {-#UNPACK#-}!(L2 VU.Vector Float)
        , level                 :: {-#UNPACK#-}!Int
--         , weight                :: {-#UNPACK#-}!Float
        , numdp                 :: {-#UNPACK#-}!Float
        , maxDescendentDistance :: {-#UNPACK#-}!Float
        , children              :: {-#UNPACK#-}!(V.Vector (CoverTree' base childContainer VU.Vector tag dp))
        , nodeV                 :: {-#UNPACK#-}!(VU.Vector (L2 VU.Vector Float))
--         , tag                   :: !tag
        }

{-# INLINE weight #-}
weight :: CoverTree' base childContainer nodeContainer tag dp -> Float
weight _ = 1

---------------------------------------
-- standard instances

-- deriving instance 
--     ( Read (Scalar dp)
--     , Read (childContainer (CoverTree' base childContainer nodeContainer tag dp))
--     , Read (nodeContainer dp)
--     , Read tag
--     , Read dp
--     , ValidCT base childContainer nodeContainer tag dp
--     ) => Read (CoverTree' base childContainer nodeContainer tag dp)

deriving instance 
    ( Show (Scalar dp)
    , Show (childContainer (CoverTree' base childContainer nodeContainer tag dp))
    , Show (nodeContainer dp)
    , Show tag
    , Show dp
    , ValidCT base childContainer nodeContainer tag dp
    ) => Show (CoverTree' base childContainer nodeContainer tag dp)

instance 
    ( NFData dp
    , NFData (Scalar dp)
    , NFData tag
    , ValidCT base childContainer nodeContainer tag dp
    ) => NFData (CoverTree' base childContainer nodeContainer tag dp) 
        where
    rnf ct = rnf $ children ct
--     rnf ct = deepseq (numdp ct) 
--            $ deepseq (maxDescendentDistance ct)
--            $ seq ct
--            $ ()

---------------------------------------
-- non-standard instances
--
class 
    ( MetricSpace dp
    , Ord (Scalar dp)
    , Floating (Scalar dp)
    , Monoid tag
    , Monoid (nodeContainer dp)
    , Monoid (childContainer dp)
    , Monoid (childContainer (CoverTree' base childContainer nodeContainer tag dp))
    , FromList nodeContainer dp
    , FromList childContainer dp
    , FromList childContainer (Scalar dp)
    , FromList childContainer (CoverTree' base childContainer nodeContainer tag dp)
    , KnownFrac base
    , Show (Scalar dp)
    , Show dp
    , Ord dp
    , VU.Unbox dp
    , nodeContainer ~ VU.Vector
--     , Prim dp
--     , nodeContainer ~ VP.Vector
    , childContainer ~ V.Vector
    , VG.Vector nodeContainer dp
    , VG.Vector childContainer (CoverTree' base childContainer nodeContainer tag dp)
    , dp ~ L2 VU.Vector Float
    , Scalar dp ~ Float
    ) => ValidCT base childContainer nodeContainer tag dp

instance 
    ( MetricSpace dp
    , Ord (Scalar dp)
    , Floating (Scalar dp)
    , Monoid tag
    , Monoid (nodeContainer dp)
    , Monoid (childContainer dp)
    , Monoid (childContainer (CoverTree' base childContainer nodeContainer tag dp))
    , FromList nodeContainer dp
    , FromList childContainer dp
    , FromList childContainer (Scalar dp)
    , FromList childContainer (CoverTree' base childContainer nodeContainer tag dp)
    , KnownFrac base
    , Show (Scalar dp)
    , Show dp
    , Ord dp
    , VU.Unbox dp
    , nodeContainer ~ VU.Vector
--     , Prim dp
--     , nodeContainer ~ VP.Vector
    , childContainer ~ V.Vector
    , VG.Vector nodeContainer dp
    , VG.Vector childContainer (CoverTree' base childContainer nodeContainer tag dp)
    , dp ~ L2 VU.Vector Float
    , Scalar dp ~ Float
    ) => ValidCT base childContainer nodeContainer tag dp

type instance Scalar (CoverTree' base childContainer nodeContainer tag dp) = Scalar dp

instance 
    ( ValidCT base childContainer nodeContainer tag dp
    ) => SpaceTree (CoverTree' base childContainer nodeContainer tag) dp
        where

    type NodeContainer (CoverTree' base childContainer nodeContainer tag) = nodeContainer 
    type ChildContainer (CoverTree' base childContainer nodeContainer tag) = childContainer

    {-# INLINABLE stMinDistance #-}
    {-# INLINABLE stMaxDistance #-}
    {-# INLINABLE stMinDistanceDpWithDistance #-}
    {-# INLINABLE stMaxDistanceDpWithDistance #-}
    {-# INLINABLE stMinDistanceDpFromDistance #-}
    {-# INLINABLE stMaxDistanceDpFromDistance #-}
    {-# INLINABLE stIsMinDistanceDpFartherThanWithDistance #-}
    {-# INLINABLE stIsMaxDistanceDpFartherThanWithDistance #-}

    stMinDistanceWithDistance !ct1 !ct2 = 
        (# dist-(maxDescendentDistance ct1)-(maxDescendentDistance ct2), dist #)
        where dist = distance (nodedp ct1) (nodedp ct2) 
    stMaxDistanceWithDistance !ct1 !ct2 = 
        (# dist+(maxDescendentDistance ct1)+(maxDescendentDistance ct2), dist #)
        where dist = distance (nodedp ct1) (nodedp ct2) 

    stMinDistanceDpWithDistance !ct !dp = 
        (# dist - maxDescendentDistance ct, dist #)
        where dist = distance (nodedp ct) dp
    stMaxDistanceDpWithDistance !ct !dp = 
        (# dist + maxDescendentDistance ct, dist #)
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

    {-# INLINE stChildren #-}
    {-# INLINE stNode #-}
    {-# INLINE stNodeV #-}
    {-# INLINE stHasNode #-}
    {-# INLINE stIsLeaf #-}
    stChildren  = children
    stNodeV     = nodeV
    stNode      = nodedp
    stWeight    = weight
    stHasNode _ = True
    stIsLeaf ct = null $ toList $ children ct

    ro _ = 0
    lambda !ct = maxDescendentDistance ct 

-------------------------------------------------------------------------------
-- 

packCT :: 
    ( ValidCT base childContainer nodeContainer tag dp
    , VG.Vector nodeContainer dp
    ) => CoverTree' base childContainer nodeContainer tag dp 
      -> CoverTree' base childContainer nodeContainer tag dp
packCT ct = snd $ go 0 ct'
    where
        go i t = (i',t
            { nodedp = v VG.! i
            , nodeV = VG.slice (i+1) (VG.length $ nodeV t) v
            , children = fromList children'
            })
            where
                (i',children') = mapAccumL 
                    go 
                    (i+1+length (toList $ nodeV t)) 
                    (sortBy sortgo $ toList $ children t)

        ct' =  setNodeV 0 ct
        v = fromList $ mkNodeList ct'

        sortgo a b = compare (numdp a) (numdp b)
                  <> compare (distance (nodedp ct) (nodedp a)) (distance (nodedp ct) (nodedp b))

        mkNodeList ct = [nodedp ct] 
                     ++ (toList $ nodeV ct) 
                     ++ (concatMap mkNodeList $ sortBy sortgo $ toList $ children ct)


setNodeV :: 
    ( ValidCT base childContainer nodeContainer tag dp
    ) => Int 
      -> CoverTree' base childContainer nodeContainer tag dp 
      -> CoverTree' base childContainer nodeContainer tag dp
setNodeV n ct = if stNumNodes ct > n
    then ct
        { children = fromList $ fmap (setNodeV n) $ filter (not . stIsLeaf) $ toList $ children ct
        , nodeV = fromList $ fmap nodedp $ filter stIsLeaf $ toList $ children ct
        }
    else ct
        { children = mempty
        , nodeV = fromList $ stToList ct
        }

packCT3 :: 
    ( ValidCT base childContainer nodeContainer tag dp
    , VG.Vector nodeContainer dp
    , VUM.Unbox dp
    ) => CoverTree' base childContainer nodeContainer tag dp 
      -> CoverTree' base childContainer nodeContainer tag dp
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
    ( ValidCT base childContainer nodeContainer tag dp
    , VG.Vector nodeContainer dp
    , VUM.Unbox dp
    ) => Int 
      -> CoverTree' base childContainer nodeContainer tag dp 
      -> CoverTree' base childContainer nodeContainer tag dp
packCT2 n ct = snd $ go 1 $ ct' { nodedp = v VG.! 0 }
    where
        go i t = (i',t
            { nodeV = VG.slice i (VG.length $ nodeV t) v
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

-------------------------------------------------------------------------------
-- insertion as described in the paper

safeInsert :: forall base childContainer nodeContainer tag dp.
    ( ValidCT base childContainer nodeContainer tag dp
    ) => CoverTree' base childContainer nodeContainer tag dp 
      -> Weighted dp 
      -> CoverTree' base childContainer nodeContainer tag dp
safeInsert node (0,_) = node
safeInsert node (w,dp) = case insert node (w,dp) of
    Just x -> x
    Nothing -> Node
        { nodedp    = dp
        , level     = dist2level_up (Proxy::Proxy base) dist
--         , weight    = w
        , numdp     = numdp node+1
        , children  = if stIsLeaf node
            then fromList [node { level = dist2level_down (Proxy::Proxy base) dist } ]
            else fromList [node]
        , nodeV     = mempty
        , maxDescendentDistance = maximum $ map (distance dp) $ stToList node
--         , tag       = mempty
        }
        where
            dist = distance (nodedp node) dp

insert :: forall base childContainer nodeContainer tag dp.
    ( ValidCT base childContainer nodeContainer tag dp
    ) => CoverTree' base childContainer nodeContainer tag dp 
      -> Weighted dp 
      -> Maybe (CoverTree' base childContainer nodeContainer tag dp)
insert node (w,dp) = if isFartherThan dp (nodedp node) (sepdist node)
    then Nothing
    else Just $ Node
        { nodedp    = nodedp node
        , level     = level node
--         , weight    = weight node
        , numdp     = weight node + sum (map numdp children')
        , children  = fromList $ sortBy sortgo children'
        , nodeV     = mempty
        , maxDescendentDistance = max
            (maxDescendentDistance node)
            (distance (nodedp node) dp)
--         , tag       = tag node
        }

    where 
        sortgo ct1 ct2 = compare 
            (distance dp (nodedp ct1)) 
            (distance dp (nodedp ct2))
--         sortgo ct1 ct2 = compare 
--             (distance (nodedp node) (nodedp ct2)) 
--             (distance (nodedp node) (nodedp ct1))

--         children' = go $ sortBy sortgo $ toList $ children node
        children' = go $ toList $ children node

        go [] = [ Node 
                    { nodedp   = dp
                    , level    = level node-1
--                     , weight   = w
                    , numdp    = w
                    , children = mempty
                    , nodeV    = mempty
                    , maxDescendentDistance = 0
--                     , tag      = mempty
                    }
                ]
        go (x:xs) = if isFartherThan (nodedp x) dp (sepdist x)
            then x:go xs
            else case insert x (w,dp) of
                Just x' -> x':xs

insertBatch :: forall base childContainer nodeContainer tag dp.
    ( ValidCT base childContainer nodeContainer tag dp
    ) => [dp] 
      -> CoverTree' base childContainer nodeContainer tag dp
insertBatch (dp:dps) = go dps $ Node 
    { nodedp    = dp
    , level     = minBound
--     , weight    = 1
    , numdp     = 1
    , children  = mempty
    , nodeV     = mempty
    , maxDescendentDistance = 0
--     , tag       = mempty
    }
    where
        go [] tree = tree
        go (x:xs) tree = go xs $ safeInsert tree (1,x)

-------------------------------------------------------------------------------
-- algebra

type instance Scalar (CoverTree' base childContainer nodeContainer tag dp) = Scalar dp

-- instance VG.Foldable (CoverTree' tag) where
--     foldr f i ct = if Map.size (childrenMap ct) == 0
--         then f (nodedp ct) i
--         else foldr (\ct' i' -> VG.foldr f i' ct') i' (Map.elems $ childrenMap ct)
--         where
--             i' = if nodedp ct `Map.member` childrenMap ct
--                 then i
--                 else f (nodedp ct) i

instance 
    ( ValidCT base childContainer nodeContainer tag dp
    ) => Comonoid (CoverTree' base childContainer nodeContainer tag dp) 
        where
    partition n ct = [ takeFromTo (fromIntegral i*splitlen) splitlen ct | i <- [0..n-1] ]  
        where
            splitlen = fromIntegral (ceiling $ toRational (numdp ct) / toRational n::Int)

takeFromTo :: forall base childContainer nodeContainer tag dp.
    ( ValidCT base childContainer nodeContainer tag dp
    ) => Scalar dp 
      -> Scalar dp 
      -> CoverTree' base childContainer nodeContainer tag dp 
      -> CoverTree' base childContainer nodeContainer tag dp
takeFromTo from len ct = 
     ct
--         { weight = nodeweight
        { nodeV = nodeV'
        , children = children'
        }
    where
        nodeweight :: Scalar dp
        nodeweight = if from <= 0
            then min len (weight ct)
            else 0

        nodeV' = fromList $ take (round len) $ drop (round $ from-nodeweight) $ toList $ nodeV ct

--         taken = nodeweight+ (fromIntegral $ VG.length nodeV') :: Scalar dp
--         nottaken = 1-nodeweight+(fromIntegral $ (VG.length $ nodeV ct)-(VG.length nodeV')) :: Scalar dp
        taken = nodeweight+ (fromIntegral $ length $ toList nodeV') :: Scalar dp
        nottaken = 1-nodeweight+(fromIntegral $ (length $ toList $ nodeV ct)-(length $ toList nodeV')) :: Scalar dp

        children' = fromList $ snd $ mapAccumL mapgo (from-nottaken,len-taken) $ toList $ children ct
        
        mapgo (from',len') child = 
            ((from'',len''),takeFromTo from' len' child)
            where
                from'' = if from' == 0
                    then 0
                    else max 0 $ from' - numdp child

                len'' = if from' == 0
                    then max 0 $ len' - numdp child
                    else if from' - len' > 0
                        then len'
                        else len'-(max 0 $ numdp child-from')
                

instance
    ( ValidCT base childContainer nodeContainer tag dp
    ) => Semigroup (CoverTree' base childContainer nodeContainer tag dp) 
        where
    {-# INLINABLE (<>) #-}
    ct1 <> ct2 = case ctmerge' ct1' ct2' of
        Strict.Just (ct, []) -> ct
        Strict.Just (ct, xs) -> foldl' (<>) ct xs
        Strict.Nothing -> 
            (growct 
                ct1' 
                (dist2level_up (Proxy::Proxy base) $ distance (nodedp ct1') (nodedp ct2'))
            ) <> ct2'
        where
            ct1' = growct ct1 maxlevel
            ct2' = growct ct2 maxlevel
            maxlevel = maximum 
                [ level ct1
                , level ct2
                , dist2level_down (Proxy::Proxy base) $ distance (nodedp ct1) (nodedp ct2)
                ]

ctmerge' :: forall base childContainer nodeContainer tag dp.
    ( ValidCT base childContainer nodeContainer tag dp
    ) => CoverTree' base childContainer nodeContainer tag dp 
      -> CoverTree' base childContainer nodeContainer tag dp 
      -> Strict.Maybe 
            ( CoverTree' base childContainer nodeContainer tag dp
            , [CoverTree' base childContainer nodeContainer tag dp]
            )
ctmerge' ct1 ct2 = 
  if isFartherThan (nodedp ct1) (nodedp ct2) (sepdist ct1)
    then Strict.Nothing
    else Strict.Just 
        ( 
          flip safeInsert (stNodeW ct2) $ 
          ct1
            { children = children'
            , numdp = sum $ map numdp $ toList children'
            , maxDescendentDistance 
                = maximum $ map (distance (nodedp ct1)) $ (stToList ct2++stToList ct1)
            }
        , invalidchildren++invalid_newleftovers
        )
    where
--         children' = fromList $ Strict.strictlist2list $ Strict.list2strictlist $ Map.elems childrenMap' 
        children' = fromList $ Map.elems childrenMap' 

        childrenMap ct = Map.fromList $ map (\v -> (nodedp v,v)) $ stChildrenList ct

        childrenMap' = newchildren `Map.union` Map.fromList 
            (map (\x -> (nodedp x,growct x $ level ct1-1)) valid_newleftovers)


        validchild x = not $ isFartherThan (nodedp ct1) (nodedp x) (sepdist ct1)
        (validchildren,invalidchildren) = Data.List.partition validchild $ Map.elems $ childrenMap ct2

        (newchildren,newleftovers) = go (childrenMap ct1,[]) validchildren
        (valid_newleftovers,invalid_newleftovers) = Data.List.partition validchild newleftovers

        go (childmap,leftovers) []     = (childmap,leftovers)
        go (childmap,leftovers) (x:xs) = 
            case   
                filter (Strict.isJust . snd) $ map (\(k,v) -> (k,ctmerge'' v x)) $ Map.assocs childmap of
                    [] -> go 
                        ( Map.insert (nodedp x) (x { level = level ct1-1 }) childmap
                        , leftovers
                        ) xs

                    (old, Strict.Just (new,leftovers')):ys ->
                        go ( Map.insert (nodedp new) (new { level = level ct1-1 })
                             $ Map.delete old childmap
                           , leftovers'++leftovers
                           ) xs
            where
                ctmerge'' ct1 ct2 = ctmerge' ct1 ct2
                    where
                        ct1' = growct ct1 maxlevel
                        ct2' = growct ct2 maxlevel
                        maxlevel = maximum 
                            [ level ct1
                            , level ct2
                            , dist2level_down (Proxy::Proxy base) $ distance (nodedp ct1) (nodedp ct2)
                            ]

-------------------------------------------------------------------------------
-- misc helper functions

growct :: forall base childContainer nodeContainer tag dp.
    ( ValidCT base childContainer nodeContainer tag dp
    ) => CoverTree' base childContainer nodeContainer tag dp 
      -> Int 
      -> CoverTree' base childContainer nodeContainer tag dp
growct = growct_unsafe

growct_safe :: forall base childContainer nodeContainer tag dp.
    ( ValidCT base childContainer nodeContainer tag dp
    ) => CoverTree' base childContainer nodeContainer tag dp 
      -> Int 
      -> CoverTree' base childContainer nodeContainer tag dp
growct_safe ct d = if sepdist ct==0 || stIsLeaf ct 
    then ct { level=d }
    else if d > level ct
        then growct (Node
            { nodedp    = nodedp ct
            , level     = level ct+1
--             , weight    = 0 -- weight ct
            , numdp     = numdp ct
            , children  = fromList [ct]
            , nodeV     = mempty
            , maxDescendentDistance = maxDescendentDistance ct
--             , tag       = mempty
            }
            ) d
        else ct
--     where
--         coverfactor = fromRational $ fracVal (Proxy :: Proxy base)

-- | this version of growct does not strictly obey the separating property; it never creates ghosts, however, so seems to work better in practice
growct_unsafe :: forall base childContainer nodeContainer tag dp.
    ( ValidCT base childContainer nodeContainer tag dp
    ) => CoverTree' base childContainer nodeContainer tag dp 
      -> Int 
      -> CoverTree' base childContainer nodeContainer tag dp
growct_unsafe ct d = if sepdist ct==0 || stIsLeaf ct
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

rmleaf :: 
    ( ValidCT base childContainer nodeContainer tag dp
    ) => CoverTree' base childContainer nodeContainer tag dp 
      -> ( CoverTree' base childContainer nodeContainer tag dp
         , CoverTree' base childContainer nodeContainer tag dp
         )
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

level2sepdist :: forall base num. (KnownFrac base, Floating num) =>  Proxy base -> Int -> num
level2sepdist _ l = (fromRational $ fracVal (Proxy :: Proxy base))**(fromIntegral l)

dist2level_down :: forall base num. (KnownFrac base, RealFrac num, Floating num) => Proxy base -> num -> Int
dist2level_down _ d = floor $ log d / log (fromRational $ fracVal (Proxy::Proxy base))

dist2level_up :: forall base num. (KnownFrac base, RealFrac num, Floating num) => Proxy base -> num -> Int
dist2level_up _ d = ceiling $ log d / log (fromRational $ fracVal (Proxy::Proxy base))

sepdist :: forall base childContainer nodeContainer tag dp. (KnownFrac base, Floating (Scalar dp)) => 
    CoverTree' base childContainer nodeContainer tag dp -> Scalar dp
sepdist ct = level2sepdist (Proxy::Proxy base) (level ct)

{-# INLINE coverDist #-}
coverDist :: forall base childContainer nodeContainer tag dp.
    ( Floating (Scalar dp)
    , KnownFrac base
    ) => CoverTree' base childContainer nodeContainer tag dp -> Scalar dp
coverDist node = sepdist node*coverfactor 
    where
        coverfactor = fromRational $ fracVal (Proxy :: Proxy base)

{-# INLINE sepdist_child #-}
sepdist_child :: forall base childContainer nodeContainer tag dp. (KnownFrac base, MetricSpace dp, Floating (Scalar dp)) => 
    CoverTree' base childContainer nodeContainer tag dp -> Scalar dp
sepdist_child ct = next -- rounddown (sing::Sing base) $ next --next/10
    where next = sepdist ct/(fromRational $ fracVal (Proxy :: Proxy base)) 

{-# INLINE roundup #-}
roundup :: forall base d. 
    ( Floating d
    , RealFrac d
    , KnownFrac base
    ) => Proxy (base::Frac) -> d -> d
roundup s d = rounddown s $ d * coverfactor
    where
        coverfactor = fromRational $ fracVal (Proxy :: Proxy base)

{-# INLINE rounddown #-}
rounddown :: forall base d. 
    ( Floating d
    , RealFrac d
    , KnownFrac base
    ) => Proxy (base::Frac) -> d -> d
rounddown _ d = coverfactor^^(floor $ log d / log coverfactor :: Int)
    where
        coverfactor = fromRational $ fracVal (Proxy :: Proxy base)

---------------------------------------

{-
recover ct = foldl' safeInsert ct' xs
    where
        (ct', xs) = recover' ct

recover' :: forall base childContainer nodeContainer tag dp.
    ( MetricSpace dp
    
    , KnownFrac base
    ) => CoverTree' base childContainer nodeContainer tag dp -> (CoverTree' base childContainer nodeContainer tag dp, [Weighted dp])
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

-- unsafeMap :: forall base childContainer nodeContainer tag dp1 dp2.
--     ( MetricSpace dp2
--     , Scalar dp1 ~ Scalar dp2
--     2
--     , KnownFrac base
--     ) => (dp1 -> dp2) -> AddUnit (CoverTree' base) tag dp1 -> AddUnit (CoverTree' base) tag dp2
unsafeMap f Unit = Unit
unsafeMap f (UnitLift ct) = UnitLift $ unsafeMap' f ct

unsafeMap' :: forall base childContainer nodeContainer tag dp1 dp2.
    ( MetricSpace dp2
    , Scalar dp1 ~ Scalar dp2
    2
    , KnownFrac base
    ) => (dp1 -> dp2) -> CoverTree' base childContainer nodeContainer tag dp1 -> CoverTree' base childContainer nodeContainer tag dp2
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

ctmap' :: forall base childContainer nodeContainer tag dp1 dp2.
    ( MetricSpace dp2
    , Scalar dp1 ~ Scalar dp2
    , Floating (Scalar dp1)
    2
    , Monoid tag
    , KnownFrac base
    ) => (dp1 -> dp2) -> CoverTree' base childContainer nodeContainer tag dp1 -> CoverTree' base childContainer nodeContainer tag dp2
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

extractLeaf :: forall base childContainer nodeContainer tag dp.
    ( MetricSpace dp 
    
    , KnownFrac base
    ) => CoverTree' base childContainer nodeContainer tag dp -> (Weighted dp, Maybe (CoverTree' base childContainer nodeContainer tag dp))
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

-- setLeafSize :: (MetricSpace dp, KnownFrac base) => Int -> CoverTree' base childContainer nodeContainer tag dp -> CoverTree' base childContainer nodeContainer tag dp
-- setLeafSize n ct = if stNumNodes ct < n
--     then ct { children = fmap singleton $ Strict.list2strictlist $ stToListW ct } 
--     else ct { children = fmap (setLeafSize n) $ children ct }
--     where
-- --         singleton :: Weighted dp -> CoverTree' base childContainer nodeContainer tag dp
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

instance 
    ( ValidCT base childContainer nodeContainer tag dp 
    ) => NumDP (CoverTree' base childContainer nodeContainer tag dp) where
    numdp = numdp

instance 
    ( ValidCT base childContainer nodeContainer tag dp
    ) => HomTrainer (AddUnit (CoverTree' base childContainer nodeContainer) tag dp) 
        where
    type Datapoint (AddUnit (CoverTree' base childContainer nodeContainer) tag dp) = dp

    {-# INLINE train1dp #-}
    train1dp dp = UnitLift $ Node 
        { nodedp    = dp
        , level     = minBound
--         , weight    = 1
        , numdp     = 1
        , children  = mempty
        , nodeV     = mempty
        , maxDescendentDistance = 0
--         , tag       = mempty
        }

    {-# INLINABLE train #-}
    train = UnitLift . insertBatch . F.toList

-------------------------------------------------------------------------------
-- tests

{-
instance 
    ( ValidCT base childContainer nodeContainer tag (Double,Double)
    ) => Arbitrary (AddUnit (CoverTree' base childContainer nodeContainer) tag (Double,Double)) 
        where
    arbitrary = do
        num :: Int <- choose (1,100)
--         xs <- replicateM num arbitrary
        xs <- replicateM num $ do
--             x <- arbitrary
--             y <- arbitrary
            x <- choose (-2**50,2**50)
            y <- choose (-2**50,2**50)
            return (x,y)
--         return $ unUnit $ train xs 
        return $ train xs 

-}

property_all :: 
    ( ValidCT base childContainer nodeContainer tag dp
    ) => AddUnit (CoverTree' base childContainer nodeContainer) tag dp -> Bool
property_all ct = and $ map (\x -> x ct)
    [ property_covering
    , property_leveled
    , property_separating
    , property_maxDescendentDistance
    ]

property_covering :: 
    ( ValidCT base childContainer nodeContainer tag dp
    ) => AddUnit (CoverTree' base childContainer nodeContainer) tag dp -> Bool
property_covering Unit = True
property_covering (UnitLift node) = if not $ stIsLeaf node
    then VG.maximum (fmap (distance (nodedp node) . nodedp) $ children node) < coverDist node 
      && VG.and (fmap (property_covering . UnitLift) $ children node)
    else True

property_leveled :: 
    ( ValidCT base childContainer nodeContainer tag dp
    ) => AddUnit (CoverTree' base childContainer nodeContainer) tag dp -> Bool
property_leveled (Unit) = True
property_leveled (UnitLift node)
    = VG.all (== VG.head xs) xs
   && VG.and (fmap (property_leveled . UnitLift) $ children node)
    where
        xs = fmap level $ children node

property_separating :: 
    ( ValidCT base childContainer nodeContainer tag dp
    ) => AddUnit (CoverTree' base childContainer nodeContainer) tag dp -> Bool
property_separating Unit = True
property_separating (UnitLift node) = if length (VG.toList $ children node) > 1 
    then VG.foldl1 min ((mapFactorial stMaxDistance) $ children node) > sepdist_child node
      && VG.and (fmap (property_separating . UnitLift) $ children node)
    else True
    where
        mapFactorial :: (VG.Vector v a, VG.Vector v b) =>(a -> a -> b) -> v a -> v b
        mapFactorial f = VG.fromList . mapFactorial' f . VG.toList
        mapFactorial' :: (a -> a -> b) -> [a] -> [b]
        mapFactorial' f xs = go xs []
            where
                go [] ys = ys
                go (x:xs) ys = go xs (map (f x) xs `mappend` ys)

property_maxDescendentDistance :: 
    ( ValidCT base childContainer nodeContainer tag dp
    ) => AddUnit (CoverTree' base childContainer nodeContainer) tag dp -> Bool
property_maxDescendentDistance Unit = True
property_maxDescendentDistance (UnitLift node) 
    = and (map (property_maxDescendentDistance . UnitLift) $ stChildrenList node)
   && and (map (\dp -> distance dp (nodedp node) <= maxDescendentDistance node) $ stDescendents node)

property_validmerge :: 
    ( ValidCT base childContainer nodeContainer tag dp
    ) => ( AddUnit (CoverTree' base childContainer nodeContainer) tag dp -> Bool )
      -> AddUnit (CoverTree' base childContainer nodeContainer) tag dp 
      -> AddUnit (CoverTree' base childContainer nodeContainer) tag dp 
      -> Bool
property_validmerge prop (UnitLift ct1) (UnitLift ct2) = prop . UnitLift $ ct1 <> ct2

-- property_lossless :: [(Double,Double)] ->  Bool
-- property_lossless [] = True
-- property_lossless xs = Set.fromList xs == dpSet ct
--     where
--         UnitLift ct = train xs :: AddUnit (CoverTree' (2/1) V.Vector V.Vector) () (Double,Double)
-- 
--         dpSet :: (Ord dp) => CoverTree' base V.Vector V.Vector tag dp -> Set.Set dp
--         dpSet = Set.fromList . dpList
--             where
--                 dpList :: CoverTree' base V.Vector V.Vector tag dp -> [dp]
--                 dpList node = nodedp node:(concat . fmap dpList . V.toList $ children node)

property_numdp :: 
    ( ValidCT base childContainer nodeContainer tag dp
    ) => AddUnit (CoverTree' base childContainer nodeContainer) tag dp -> Bool
property_numdp Unit = True
property_numdp (UnitLift node) = numdp node == sum (map fst $ stToListW node)


---------------------------------------

{-

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
my = train ys :: CoverTree (Double,Double)
-- my2 = train $ take 3 ys :: CoverTree (Double,Double)
-- my = prunect $ insertBatch ys

ys' :: [(Double,Double)]
ys' = [(1,2),(2,1)]
-- my' = train ys' :: CoverTree (Double,Double)
-- my' = prunect $ insertBatch ys'

zs :: [(Double,Double)]
-- zs = [(20,21),(22,23),(21,22),(30,20),(20,20),(19,20),(20,10),(22,21)]
zs = [(20,21),(22,23),(21,22),(30,20),(20,20),(20,10),(22,21)]
mz = train zs :: CoverTree (Double,Double)

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

-}

-------------------------------------------------------------------------------
-- diagrams

{-
drawT ct1 ct2 = (strutY 2.5 === draw (unUnit ct2))
--             ||| (text "<>" <> strutX 2.5)
            ||| (strutX 2.5)
            ||| (strutY 2.5 === draw (unUnit ct1))
--             ||| (text "=" <> strutX 2) 
            ||| (strutX 2.5)
            ||| (draw $ unUnit $ ct1 `mappend` ct2)

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
             connect (label++show depth) (label++show (depth+1)) . apList (fmap 
                (\key -> connect (label++show depth) (intShow key++show (depth+1))) 
                (map nodedp $ toList $ children tree))
            
justdouble :: Maybe Double -> String
justdouble Nothing = "0"
justdouble (Just x) = show x

apList :: [a -> a] -> a -> a
apList [] a = a
apList (x:xs) a = apList xs (x a)

-- apList :: List (a -> a) -> a -> a
-- apList Strict.Nil a = a
-- apList (x:.xs) a = apList xs (x a)

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

-}
