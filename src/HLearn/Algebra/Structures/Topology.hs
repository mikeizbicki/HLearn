module HLearn.Algebra.Structures.Topology
    where

import GHC.Prim
import Prelude hiding (elem)
import qualified Prelude as Prelude

import HLearn.Algebra.Structures.Modules
import HLearn.Algebra.Structures.MetricSpace

-------------------------------------------------------------------------------
-- classes

class HasElems t where
    type ElemConstraint t a :: Constraint
    type ElemConstraint t a = ()

    elem :: ElemConstraint t a => a -> t a -> Bool

-- | Topology must obey the laws:
-- (x `union` y) `intersect` y = y
-- (x `intersect` y) `union` y = y
class {-HasElems t =>-} Topology t where
    type TopologyConstraint t a :: Constraint
    type TopologyConstraint t a = ElemConstraint t a

    union        :: TopologyConstraint t a => t a -> t a -> t a
    intersection :: TopologyConstraint t a => t a -> t a -> t a


-------------------------------------------------------------------------------
-- instances

---------------------------------------
-- discrete topology

instance HasElems [] where
    type ElemConstraint [] a = Eq a

    elem = Prelude.elem 

instance Topology [] where
    union = (++)
    intersection _ [] = []
    intersection [] _ = []
    intersection (x:xs) ys = if x `elem` ys
        then x:(intersection xs ys)
        else intersection xs ys

---------------------------------------
-- free topology on a basis

newtype FreeTopology basis a = FreeTopology [basis a]

instance HasElems basis => HasElems (FreeTopology basis) where
    type ElemConstraint (FreeTopology basis) a = ElemConstraint basis a

    elem a (FreeTopology xs) = or $ map (elem a) xs

instance Topology (FreeTopology OpenInterval) where
    type TopologyConstraint (FreeTopology OpenInterval) a = (Eq a, ElemConstraint OpenInterval a)

    union (FreeTopology xs) (FreeTopology ys) = FreeTopology $ xs `union` ys
    intersection (FreeTopology xs) (FreeTopology ys) = FreeTopology $ xs `intersection` ys

---------------------------------------
-- bases

newtype OpenInterval a = OpenInterval (a,a)
    deriving (Read,Show,Eq)

instance HasElems OpenInterval where
    type ElemConstraint OpenInterval a = Ord a

    elem a (OpenInterval (x,y)) = a>x && a<y

-------------------

data MetricBall dp = MetricBall 
    { center :: !dp
    , radius :: !(Ring dp)
    }

instance HasElems MetricBall where
    type ElemConstraint MetricBall dp = MetricSpace dp

    elem dp ball = not $ isFartherThan dp (center ball) (radius ball)
