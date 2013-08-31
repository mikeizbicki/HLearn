module HLearn.NPHard.KCenters
    where

import Data.List.Extras
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import GHC.TypeLits

import Debug.Trace

import HLearn.Algebra

-------------------------------------------------------------------------------
-- data types    

data KCenters dp (k::Nat) = KCenters
    { clusters :: Map.Map dp (Cluster dp)
    }
    deriving (Read,Show,Eq,Ord)
    
type Cluster dp = Seq.Seq dp
    
-------------------------------------------------------------------------------
-- algebra

instance (SingI k, Ord dp, MetricSpace dp r) => Semigroup (KCenters dp k) where
    kc1 <> kc2 = KCenters
        { clusters = merge $ Map.unionWith (<>) (clusters kc1) (clusters kc2)
        }
        where
            k = fromIntegral $ fromSing (sing :: Sing k)
            
            merge clusters = if Map.size clusters <= k
                then clusters
                else let (dp1,dp2) = closest $ Map.keys clusters
                         (Just dp2data) = Map.lookup dp2 clusters
                         inserted = Map.insertWith (<>) dp1 dp2data clusters
                     in Map.delete dp2 inserted
      
kc1 <<>> kc2 = KCenters
    { clusters = merge $ Map.unionWith (<>) (clusters kc1) (clusters kc2)
    }
    where
        k = 3
        
        merge clusters = if Map.size clusters <= k
            then clusters
            else let (dp1,dp2) = closest $ Map.keys clusters
                     res = trace ("dp2: "++show dp2++"  clusters: "++show clusters) $ Map.lookup dp2 clusters
                     (Just dp2data) = trace ("res: "++show res) res
                     inserted = Map.insertWith (<>) dp1 dp2data clusters
                 in Map.delete dp2 inserted
                    
x=train [1,2,3,100,101,102,1000,1001,1002::Double] :: KCenters Double 3
y=train [10,11,12,100,101,102,1000,1001,1002::Double] :: KCenters Double 3


closest :: (MetricSpace dp r) => [dp] -> (dp,dp)
closest dpL = argmin (uncurry dist) [(dp1,dp2) | dp1<- dpL, dp2 <- dpL, dp1/=dp2]

instance (SingI k, Ord dp, MetricSpace dp r) => Monoid (KCenters dp k) where
    mappend = (<>)
    mempty = KCenters
        { clusters = mempty
        }
        
-------------------------------------------------------------------------------
-- model

instance Model (NoParams (KCenters dp k)) (KCenters dp k) where
    getparams _ = NoParams
    
instance DefaultModel (NoParams (KCenters dp k)) (KCenters dp k) where
    defparams = NoParams
    
instance (SingI k, Ord dp, MetricSpace dp r) => HomTrainer (NoParams (KCenters dp k)) dp (KCenters dp k) where
    train' _ dps = KCenters $ Map.fromList $ map (\x -> (x,mempty)) (go k [])
        where
            k = fromIntegral $ fromSing (sing :: Sing k)
            
            go :: Int -> [dp] -> [dp]
            go 0 centerL = centerL
            go i [] = go (i-1) $ [head $ F.toList dps]
            go i centerL = go (i-1) $ (F.foldr1 gonext dps):centerL
                where 
                    gonext dp1 dp2 = 
                        if clusterdistance centerL dp1<clusterdistance centerL dp2
                            then dp2
                            else dp1    
    
clusterdistance :: (F.Foldable container, MetricSpace dp r) => container dp -> dp -> r
clusterdistance centerL dp = dist dp $ F.foldr1 go centerL
    where
        go dp1 dp2 = 
            if dist dp dp1<dist dp dp2
                then dp1
                else dp2
        
