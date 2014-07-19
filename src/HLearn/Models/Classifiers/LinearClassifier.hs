{-# LANGUAGE DataKinds, RankNTypes #-}
module HLearn.Models.Classifiers.LinearClassifier
    where

import Data.Data
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Random
import Data.Dynamic
import Data.List.Extras
import Data.Maybe
import Data.Typeable
import qualified Data.ByteString.Lazy as BS
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Foreign.Storable
import System.IO

import Data.Csv

import Debug.Trace

import HLearn.Algebra
import HLearn.Algebra.History
import HLearn.Algebra.LinearAlgebra as LA
import HLearn.Evaluation.CrossValidation
import HLearn.Metrics.Lebesgue
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common
import HLearn.Optimization.GradientDescent 
import HLearn.Optimization.NewtonRaphson 
import HLearn.Optimization.QuasiNewton 
import HLearn.Optimization.Common 
import qualified HLearn.Optimization.LineMinimization as LineMin
import HLearn.Optimization.Trace 

-------------------------------------------------------------------------------
-- data types

data LinearClassifier dp = LinearClassifier
    { weights :: Map.Map (Label dp) (Scalar dp, Attributes dp, Taylor (Attributes dp))
    , datapoints :: V.Vector dp
    , reg :: Scalar dp
    }
    deriving (Typeable)

data Taylor dp 
    = Taylor dp (Matrix (Scalar dp))
    | NoTaylor
    deriving (Typeable)

instance 
    ( NFData (Scalar dp)
    , NFData (Label dp)
    , NFData (Attributes dp)
    ) => NFData (LinearClassifier dp)
    where
    rnf lr = seq (Map.map (\(n,w,t) -> deepseq w $ seq n$ seq t $ () ) $ weights lr) ()

instance 
    ( Show (Label dp)
    , Show (Scalar dp)
    , Show (Attributes dp)
    , Storable (Scalar dp)
    ) => Show (LinearClassifier dp)
        where
    show lr = show $ Map.map (\(n,w,_) -> (n,w)) $ weights lr

-------------------------------------------------------------------------------
-- training

mappendAverage ::
    ( VG.Vector v t
    , Attributes dp ~ v t
    , Fractional t
    , t ~ Scalar dp
    , Ord (Label dp)
    ) => LinearClassifier dp -> LinearClassifier dp -> LinearClassifier dp
mappendAverage lr1 lr2 = LinearClassifier 
    { weights = Map.unionWith go (weights lr1) (weights lr2)
    , datapoints = V.fromList $ V.toList (datapoints lr1) ++ V.toList (datapoints lr2)
    , reg = reg lr1
    }
    where
        go (n1,w1,_) (n2,w2,_) = (n1+n2,w,NoTaylor)
            where
                w = VG.zipWith (\a b -> (n1*a+n2*b)/(n1+n2)) w1 w2

mappendTaylor ::
    ( VG.Vector v t
    , Attributes dp ~ v t
    , Fractional t
    , t ~ Scalar dp
    , t ~ Scalar t
    , v ~ LA.Vector
    , LA.Field t
    , VectorSpace t
    , Typeable t
    , Show t
    , Ord t
    , Ord (Label dp)
    ) => LinearClassifier dp -> LinearClassifier dp -> LinearClassifier dp
mappendTaylor lr1 lr2 = LinearClassifier 
    { weights = Map.unionWith go (weights lr1) (weights lr2)
    , datapoints = V.fromList $ V.toList (datapoints lr1) ++ V.toList (datapoints lr2)
    , reg = reg lr1
    }
    where
        go (n1,w1,NoTaylor) (n2,w2,NoTaylor) = (n1+n2,w1,NoTaylor)
        go (_,_,NoTaylor) a = a
        go a (_,_,NoTaylor) = a
        go (n1,w1,Taylor v1 m1) (n2,w2,Taylor v2 m2) = (n1+n2,w,Taylor v' m')  
            where
                m' = ((n1.*m1) <> (n2.*m2))/.(n1+n2)
                v' = ((n1.*v1) <> (n2.*v2))/.(n1+n2)
                w = LA.inv m' `LA.matProduct` v'
-- reoptimize
--   :: (Labeled dp, Typeable (Label dp), Typeable (Scalar dp),
--       Typeable dp, Show (Label dp), Show (Scalar dp), Show dp,
--       Ord (Label dp), Ord (Scalar dp), Ord dp,
--       Attributes dp1 ~ Vector (Scalar dp),
--       Attributes dp ~ Vector (Scalar dp), Label dp1 ~ Label dp,
--       Tensor 1 (Scalar dp) ~ Scalar dp,
--       Scalar (Scalar dp) ~ Scalar dp) =>
--      (LinearClassifier dp
--       -> LinearClassifier dp -> LinearClassifier dp1)
--      -> LinearClassifier dp
--      -> LinearClassifier dp
--      -> LinearClassifier dp
-- reoptimize f lr1 lr2 = lrtrain2 (reg lr1) dps' $ Map.map go $ weights $ f lr1 lr2
--     where
--         dps' = V.fromList $ V.toList (datapoints lr1) ++ V.toList (datapoints lr2)
--         go (_,w,_) = w

traceLinearClassifier :: forall dp. Typeable dp => dp -> Event -> [String]
traceLinearClassifier _ opt = case fromDynamic (dyn opt) :: Maybe (LinearClassifier dp) of
    Nothing -> []
    Just x -> 
        [ (head $ words $ drop 2 $ init $ init $ show $ dyn opt)
--         ++"; fx1="++showDoubleLong (fx1 x)
--         ++"; |f'x1|="++showDouble (innerProductNorm $ f'x1 x)
--         ++"; step="++showDouble (stepSize x)
--         ++"; step="++showDouble (stepSize x)
--         ++"; sec="++showDouble ((fromIntegral $ stoptime opt)*1e-12)
        ++"; sec="++showDouble ((fromIntegral $ runtime opt)*1e-12)
        ]

data MonoidType
    = MappendAverage
    | MappendTaylor
    | MappendUpperBound
    | MappendUpperBoundCenter
    deriving (Read, Show, Eq, Data, Typeable)

trainLogisticRegression monoidtype lambda c2reg c2loss dps 
    = trainLogisticRegressionWarmStart monoidtype lambda c2reg c2loss dps $ zeroWeights dps

trainLogisticRegressionWarmStart :: forall dp vec r container.
    ( Ord dp
    , Ord (Label dp)
    , Labeled dp
    , Attributes dp ~ vec r
    , Tensor 1 (Attributes dp) ~ Attributes dp
    , VG.Vector vec r
    , Floating r
    , Monoid r
    , Typeable r
    , Typeable vec
    , r ~ Scalar dp
    , r ~ Tensor 0 dp
    , Ord r
    , r ~ Scalar (vec r)
    , InnerProduct (vec r)
    , VectorSpace r
    , vec ~ LA.Vector
    , LA.Field r
    , Show (Label dp) 
    , Show (vec r)
    , Show r
    , Show dp
    , Typeable dp
    , Typeable (Label dp)
    , Typeable (Attributes dp)
    , F.Foldable container
    , ValidTensor r
    , IsScalar r
    ) => MonoidType
      -> Scalar dp 
      -> C2Function (Attributes dp)          -- ^ regularization function
      -> (Label dp -> dp -> C2Function (Attributes dp))   -- ^ loss function
      -> container dp 
      -> Map.Map (Label dp) (Attributes dp) 
      -> History (LinearClassifier dp)
trainLogisticRegressionWarmStart monoidtype lambda c2reg c2loss dps weights0 = do
    weights' <- collectEvents $ fmap Map.fromList $ go $ Map.assocs weights0
    report $ LinearClassifier 
        { weights = weights'
        , datapoints = V.fromList $ F.toList dps
        , reg= lambda
        }
    where
        n :: Label dp -> Scalar dp
        n l = fromIntegral $ length $ filter (\dp -> getLabel dp ==l) $ F.toList dps

        -- the weights for the last label are set to zero;
        -- this is equivalent to running the optimization procedure,
        -- but much cheaper
        go ((l,w0):[]) = report [(l, (n l,VG.replicate (VG.length w0) 0, NoTaylor))]

        -- calculate the weights for label l
        go ((l,w0):xs) = do
            opt <- newtonRaphson f f' f'' w0
--             opt <- conjugateGradientDescent f f' w0
--             opt <- quasiNewton f f' w0
                [ maxIterations 200
                , fx1grows
                , multiplicativeTollerance 1e-6
                ]

            let w1 = opt^.x1
                fw1 = f w1
                f'w1 = f' w1
                f''w1 = f'' w1

            resTaylor <- report 
                $ trace ("w1="++show w1)
                $ deepseq w1 
                $ (l, (n l, w1, Taylor (w1 `mul` f''w1) f''w1))

            resUpper <- report 
                $ trace ("w1="++show w1)
                $ deepseq w1 
                $ (l, (n l, w1, Taylor (ub_b w1) (ub_a w1) ))

--             fmap ((if taylor then resTaylor else resUpper):) $ go xs
            fmap ((:) $ case monoidtype of
                    MappendTaylor -> resTaylor
                    MappendUpperBound -> resUpper
                ) $ go xs
            where
                reg   w = (c2reg w)^._1
                reg'  w = (c2reg w)^._2
                reg'' w = (c2reg w)^._3

                loss   dp w = (c2loss l dp w)^._1
                loss'  dp w = (c2loss l dp w)^._2
                loss'' dp w = (c2loss l dp w)^._3

                f   w = (numdp*lambda .* reg   w) <> (sumOver dps $ \dp -> loss   dp w)
                f'  w = (numdp*lambda .* reg'  w) <> (sumOver dps $ \dp -> loss'  dp w)
                f'' w = (numdp*lambda .* reg'' w) <> (sumOver dps $ \dp -> loss'' dp w)

                project :: Vector r -> Vector r -> Vector r
                project dp1 dp2 = 0.5 .* (inner dp1 dp2 / innerProductNorm dp2) .* dp2

                ub_a :: Vector r -> Matrix r
                ub_a w = inverse (sumOver dps $ \dp ->  
                            outerProduct (getAttributes dp) (getAttributes dp) *. (
                                (lambda .* reg   w) <>
                                innerProductNorm
                                    ( loss' dp (project w (getAttributes dp))
                                      <> inverse (loss' dp mempty)
                                    ) /.(2*innerProductNorm (project w (getAttributes dp)))
                                )
                            )
                ub_b w = (numdp*lambda .* reg'  w) <>
                       (sumOver dps $ \dp -> loss' dp mempty)

                numdp :: Scalar dp
                numdp = fromIntegral $ length $ F.toList dps

-------------------------------------------------------------------------------

type C2Function x = Tensor 1 x -> (Tensor 0 x, Tensor 1 x, Tensor 2 x)

-------------------

l1reg :: (Ord r, IsScalar r) => C2Function (LA.Vector r)
l1reg w = 
    ( VG.sum $ VG.map abs w
    , VG.map (\i -> if i>=0 then 1 else -1) w 
    , let z = VG.replicate (VG.length w) 0 
      in LA.outerProduct z z
    )

l2reg :: IsScalar r => C2Function (LA.Vector r)
l2reg w =
    ( VG.sum $ VG.map (**2) w
    , VG.map (*2) w 
    , 2 .* LA.eye (VG.length w) 
    )

elasticNet :: (Ord r, IsScalar r) => C2Function (LA.Vector r)
elasticNet w = 
    ( l1^._1 <> eta.*l2^._1
    , l1^._2 <> eta.*l2^._2
    , l1^._3 <> eta.*l2^._3
    )
    where
        eta = 0.85
        l1 = l1reg w
        l2 = l2reg w

-------------------

logloss :: 
    ( ValidTensor (Attributes dp)
    , Ord (Scalar dp)
    , Ord (Scalar (Attributes dp))
    , Labeled dp
    , Eq (Label dp)
    ) => Label dp -> dp -> C2Function (Attributes dp)
logloss label dp w =
    ( logSumOfExp2 0 $ -y dp * inner w x
    , (-y dp*(1-invlogit (y dp * inner w x))) .* x
    , LA.outerProduct x x
      /. (1+exp(-y dp*inner w x))
    )
    where
        x = mkTensor $ getAttributes dp
        y dp = bool2num $ getLabel dp==label

hingeloss ::
    ( Attributes dp ~ LA.Vector r
    , LA.Field r
    , IsScalar r
    , Ord r
    , VectorSpace r
    , Labeled dp
    , Eq (Label dp)
    , ValidTensor (Attributes dp)
    ) => Label dp -> dp -> C2Function (Attributes dp)
hingeloss label dp w =
    ( max 0 $ 1 - y dp * inner (getAttributes dp) w
    , if inner (getAttributes dp) w > 1
        then VG.map (const 0) w
        else (-y dp) .* getAttributes dp
    , outerProduct (VG.map (*0) w) (VG.map (*0) w)

    )
    where
        y dp = bool2num $ getLabel dp==label

-------------------------------------------------------------------------------

invlogit x = 1 / (1 + exp (-x))

-- | calculates log . sum . map exp in a numerically stable way 
logSumOfExp xs = m + log (sum [ exp $ x-m | x <- xs ] )
    where
        m = maximum xs

-- | calculates log $ exp x1 + exp x2 in a numerically stable way
logSumOfExp2 x1 x2 = m + log ( exp (x1-m) + exp (x2-m) )
    where
        m = max x1 x2

sumOver :: (F.Foldable container, Monoid r) => container x -> (x -> r) -> r
sumOver xs f = F.foldMap f xs


zeroWeights :: forall dp vec r container.
    ( Ord dp
    , Ord (Label dp)
    , Labeled dp
    , Attributes dp ~ vec r
    , VG.Vector vec r
    , Floating r
    , Monoid r
    , r ~ Scalar dp
    , Ord r
    , r ~ Scalar (vec r)
    , InnerProduct (vec r)
    , Show (Label dp) 
    , Show (vec r)
    , Show r
    , F.Foldable container
    ) => container dp -> Map.Map (Label dp) (Attributes dp)
zeroWeights dps = Map.fromList [ (label,VG.replicate dim mempty) | label <- labels ]
    where
        labels = map getLabel $ F.toList dps
        dim = VG.length $ getAttributes $ head $ F.toList dps

centroidtrain :: forall dp vec r container.
    ( Ord dp
    , Ord (Label dp)
    , Labeled dp
    , Attributes dp ~ vec r
    , VG.Vector vec r
    , Floating r
    , Monoid r
    , r ~ Scalar dp
    , Ord r
    , r ~ Scalar (vec r)
    , InnerProduct (vec r)
    , Show (Label dp) 
    , Show r
    , Show (vec r)
    , F.Foldable container
    ) => container dp -> LinearClassifier dp
centroidtrain dps = LinearClassifier 
    { weights = Map.fromList $ go $ Map.assocs $ zeroWeights dps
    , datapoints = V.fromList $ F.toList dps
    , reg =0
    }
    where
        n l = fromIntegral $ length $ filter (\dp -> getLabel dp == l) $ F.toList dps

        go [] = []
        go ((l,w0):xs) = (l,(n l, w', NoTaylor)):go xs
            where
                w' = (foldl1 (<>) $ map getAttributes $ filter (\dp -> getLabel dp==l) $ F.toList dps) /. n l

nbtrain :: forall dp vec r container.
    ( Ord dp
    , Ord (Label dp)
    , Labeled dp
    , Attributes dp ~ vec r
    , VG.Vector vec r
    , Floating r
    , Monoid r
    , r ~ Scalar dp
    , Ord r
    , r ~ Scalar (vec r)
    , InnerProduct (vec r)
    , Show (Label dp) 
    , Show r
    , Show (vec r)
    , F.Foldable container
    ) => container dp -> LinearClassifier dp
nbtrain dps = LinearClassifier 
    { weights = Map.fromList $ go $ Map.assocs $ zeroWeights dps
    , datapoints = V.fromList $ F.toList dps
    , reg =0
    }
    where
        n l = fromIntegral $ length $ filter (\dp -> getLabel dp ==l) $ F.toList dps

        go [] = []
        go ((l,w0):xs) = (l,(n l, w', NoTaylor)):go xs
            where
                w' = (VG.convert $ VG.map (\(n,t) -> mean n/(variance t+1e-6)) normV)
                   VG.// [(0,bias)]

                bias = -sumOver normV (\(n,t) -> (mean n*mean n)/(2*(variance t+1e-6)))

                normV = VG.zip (fromJust $ Map.lookup l gaussianMap) gaussianTot

        gaussianTot = foldl1 (VG.zipWith (<>)) $ Map.elems gaussianMap

        gaussianMap = Map.fromListWith (VG.zipWith (<>)) 
            [ ( getLabel dp
              , V.map (train1dp :: r -> Normal r r) $ VG.convert $ getAttributes dp
              ) 
            | dp <- F.toList dps
            ]

-------------------------------------------------------------------------------
-- classification

instance Monoid (LinearClassifier dp) where
    mappend = undefined
    mempty = undefined

type instance Scalar (LinearClassifier dp) = Scalar dp

instance HomTrainer (LinearClassifier dp) where
    type Datapoint (LinearClassifier dp) = dp
    train = undefined

instance
    ( Labeled dp
    , Scalar (Attributes dp) ~ Scalar dp
    , InnerProduct (Attributes dp)
    , Floating (Scalar dp)
    , Ord (Scalar dp)
    , Attributes dp ~ vec (Scalar dp)
    , VG.Vector vec (Scalar dp)
    , Show (Scalar dp)
    , Show (Label dp)
    ) => Classifier (LinearClassifier dp)
        where
    classify m attr = fst $ argmax (\(l,s) -> s) $ Map.assocs $ Map.map (\(_,w,_) -> inner w attr) $ weights m

-------------------------------------------------------------------------------
-- test

