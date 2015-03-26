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
import qualified Data.Vector.Storable as VS
import Foreign.Storable
import System.IO

import Data.Csv hiding (Field)

import Debug.Trace

import qualified Prelude as P
import Prelude (take,drop,map,filter,zip,head)
import SubHask hiding (Functor(..), Applicative(..), Monad(..), Then(..), fail, return, argmax)
import SubHask.Compatibility.HMatrix

-- import HLearn.Algebra
import HLearn.History
-- import HLearn.Evaluation.CrossValidation
-- import HLearn.Metrics.Lebesgue
-- import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common
import HLearn.Models.Distributions.Common
import HLearn.Optimization.Common
import HLearn.Optimization.GradientDescent
-- import HLearn.Optimization.NewtonRaphson
-- import HLearn.Optimization.QuasiNewton
-- import HLearn.Optimization.StochasticGradientDescent
import HLearn.Optimization.LineMinimization.Univariate
import HLearn.Optimization.LineMinimization.Multivariate

bool2num :: Ring r => Bool -> r
bool2num True = 1
bool2num False = -1

-------------------------------------------------------------------------------
-- data types

data LabelDecision dp = LabelDecision
    { ldWeight :: !(Scalar dp)
    , ldVector :: !(Attributes dp)
    , ldTaylor :: (Taylor (Attributes dp))
    }
    deriving (Typeable)

data LinearClassifier dp = LinearClassifier
    { weights :: Map.Map (Label dp) (LabelDecision dp)
    , datapoints :: V.Vector dp
    , reg :: Scalar dp
    }
    deriving (Typeable)

data Taylor dp
    = Taylor dp (Matrix (Scalar dp))
    | NoTaylor
    deriving (Typeable)

dptaylor (Taylor dp _) = dp
mattaylor (Taylor _ mat) = mat

type instance Scalar (Taylor dp) = Scalar dp

instance
    ( NFData (Scalar dp)
    , NFData (Label dp)
    , NFData (Attributes dp)
    ) => NFData (LabelDecision dp)
    where
    rnf ld = deepseq (ldWeight ld)
           $ deepseq (ldVector ld)
           $ seq (ldTaylor ld)
           $ ()

instance
    ( NFData (Scalar dp)
    , NFData (Label dp)
    , NFData (Attributes dp)
    ) => NFData (LinearClassifier dp)
    where
    rnf lr = seq (Map.map rnf $ weights lr) ()

-- instance
--     ( Show (Label dp)
--     , Show (Scalar dp)
--     , Show (Attributes dp)
--     , Storable (Scalar dp)
--     ) => Show (LinearClassifier dp)
--         where
--     show lr = show $ Map.map (\ld -> (n,w)) $ weights lr

-------------------------------------------------------------------------------
-- training

-- mappendAverage ::
--     ( VG.Vector v t
--     , Attributes dp ~ v t
--     , Fractional t
--     , t ~ Scalar dp
--     , Ord (Label dp)
--     ) => LinearClassifier dp -> LinearClassifier dp -> LinearClassifier dp
mappendAverage lr1 lr2 = LinearClassifier
    { weights = Map.unionWith go (weights lr1) (weights lr2)
    , datapoints = V.fromList $ V.toList (datapoints lr1) ++ V.toList (datapoints lr2)
    , reg = reg lr1
    }
    where
        go (LabelDecision n1 w1 _) (LabelDecision n2 w2 _) = LabelDecision (n1+n2) w NoTaylor
            where
                w = VG.zipWith (\a b -> (n1*a+n2*b)/(n1+n2)) w1 w2

-- mappendQuadratic ::
--     ( VG.Vector v t
--     , Attributes dp ~ v t
--     , Fractional t
--     , t ~ Scalar dp
--     , t ~ Scalar t
--     , v ~ LA.Vector
--     , LA.Field t
--     , VectorSpace t
--     , Typeable t
--     , Show t
--     , Ord t
--     , Ord (Label dp)
--     ) => LinearClassifier dp -> LinearClassifier dp -> LinearClassifier dp
mappendQuadratic lr1 lr2 = LinearClassifier
    { weights = Map.unionWith go (weights lr1) (weights lr2)
    , datapoints = V.fromList $ V.toList (datapoints lr1) ++ V.toList (datapoints lr2)
    , reg = reg lr1
    }
    where
        go (LabelDecision n1 w1 NoTaylor) (LabelDecision n2 w2 NoTaylor) = LabelDecision (n1+n2) w1 NoTaylor
        go (LabelDecision _ _ NoTaylor) a = a
        go a (LabelDecision _ _ NoTaylor) = a
        go (LabelDecision n1 w1 (Taylor v1 m1)) (LabelDecision  n2 w2 (Taylor v2 m2))
            = LabelDecision (n1+n2) w (Taylor v' m')
            where
                m' = ((n1*.m1) + (n2*.m2))./(n1+n2)
                v' = ((n1*.v1) + (n2*.v2))./(n1+n2)
                w = reciprocal m' * v'
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

-- traceLinearClassifier :: forall dp. Typeable dp => dp -> Report -> [String]
-- traceLinearClassifier _ opt = case fromDynamic (dyn opt) :: Maybe (LinearClassifier dp) of
--     Nothing -> []
--     Just x ->
--         [ (head $ words $ drop 2 $ init $ init $ show $ dyn opt)
-- --         ++"; fx1="++showDoubleLong (fx1 x)
-- --         ++"; |f'x1|="++showDouble (innerProductNorm $ f'x1 x)
-- --         ++"; step="++showDouble (stepSize x)
-- --         ++"; step="++showDouble (stepSize x)
-- --         ++"; sec="++showDouble ((fromIntegral $ stoptime opt)*1e-12)
--         ++"; sec="++showDouble ((fromIntegral $ cpuTimeDiff opt)*1e-12)
--         ]

data MonoidType
    = MappendAverage
    | MappendTaylor
    | MixtureUpperTaylor Rational
    | MixtureAveTaylor Rational
    | MixtureAveUpper Rational
    | MappendUpperBound
    | MappendUpperBoundCenter
    deriving (Read, Show, Data, Typeable)

monoidMixRate :: MonoidType -> Rational
monoidMixRate (MixtureUpperTaylor i)    = i
monoidMixRate (MixtureAveTaylor i)  = i
monoidMixRate (MixtureAveUpper i)      = i
monoidMixRate _                     = 0

trainLinearClassifier :: forall dp vec r container m.
    ( Labeled dp
    , Ord (Label dp)
    , Attributes dp ~ vec r
    , Scalar dp ~ r
    , Normed r
    , Ord r
    , Module r
    , HasScalar dp
    , OuterProductSpace (vec r)
    , Hilbert (vec r)
    , VectorSpace (Outer (vec r))
    , Floating r
    , F.Foldable container
    , VG.Vector vec r
    , P.Ord (Label dp)
    , Eq dp

    , HistoryMonad m
    , Reportable m (LinearClassifier dp)
    , Reportable m (LabelDecision dp)
    , Reportable m (vec r)
    , Reportable m r
    , Reportable m dp
    , Reportable m String
    , Reportable m (Label dp, LabelDecision dp)

    , Show (vec r)
    , Show (Label dp)
--     , Outer (vec r) ~ Matrix r
    , NFData (vec r)
    , Typeable (Label dp)
    , Typeable container
    , VG.Vector container (vec r)
    , VG.Vector container dp
--     , container ~ []
    ) => MonoidType
      -> Scalar dp
      -> C2Function (Attributes dp)          -- ^ regularization function
      -> (Label dp -> dp -> C2Function (Attributes dp))   -- ^ loss function
      -> OptimizationMethod m dp
      -> container dp
      -> m (LinearClassifier dp)
trainLinearClassifier monoidtype lambda c2reg c2loss optMethod dps
    = warmStartLinearClassifier monoidtype lambda c2reg c2loss optMethod dps $ zeroWeights dps

warmStartLinearClassifier :: forall dp vec r container m.
    ( Labeled dp
    , Ord (Label dp)
    , Attributes dp ~ vec r
    , Scalar dp ~ r
    , Normed r
    , Ord r
    , Module r
    , HasScalar dp
    , OuterProductSpace (vec r)
    , Hilbert (vec r)
    , VectorSpace (Outer (vec r))
    , Floating r
    , F.Foldable container
    , VG.Vector vec r
    , P.Ord (Label dp)
    , Eq dp

    , HistoryMonad m
    , Reportable m (LinearClassifier dp)
    , Reportable m (LabelDecision dp)
    , Reportable m (vec r)
    , Reportable m r
    , Reportable m dp
    , Reportable m String
    , Reportable m (Label dp, LabelDecision dp)

    , Show (vec r)
    , Show (Label dp)
--     , Outer (vec r) ~ Matrix r
    , NFData (vec r)
    , Typeable (Label dp)
    , Typeable container
    , VG.Vector container (vec r)
    , VG.Vector container dp
--     , container ~ []
    ) => MonoidType
      -> Scalar dp
      -> C2Function (Attributes dp)          -- ^ regularization function
      -> (Label dp -> dp -> C2Function (Attributes dp))   -- ^ loss function
      -> OptimizationMethod m dp
      -> container dp
      -> Map.Map (Label dp) (Attributes dp)
      -> m (LinearClassifier dp)
warmStartLinearClassifier monoidtype lambda c2reg c2loss optMethod dps weights0 = trace ("weights0="++show weights0) $ do
    weights' <- collectReports $ fmap Map.fromList $ go $ Map.assocs weights0
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
        go ((l,w0):[]) = do
            x <- report $ LabelDecision (n l) (VG.replicate (VG.length w0) 0) NoTaylor
            return [(l,x)]

        -- calculate the weights for label l
        go ((l,w0):xs) = do
            w1 <- optMethod dps (\dp -> lambda*.c2reg+c2loss l dp) w0

            report "next";

            let ret = (l, LabelDecision (n l) w1 undefined)
            deepseq w1 $ report ret
            fmap (ret:) $ go xs

-------------------------------------------------------------------------------

type C2Function v = v -> (Scalar v, v, Outer v)

type C2Loss dp = dp -> C2Function (Attributes dp)

type OptimizationMethod m dp =
    ( Reportable m (Attributes dp)
    , Reportable m (Scalar (Attributes dp))
    , Reportable m dp
    ) => forall container.
      ( F.Foldable container
      , Typeable container
      ) => container dp -> C2Loss dp -> Attributes dp -> m (Attributes dp)

cgd ::
    ( Hilbert (Attributes dp)
    , BoundedField (Scalar (Attributes dp))
    , Normed (Scalar (Attributes dp))
    , Floating (Scalar (Attributes dp))
    , Ord (Scalar (Attributes dp))
    , HistoryMonad m
    , Reportable m (LineBracket (Scalar (Attributes dp)))
    , Reportable m (Brent (Scalar (Attributes dp)))
    , Reportable m (ConjugateGradientDescent (Attributes dp))

    , Show (Attributes dp)
    , Show (Scalar (Attributes dp))
    ) => OptimizationMethod m dp
cgd dps c2f w0 = do
    ret <- conjugateGradientDescent
        (\w -> sumOver dps $ \dp -> (c2f dp w) ^._1)
        (\w -> sumOver dps $ \dp -> (c2f dp w) ^._2)
        w0
    return $ ret^.x1

-- sgd ::
--     ( VectorSpace (Attributes dp)
--     , LearningRate hyperparams params (Attributes dp)
--     , Normed (Scalar (Attributes dp))
--     , Floating (Scalar (Attributes dp))
--     , Ord (Scalar (Attributes dp))
--     , Labeled dp
--     , Typeable params
--     ) => [StopCondition (SGD V.Vector params dp (Attributes dp))]
--       -> StochasticMethod V.Vector dp (Attributes dp)
--       -> hyperparams (Attributes dp)
--       -> OptimizationMethod dp
-- sgd stop sm lr dps c2f w0 = do
--     ret <- stochasticGradientDescent
--         sm
--         lr
--         ( V.fromList $ F.toList dps )
--         (\dp w -> c2f dp w ^. _2)
--         w0
--         stop
--     return $ ret^.x1

-------------------

l1reg :: forall vec r.
    ( VG.Vector vec r
    , r ~ Scalar (vec r)
    , Field r
    , Normed r
    , Ord r
    , IsScalar r
    , OuterProductSpace (vec r)
    ) => C2Function (vec r)
l1reg w =
    ( VG.foldl1' (+) $ VG.map abs w
    , VG.map (\i -> if i>=0 then 1 else -1) w
    , let z = VG.replicate (VG.length w) 0 :: vec r in z >< z
    )

-- l2reg :: (Storable r, IsScalar r) => C2Function (Vector r)
l2reg w =
    ( VG.sum $ VG.map (**2) w
    , VG.map (*2) w
    , 2
    )

-- elasticNet :: (Storable r, Ord r, IsScalar r) => C2Function (Vector r)
elasticNet w =
    ( l1^._1 + eta*.l2^._1
    , l1^._2 + eta*.l2^._2
    , l1^._3 + eta*.l2^._3
    )
    where
        eta = 0.85
        l1 = l1reg w
        l2 = l2reg w

-------------------

-- logloss ::
--     ( Hilbert (Attributes dp)
--     , Ord (Scalar dp)
--     , Ord (Scalar (Attributes dp))
--     , Labeled dp
--     , Eq (Label dp)
--     ) => Label dp -> dp -> C2Function (Attributes dp)
logloss label dp w =
    ( logSumOfExp2 zero $ -y dp * w <> x
    , (-y dp*(1-invlogit (y dp * w <> x))) *. x
    , x >< x ./ (1+exp(-y dp* w<>x))
    )
    where
        x = getAttributes dp
        y dp = bool2num $ getLabel dp==label

-- hingeloss ::
--     ( Attributes dp ~ LA.Vector r
--     , LA.Field r
--     , IsScalar r
--     , Ord r
--     , VectorSpace r
--     , Labeled dp
--     , Eq (Label dp)
--     , ValidTensor (Attributes dp)
--     ) => Label dp -> dp -> C2Function (Attributes dp)
hingeloss label dp w =
    ( max 0 $ 1 - y dp * (getAttributes dp) <> w
    , if (getAttributes dp) <> w > 1
        then VG.map (const 0) w
        else (-y dp) *. getAttributes dp
    , (VG.map (*0) w) >< (VG.map (*0) w)

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

-- sumOver :: (F.Foldable container, Semigroup r) => container x -> (x -> r) -> r
sumOver :: (F.Foldable container, Monoid r) => container x -> (x -> r) -> r
sumOver xs f = F.foldl' (\a b -> a + f b) zero xs


-- zeroWeights :: forall dp vec r container.
--     ( Ord dp
--     , Ord (Label dp)
--     , Labeled dp
--     , Attributes dp ~ vec r
--     , VG.Vector vec r
--     , Floating r
--     , Monoid r
--     , r ~ Scalar dp
--     , Ord r
--     , r ~ Scalar (vec r)
--     , InnerProduct (vec r)
--     , Show (Label dp)
--     , Show (vec r)
--     , Show r
--     , F.Foldable container
--     ) => container dp -> Map.Map (Label dp) (Attributes dp)
zeroWeights dps = Map.fromList [ (label,VG.replicate dim zero) | label <- labels ]
    where
        labels = map getLabel $ F.toList dps
        dim = VG.length $ getAttributes $ head $ F.toList dps

-- centroidtrain :: forall dp vec r container.
--     ( Ord dp
--     , Ord (Label dp)
--     , Labeled dp
--     , Attributes dp ~ vec r
--     , VG.Vector vec r
--     , Floating r
--     , Monoid r
--     , r ~ Scalar dp
--     , Ord r
--     , r ~ Scalar (vec r)
--     , InnerProduct (vec r)
--     , Show (Label dp)
--     , Show r
--     , Show (vec r)
--     , F.Foldable container
--     ) => container dp -> LinearClassifier dp
-- centroidtrain dps = LinearClassifier
--     { weights = Map.fromList $ go $ Map.assocs $ zeroWeights dps
--     , datapoints = V.fromList $ F.toList dps
--     , reg =0
--     }
--     where
--         n l = fromIntegral $ length $ filter (\dp -> getLabel dp == l) $ F.toList dps
--
--         go [] = []
--         go ((l,w0):xs) = (l,LabelDecision (n l)  w' NoTaylor):go xs
--             where
--                 w' = (foldl1 (+) $ map getAttributes $ filter (\dp -> getLabel dp==l) $ F.toList dps) /. n l

-- nbtrain :: forall dp vec r container.
--     ( Ord dp
--     , Ord (Label dp)
--     , Labeled dp
--     , Attributes dp ~ vec r
--     , VG.Vector vec r
--     , Floating r
--     , Monoid r
--     , r ~ Scalar dp
--     , Ord r
--     , r ~ Scalar (vec r)
--     , InnerProduct (vec r)
--     , Show (Label dp)
--     , Show r
--     , Show (vec r)
--     , F.Foldable container
--     ) => container dp -> LinearClassifier dp
-- nbtrain dps = LinearClassifier
--     { weights = Map.fromList $ go $ Map.assocs $ zeroWeights dps
--     , datapoints = V.fromList $ F.toList dps
--     , reg =0
--     }
--     where
--         n l = fromIntegral $ length $ filter (\dp -> getLabel dp ==l) $ F.toList dps
--
--         go [] = []
--         go ((l,w0):xs) = (l, LabelDecision (n l) w' NoTaylor):go xs
--             where
--                 w' = (VG.convert $ VG.map (\(n,t) -> mean n/(variance t+1e-6)) normV)
--                    VG/./ [(0,bias)]
--
--                 bias = -sumOver normV (\(n,t) -> (mean n*mean n)/(2*(variance t+1e-6)))
--
--                 normV = VG.zip (fromJust $ Map.lookup l gaussianMap) gaussianTot
--
--         gaussianTot = foldl1 (VG.zipWith (+)) $ Map.elems gaussianMap
--
--         gaussianMap = Map.fromListWith (VG.zipWith (+))
--             [ ( getLabel dp
--               , V.map (train1dp :: r -> Normal r r) $ VG.convert $ getAttributes dp
--               )
--             | dp <- F.toList dps
--             ]

-------------------------------------------------------------------------------
-- classification

-- instance Monoid (LinearClassifier dp) where
--     mappend = undefined
--     zero = undefined

type instance Scalar (LinearClassifier dp) = Scalar dp

-- instance HomTrainer (LinearClassifier dp) where
--     type Datapoint (LinearClassifier dp) = dp
--     train = undefined

instance
--     ( Labeled dp
--     , Scalar (Attributes dp) ~ Scalar dp
--     , Hilbert (Attributes dp)
--     , Floating (Scalar dp)
--     , Ord (Scalar dp)
--     , Attributes dp ~ vec (Scalar dp)
--     , VG.Vector vec (Scalar dp)
--     , Show (Scalar dp)
--     , Show (Label dp)
    ( Labeled dp
    , P.Ord (Scalar (Datapoint (LinearClassifier dp)))
    , Hilbert (Attributes (Datapoint (LinearClassifier dp)))
    ) => Classifier (LinearClassifier dp)
        where
    classify m attr
        = (fst::(a,b) -> a)
        $ argmax (\(l,s) -> s)
        $ Map.assocs
        $ Map.map (\ld -> ldVector ld <> attr)
        $ weights m

type instance Datapoint (LinearClassifier dp) = dp

-------------------------------------------------------------------------------
-- test

