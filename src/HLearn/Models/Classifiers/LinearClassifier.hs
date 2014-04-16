{-# LANGUAGE DataKinds #-}
module HLearn.Models.Classifiers.LinearClassifier
    where

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

instance 
    ( Ord (Label dp)
    , Ord (Scalar (Attributes dp))
    , LA.Field (Scalar (Attributes dp))
    , VectorSpace (Scalar (Attributes dp))
    , Monoid (Attributes dp)
    , Num (Scalar dp)
    , Attributes dp ~ LA.Vector (Scalar (Attributes dp))
    , Typeable (Scalar (Attributes dp))
    , Show (Scalar (Attributes dp))
    ) => Monoid (LinearClassifier dp) where
    mempty = undefined
    mappend = undefined

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

reoptimize f lr1 lr2 = lrtrain2 (reg lr1) dps' $ Map.map go $ weights $ f lr1 lr2
    where
        dps' = V.fromList $ V.toList (datapoints lr1) ++ V.toList (datapoints lr2)
        go (_,w,_) = w

instance 
    ( Ord dp
    , Ord (Label dp)
    , Labeled dp
    , Attributes dp ~ vec r
    , VG.Vector vec r
    , Floating r
    , Monoid r
    , Typeable r
    , Typeable vec
    , r ~ Scalar dp
     , Ord r
    , r ~ Scalar (vec r)
    , IsScalar r
    , InnerProduct (vec r)
    , vec ~ LA.Vector
    , LA.Field r
    , Show (Label dp) 
    , Show (vec r)
    , Show r
    , Show dp
    , Typeable dp
    , Typeable (Label dp)
    , Typeable (Attributes dp)
    ) => HomTrainer (LinearClassifier dp) 
        where
    type Datapoint (LinearClassifier dp) = dp

    train dps = lrtrain2 0 dps $ zeroWeights dps
--     train = nbtrain 

lrtrain lambda dps = lrtrain2 lambda dps $ zeroWeights dps
lrtrainMsimple lambda dps = lrtrainM lambda dps $ zeroWeights dps

lrtrain2 :: forall dp vec r container.
    ( Ord dp
    , Ord (Label dp)
    , Labeled dp
    , Attributes dp ~ vec r
    , VG.Vector vec r
    , Floating r
    , Monoid r
    , Typeable r
    , Typeable vec
    , r ~ Scalar dp
    , Ord r
    , r ~ Scalar (vec r)
    , InnerProduct (vec r)
    , IsScalar r
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
    ) => Scalar dp -> container dp -> Map.Map (Label dp) (Attributes dp) -> LinearClassifier dp
lrtrain2 lambda dps weights0 = traceHistory 
    [ traceBFGS
    , traceNewtonRaphson
    , traceLinearClassifier (undefined::dp)
    ]
    $ collectEvents $ lrtrainM lambda dps weights0

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

lrtrainM :: forall dp vec r container.
    ( Ord dp
    , Ord (Label dp)
    , Labeled dp
    , Attributes dp ~ vec r
    , VG.Vector vec r
    , Floating r
    , Monoid r
    , Typeable r
    , Typeable vec
    , r ~ Scalar dp
    , Ord r
    , r ~ Scalar (vec r)
    , r ~ Tensor 1 r
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
    ) => Scalar dp 
--       -> C2Function dp                       -- ^ regularization function
--       -> (Label dp -> dp -> C2Function dp)   -- ^ loss function
      -> container dp 
      -> Map.Map (Label dp) (Attributes dp) 
      -> History (LinearClassifier dp)
lrtrainM lambda dps weights0 = do
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
--             opt <- newtonRaphson f f' f'' w0
            opt <- conjugateGradientDescent f f' w0
--             opt <- quasiNewton f f' w0
                [ maxIterations 200
                , fx1grows
--                 , multiplicativeTollerance 1e-6
                ]

            let w1 = opt^.x1
                fw1 = f w1
                f'w1 = f' w1
                f''w1 = f'' w1

            res <- report 
                $ trace ("w1="++show w1)
                $ deepseq w1 
                $ (l, (n l, w1, Taylor (w1 `mul` f''w1) f''w1))

            fmap (res:) $ go xs
            where
                reg   w = (elasticNet w)^._1
                reg'  w = (elasticNet w)^._2
                reg'' w = (elasticNet w)^._3

                loss   dp w = (logloss l dp w)^._1
                loss'  dp w = (logloss l dp w)^._2
                loss'' dp w = (logloss l dp w)^._3

                f   w = (numdp*lambda .* reg   w) <> (sumOver dps $ \dp -> loss   dp w)
                f'  w = (numdp*lambda .* reg'  w) <> (sumOver dps $ \dp -> loss'  dp w)
                f'' w = (numdp*lambda .* reg'' w) <> (sumOver dps $ \dp -> loss'' dp w)

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

monoidtest dps n = do
    dps' <- shuffle dps
    xs' <- kfold 10 dps'
    return $ foldl1 (zipWith (<>)) $ map (map (train1dp :: Double -> Normal Double Double)) $ do
        testset <- xs'
        let trainingset = concat $ filter (/=testset) xs'
        return $ runtest (partition n trainingset) testset

runtest ::
    ( Scalar dp ~ Double
    , Labeled dp 
    , LA.Field (Scalar dp)
    , VectorSpace (Scalar dp)
    , Ord (Scalar dp)
    , Ord (Label dp)
    , Attributes dp ~ LA.Vector Double
    , Show (Label dp)
    , Show dp
    , Typeable dp
    , Typeable (Label dp)
    , Typeable (Attributes dp)
    , Ord dp
    ) => [[dp]] -> [dp] -> [Scalar dp]
runtest dpsL testset = 
    [ errorRate zero testset
    , errorRate centroid testset
    , errorRate nb testset
    , 0/1/0
    , errorRate lr1 testset
    , errorRate lr2 testset
    , errorRate lr3 testset
    , errorRate lr4 testset
    , errorRate lr5 testset
    , errorRate lr6 testset
    , errorRate lr7 testset
    , 0/1/0
    , errorRate lrAve1 testset
    , errorRate lrAve2 testset
    , errorRate lrAve3 testset
    , errorRate lrAve4 testset
    , errorRate lrAve5 testset
    , errorRate lrAve6 testset
    , errorRate lrAve7 testset
    , 0/1/0
    , errorRate lrTaylor1 testset
    , errorRate lrTaylor2 testset
    , errorRate lrTaylor3 testset
    , errorRate lrTaylor4 testset
    , errorRate lrTaylor5 testset
    , errorRate lrTaylor6 testset
    , errorRate lrTaylor7 testset
    , 0/1/0
    , 0/1/0
    , 0/1/0
    ]
    ++ Map.elems (Map.mapWithKey go (weights zero))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights centroid))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights nb))
    ++ [0/1/0]
    ++ [0/1/0]
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights lr1))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights lr2))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights lr3))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights lr4))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights lr5))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights lr6))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights lr7))
    ++ [0/1/0]
    ++ [0/1/0]
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights lrAve1))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights lrAve2))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights lrAve3))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights lrAve4))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights lrAve5))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights lrAve6))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights lrAve7))
    ++ [0/1/0]
    ++ [0/1/0]
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights lrTaylor1))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights lrTaylor2))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights lrTaylor3))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights lrTaylor4))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights lrTaylor5))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights lrTaylor6))
    ++ [0/1/0]
    ++ Map.elems (Map.mapWithKey go (weights lrTaylor7))
--     ++ [0/1/0]
--     ++ [0/1/0]
--     ++ [0/1/0]
--     ++ [0/1/0]
--     ++ [0/1/0]
--     ++ [0/1/0]
--     ++ VG.toList (head $ Map.elems $ Map.map (\(_,w,_) -> w) $ weights lr5)
--     ++ [0/1/0]
--     ++ VG.toList (head $ Map.elems $ Map.map (\(_,w,_) -> w) $ weights lrAve5)
--     ++ [0/1/0]
--     ++ VG.toList (head $ Map.elems $ Map.map (\(_,w,_) -> w) $ weights lrTaylor5)
--     ++ [0/1/0]

    where
        zero = LinearClassifier 
            { weights = Map.map (\w -> (0,w,NoTaylor)) $ zeroWeights $ head dpsL
            , datapoints = V.fromList $ F.toList $ head dpsL
            , reg=0
            }
        nb = nbtrain $ head dpsL
        centroid = centroidtrain $ head dpsL
        lrL1 = map (lrtrain 1e-2) dpsL
        lrL2 = map (lrtrain 1e-3) dpsL
        lrL3 = map (lrtrain 1e-4) dpsL
        lrL4 = map (lrtrain 1e-5) dpsL
        lrL5 = map (lrtrain 1e-6) dpsL
        lrL6 = map (lrtrain 1e-7) dpsL
        lrL7 = map (lrtrain 0) dpsL
        lr1 = head lrL1
        lr2 = head lrL2
        lr3 = head lrL3
        lr4 = head lrL4
        lr5 = head lrL5
        lr6 = head lrL6
        lr7 = head lrL7
        lrAve1 = foldl1 mappendAverage lrL1
        lrAve2 = foldl1 mappendAverage lrL2
        lrAve3 = foldl1 mappendAverage lrL3
        lrAve4 = foldl1 mappendAverage lrL4
        lrAve5 = foldl1 mappendAverage lrL5
        lrAve6 = foldl1 mappendAverage lrL6
        lrAve7 = foldl1 mappendAverage lrL7
        lrTaylor1 = foldl1 mappendTaylor lrL1
        lrTaylor2 = foldl1 mappendTaylor lrL2
        lrTaylor3 = foldl1 mappendTaylor lrL3
        lrTaylor4 = foldl1 mappendTaylor lrL4
        lrTaylor5 = foldl1 mappendTaylor lrL5
        lrTaylor6 = foldl1 mappendTaylor lrL6
        lrTaylor7 = foldl1 mappendTaylor lrL7

        dps = concat dpsL

        go l (_,w0,_) = f w0
            where
                f w = sumOver dps $ \dp ->
                        logSumOfExp2 0 $ -(y dp * inner w (getAttributes dp))

                y dp = bool2num $ getLabel dp==l 


type DP = MaybeLabeled String (LA.Vector Double)

test = do
    
--     let {filename = "../datasets/ida/banana_data.csv"; label_index=0}
--     let {filename = "../datasets/ripley/synth.train.csv"; label_index=2}
--     let {filename = "../datasets/uci/haberman.data"; label_index=3}
--     let {filename = "../datasets/uci/pima-indians-diabetes.data"; label_index=8}
--     let {filename = "../datasets/uci/wine.csv"; label_index=0}
    let {filename = "../datasets/uci/ionosphere.csv"; label_index=34}
--     let {filename = "../datasets/uci/sonar.csv"; label_index=60}
--     let {filename = "../datasets/uci/optdigits.train.data"; label_index=64}
        
    let verbose = True

    -----------------------------------
    -- load data

    xse :: Either String (V.Vector (V.Vector String))  
        <- trace "loading reference dataset" $ fmap (decode HasHeader) $ BS.readFile filename
    xs <- case xse of 
        Right rs -> return rs
        Left str -> error $ "failed to parse CSV file " ++ filename ++ ": " ++ take 1000 str

    let numdp = VG.length xs
    let numdim = VG.length $ xs VG.! 0
    let numlabels = Set.size $ Set.fromList $ VG.toList $ VG.map (VG.! label_index) xs

    if verbose 
        then do 
            hPutStrLn stderr "  dataset info:"
            hPutStrLn stderr $ "    num dp:     " ++ show numdp
            hPutStrLn stderr $ "    num dim:    " ++ show numdim
            hPutStrLn stderr $ "    num labels: " ++ show numlabels
        else return ()

    -----------------------------------
    -- convert to right types

    let ys = VG.map (\x -> MaybeLabeled   
            { label = Just $ x VG.! label_index
            , attr = VG.convert ( 
                VG.map read $ VG.fromList $ (:) "1" $ VG.toList $ VG.take label_index x <> VG.drop (label_index+1) x
                :: V.Vector Double
                )
            })
            xs
            :: V.Vector DP -- (MaybeLabeled String (LA.Vector Double))

    -----------------------------------
    -- convert to right types

--     let m = train ys :: LinearClassifier (MaybeLabeled String (LA.Vector Double))
--     deepseq m $ print $ m


--     let runtest f = flip evalRand (mkStdGen 100) $ validate
--             (repeatExperiment 1 (kfold 5))
--             errorRate
--             ys
--             (f (lrtrain 1e-2) :: [DP] -> LinearClassifier DP)

--     let runtest f = flip evalRand (mkStdGen 100) $ validate_monoid
--             (repeatExperiment 1 (kfold 5))
--             errorRate
--             ys
--             (f (lrtrain 1e-2) :: [DP] -> LinearClassifier DP)
--             (mappendTaylor)
--             (mappendAverage)

--     let tests = 
--             [ do 
--                 putStr $ show n++", "
--                 let res = runtest id
--                 putStrLn $ show (mean res)++", "++show (variance res)
--             | n <- [100]
--             ]
--     sequence_ tests
--     print $ runtest (partition 10 $ VG.toList ys) (VG.toList ys)

    let (res,hist) = unsafeRunHistory $ flip evalRandT (mkStdGen 100) $ validateM
            (kfold 5)
            errorRate
            ys
            (lrtrainMsimple 1e-3) 

--     printHistory [traceBFGS,traceNewtonRaphson,traceBrent] hist
    printHistory 
        [ traceLinearClassifier (undefined::DP)
        , traceBFGS
        , traceNewtonRaphson
        , traceConjugateGradientDescent
--         , traceBacktracking (undefined :: Attributes DP)
--         , traceBracket
--         , traceBrent
--         , traceGSS
        ] hist
    putStrLn $ show (mean res)++","++show (variance res)


    putStrLn "done."

