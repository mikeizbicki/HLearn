{-# LANGUAGE DataKinds #-}
module HLearn.Models.Classifiers.KernelizedLinearClassifier
    where

import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Random
import Data.Dynamic
import Data.List
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
import System.IO

import Data.Csv

import Debug.Trace

import HLearn.Algebra
import HLearn.Algebra.History
import HLearn.Algebra.LinearAlgebra
import HLearn.Evaluation.CrossValidation
import HLearn.Metrics.Lebesgue
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common
import HLearn.Optimization.GradientDescent 
import HLearn.Optimization.NewtonRaphson 
import HLearn.Optimization.QuasiNewton 
import HLearn.Optimization.Common 
import HLearn.Optimization.Trace 

import HLearn.Models.Classifiers.LinearClassifier (l1reg,l2reg,elasticNet,logloss,hingeloss,sumOver,logSumOfExp2,invlogit)


-------------------------------------------------------------------------------
-- vector space

data FreeVectorSpace r a = FreeVectorSpace (Map.Map a r)
    deriving (Read,Show,Typeable)

applyFVS :: Num r => (a -> r) -> FreeVectorSpace r a -> r
applyFVS f (FreeVectorSpace m) = foldl' (+) 0 $ map (\(a,r) -> f a * r) $ Map.assocs m

instance (NFData r, NFData a) => NFData (FreeVectorSpace r a) where
    rnf (FreeVectorSpace m) = rnf m

-------------------

type instance Scalar (FreeVectorSpace r a) = Scalar r

instance (Ord a, Abelian r) => Abelian (FreeVectorSpace r a)
instance (Ord a, Monoid r) => Monoid (FreeVectorSpace r a) where
    mempty = FreeVectorSpace $ Map.empty
    mappend (FreeVectorSpace m1) (FreeVectorSpace m2) = FreeVectorSpace $ Map.unionWith (<>) m1 m2

instance (Ord a, Group r) => Group (FreeVectorSpace r a) where
    inverse (FreeVectorSpace m1) = FreeVectorSpace $ Map.map inverse m1

instance (Ord a, Module r) => Module (FreeVectorSpace r a) where
    r .* (FreeVectorSpace m) = FreeVectorSpace $ Map.map (r.*) m

instance (Ord a, VectorSpace r) => VectorSpace (FreeVectorSpace r a)

instance (Ord a, IsScalar r) => InnerProduct (FreeVectorSpace r a) where
    inner (FreeVectorSpace m1) (FreeVectorSpace m2) = Map.foldr (+) mempty $ Map.intersectionWith (*) m1 m2

-------------------

instance 
    ( IsScalar r
    , Ord a
    ) => ValidTensor1 (FreeVectorSpace r a) 
        where
    type Tensor 0 (FreeVectorSpace r a) = r
    type Tensor 1 (FreeVectorSpace r a) = FreeVectorSpace r a
    mkTensor = id

-------------------

instance (Ord a, IsScalar r) => HomTrainer (FreeVectorSpace r a) where
    type Datapoint (FreeVectorSpace r a) = a
    train1dp dp = FreeVectorSpace $ Map.singleton dp 1

-------------------------------------------------------------------------------
-- data types

data KernelizedLinearClassifier dp = KernelizedLinearClassifier
    { weights :: Map.Map (Label dp) (Scalar dp, FreeVectorSpace (Scalar dp) (Attributes dp))
    }
    deriving (Typeable)

-------------------------------------------------------------------------------

instance Monoid (KernelizedLinearClassifier dp) where
    mempty = error "KLC mempty"
    mappend = error "KLC mappend"

instance HomTrainer (KernelizedLinearClassifier dp) where
    type Datapoint (KernelizedLinearClassifier dp) = dp

---------------------------------------

klrtrainM :: forall container dp.
    ( ValidTensor1 (Attributes dp)
    , Attributes dp ~ Tensor 1 (Attributes dp)
    , Labeled dp
    , Ord (Label dp)
    , Ord (Attributes dp)
    , Ord (Scalar (Attributes dp))
    , F.Foldable container
    , NFData (Tensor 1 (Attributes dp))
    , NFData (Scalar (Attributes dp))
    , Typeable (Label dp)
    , Typeable (Attributes dp)
    , Typeable (Scalar dp)
    , Typeable dp
    , Show (Scalar dp)
    , Show (Attributes dp)
    ) => Scalar dp
      -> container dp
      -> Map.Map (Label dp) (Attributes dp)
      -> History (KernelizedLinearClassifier dp)
klrtrainM lambda dps weights0 = do
    let emptyweights = Map.fromList $ map (\dp -> (getLabel dp,mempty)) $ F.toList dps
    weights' <- collectEvents $ fmap Map.fromList $ go $ Map.assocs emptyweights
    report $ KernelizedLinearClassifier 
        { weights = weights'
        }
    where
        n l = fromIntegral $ length $ filter (\dp -> getLabel dp ==l) $ F.toList dps

        -- the weights for the last label are set to zero;
        -- this is equivalent to running the optimization procedure,
        -- but much cheaper
        go ((l,w0):[]) = report [(l, (n l,mempty))]

        -- calculate the weights for label l
        go ((l,w0):xs) = trace "go" $ do
            opt <- conjugateGradientDescent f f' w0 
                [ maxIterations 1000
                , fx1grows
                , multiplicativeTollerance 1e-12
                ]

            let w1 = opt^.x1
--                 fw1 = f w1
--                 f'w1 = f' w1
--                 f''w1 = f'' w1

            res <- report 
--                 $ trace ("w1="++show w1)
                $ deepseq w1 
                $ (l, (n l, w1))

            fmap (res:) $ go xs
            where
                reg  (FreeVectorSpace m) = Map.foldr (+) 0 $ Map.map (**2) m
                reg' w = 2 .* w

                
                loss  dp w = let r=logSumOfExp2 0 $ -y dp * applyFVS (inner (getAttributes dp)) w
                                 x=applyFVS (inner (getAttributes dp)) w
                             in r -- trace ("  r="++show r++"; appyVFS="++show x) r
                loss' dp w = (-y dp*(1-invlogit (y dp * applyFVS (inner (getAttributes dp)) w))) 
                    .* (train1dp $ getAttributes dp :: FreeVectorSpace (Scalar dp) (Attributes dp))

                f :: FreeVectorSpace (Scalar (Attributes dp)) (Attributes dp) -> Scalar (Attributes dp)
                f   w = (numdp*lambda .* reg   w) <> (sumOver dps $ \dp -> loss   dp w)
                f'  w = (numdp*lambda .* reg'  w) <> (sumOver dps $ \dp -> loss'  dp w)

                numdp :: Scalar dp
                numdp = fromIntegral $ length $ F.toList dps

                y dp = bool2num $ getLabel dp==l

klrtrain lambda dps = klrtrain2 lambda dps 
klrtrainMsimple lambda dps = klrtrainM lambda dps mempty

klrtrain2 :: forall dp.
    ( Tensor 1 (Attributes dp) ~ Attributes dp
    , ValidTensor1 (Attributes dp)
    , Scalar dp ~ Double
    , Ord (Attributes dp)
    , Ord (Label dp)
    , Labeled dp
    , NFData (Attributes dp)
    , Show (Attributes dp)
    , Typeable dp
    , Typeable (Attributes dp)
    , Typeable (Label dp)
    , Typeable (Scalar dp)
    ) => Scalar dp -> [dp] -> KernelizedLinearClassifier dp
klrtrain2 lambda dps = traceHistory 
    [ traceBFGS
    , traceNewtonRaphson
    , traceFunk (undefined::ConjugateGradientDescent (FreeVectorSpace (Scalar dp) (Attributes dp)))
    , traceKernelizedLinearClassifier (undefined::dp)
    ]
    $ collectEvents $ klrtrainM lambda dps mempty


-------------------------------------------------------------------------------
-- classification

instance 
    ( Labeled dp
    , InnerProduct (Attributes dp)
    , Ord (Scalar dp)
    ) => Classifier (KernelizedLinearClassifier dp) 
        where
    classify m attr = fst $ argmax snd $ Map.assocs $ Map.map (\(_,w) -> applyFVS (inner attr) w) $ weights m

-------------------------------------------------------------------------------
-- tests

type DP = MaybeLabeled String (Vector Double)

traceKernelizedLinearClassifier :: forall dp. Typeable dp => dp -> Event -> [String]
traceKernelizedLinearClassifier _ opt = case fromDynamic (dyn opt) :: Maybe (KernelizedLinearClassifier dp) of
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

test = do
    
--     let {filename = "../datasets/ida/banana_data.csv"; label_index=0}
--     let {filename = "../datasets/ripley/synth.train.csv"; label_index=2}
--     let {filename = "../datasets/uci/haberman.data"; label_index=3}
--     let {filename = "../datasets/uci/pima-indians-diabetes.data"; label_index=8}
    let {filename = "../datasets/uci/wine.csv"; label_index=0}
--     let {filename = "../datasets/uci/ionosphere.csv"; label_index=34}
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
            (klrtrainMsimple 1e-3) 


--     printHistory [traceBFGS,traceNewtonRaphson,traceBrent] hist
    printHistory 
        [ traceFunk (undefined::ConjugateGradientDescent (FreeVectorSpace (Scalar DP) (Attributes DP)))
--         [ traceKernelizedLinearClassifier (undefined::DP)
--         , traceBFGS
--         , traceNewtonRaphson
        , traceBacktracking (undefined :: FreeVectorSpace (Scalar DP) (Attributes DP))
        , traceBracket
        , traceBrent
--         , traceGSS
        ] hist
    putStrLn $ show (mean res)++","++show (variance res)


    putStrLn "done."

