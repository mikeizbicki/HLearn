module HLearn.Models.Classifiers.LogisticRegression
    where

import Control.Monad
import Control.Monad.Random
import qualified Data.ByteString.Lazy as BS
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import System.IO

import Data.Csv
import Numeric.LinearAlgebra hiding ((<>))
import qualified Numeric.LinearAlgebra as LA

import Debug.Trace

import HLearn.Algebra
import HLearn.Evaluation.CrossValidation
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common


-------------------------------------------------------------------------------
-- data types

data LogisticRegression dp = LogisticRegression
    { m0 :: !(Map.Map (Label dp) (Ring dp))
    , m1 :: !(Map.Map (Label dp) (Attributes dp))
    , m2 :: !(Attributes dp)
    , w :: (VS.Vector (Ring dp))
    }

data LogRes dp = LogRes
    { _w :: !(Map.Map (Label dp) (Attributes dp))
    , _bias :: Ring dp
    }

class InnerProduct v where
    inner :: v -> v -> Ring v

instance (VS.Storable r, Num r) => InnerProduct (Vector r) where
    inner v1 v2 = sum $ zipWith (+) (VG.toList v1) (VG.toList v2)

deriving instance 
    ( Show (Label dp)
    , Show (Ring dp)
    , Show (Attributes dp)
    , VS.Storable (Ring dp)
    ) => Show (LogisticRegression dp)

-------------------------------------------------------------------------------
-- algebra

instance HasRing dp => HasRing (LogisticRegression dp) where
    type Ring (LogisticRegression dp) = Ring dp

instance 
    ( Ord (Label dp)
    , VG.Vector vec a
    , (Attributes dp) ~ vec a
    , Num (Attributes dp)
    , HasRing dp
    , Fractional a
    , Fractional (Ring dp)
    , VS.Storable (Ring dp)
    ) => Monoid (LogisticRegression dp) 
        where
    mempty = LogisticRegression
        { m0 = mempty
        , m1 = mempty
--         , m2 = VG.fromList $ replicate 8 0
        , m2 = VG.fromList $ replicate 1000 0
        , w = VG.replicate 1000 0
        }

    mappend a b = LogisticRegression
        { m0 = Map.unionWith (+) (m0 a) (m0 b)
        , m1 = Map.unionWith (+) (m1 a) (m1 b)
        , m2 = VG.fromList $ zipWith (+) (VG.toList $ m2 a) (VG.toList $ m2 b)
        , w = VG.fromList $ zipWith (\ax bx -> ax/numdp a + bx/numdp b) (VG.toList $ w a) (VG.toList $ w b)
        }

-------------------------------------------------------------------------------
-- training

instance (HasRing dp, Fractional (Ring dp)) => NumDP (LogisticRegression dp) where
    numdp m = sum $ Map.elems $ m0 m

instance 
    ( Ord (Label dp)
    , VG.Vector vec a
    , (Attributes dp) ~ vec a
    , Num (Attributes dp)
    , Labeled dp
    , HasRing dp
    , Floating a
    , Fractional (Ring dp)
    , VS.Storable (Ring dp)
    , vec ~ Vector
    , LA.Field a
    , a ~ Ring dp
    ) => HomTrainer (LogisticRegression dp) 
        where
    type Datapoint (LogisticRegression dp) = dp

    train1dp dp = LogisticRegression
        { m0 = Map.singleton (getLabel dp) 1
        , m1 = Map.singleton (getLabel dp) (getAttributes dp)
        , m2 = VG.map (**2) $ getAttributes dp
        , w = undefined
        }

    train dps = m
        { w = undefined
        }
        where
            m = batch train1dp dps

--     train dps = m
--         { w = VG.zipWith3 (\a b c -> (a-b)/c) mu0 mu1 $ VG.convert varV
--         }
--         where 
--             mu0 = scale (1 / Map.elems (m0 m) !! 0) $ Map.elems (m1 m) !! 0 
--             mu1 = scale (1 / Map.elems (m0 m) !! 1) $ Map.elems (m1 m) !! 1 
--             varV = VG.convert $ VG.map variance normV :: Vector a
--             normV = F.foldl1 (V.zipWith (<>)) $ map (V.map train1dp . VG.convert)  $ map getAttributes $ F.toList dps :: V.Vector (Normal a a)
--             m = batch train1dp dps

-------------------------------------------------------------------------------
-- classification

-- instance 
--     ( Labeled dp
--     ) => ProbabilityClassifier (LogisticRegression dp)
--         where
--     type ResultDistribution (LogisticRegression dp) = Categorical (Ring dp) (Label dp)
-- 
--     probabilityClassify m attr = 
--     probabilityClassify :: model -> Attributes (Datapoint model) -> ResultDistribution model
    
getparams m = (bias,weights)
    where
        bias = (log $ (0-pi)/pi) + (VG.sum $ VG.zipWith3 (\a b c -> (a*a-b*b)/(2*c)) mu1 mu0 sigma2)
        pi = Map.elems (m0 m) !! 0 / (sum $ Map.elems (m0 m))

--         weights = w m
        weights = VG.zipWith3 (\a b c -> (a-b)/c) mu0 mu1 sigma2
        mu0 = scale (1 / Map.elems (m0 m) !! 0) $ Map.elems (m1 m) !! 0 
        mu1 = scale (1 / Map.elems (m0 m) !! 1) $ Map.elems (m1 m) !! 1 
        sigma2 = scale (1 / (sum $ Map.elems $ m0 m)) $ VG.zipWith3 (\a b c -> a - (b+c)**2) 
            (m2 m) (Map.elems (m1 m) !! 0) (Map.elems (m1 m) !! 1)


instance
    ( Ord (Label dp)
    , VG.Vector vec a
    , (Attributes dp) ~ vec a
    , Num (Attributes dp)
    , Labeled dp
    , HasRing dp
    , Floating a
    , Ord a
    , a ~ Ring dp
    , Container vec a
    , vec ~ Vector
    , Show (Label dp)
    , Show a
    ) => Classifier (LogisticRegression dp)
        where

--     classify m attr = if 0 > tot then one else zero
    classify m attr = if 0 > tot then zero else one
        where
            (bias,weights) = getparams m
            tot = bias + VG.sum (weights * attr)
            one = head $ Map.keys $ m1 m
            zero = head $ drop 1 $ Map.keys $ m1 m
--     classify :: model -> Attributes (Datapoint model) -> Label (Datapoint model)

instance Num r => HasRing (Vector r) where
    type Ring (Vector r) = r

test = do
    
    let {filename = "../datasets/uci/pima-indians-diabetes.data"; label_index=8}
--     let {filename = "../datasets/uci/ionosphere.csv"; label_index=34}
--     let {filename = "../datasets/uci/sonar.csv"; label_index=60}
        
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
                VG.map read $ VG.fromList $ VG.toList $ VG.take label_index x <> VG.drop (label_index+1) x
                :: V.Vector Double
                )
            })
            xs
            :: V.Vector (MaybeLabeled String (VS.Vector Double))

    -----------------------------------
    -- convert to right types

--     let m = train $ VG.take 50 ys :: LogisticRegression (MaybeLabeled String (VS.Vector Double))
--     print $ m
    let res = flip evalRand (mkStdGen 1) $ crossValidate
            (kfold 10)
            errorRate
--             (VG.take 150 ys)
            ys
            (undefined :: LogisticRegression (MaybeLabeled String (VS.Vector Double)))

    putStrLn $ "mean = "++show (mean res)
    putStrLn $ "var  = "++show (variance res)

    putStrLn "done."
