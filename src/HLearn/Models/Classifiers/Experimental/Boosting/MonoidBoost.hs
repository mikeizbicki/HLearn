module HLearn.Models.Classifiers.Experimental.Boosting.MonoidBoost
    where

import Control.Applicative
import Data.List
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import Data.Sequence (fromList)
import GHC.TypeLits
import Debug.Trace

import Test.QuickCheck

import HLearn.Algebra
import HLearn.Models.Distributions.Visualization.Gnuplot
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common

-------------------------------------------------------------------------------
-- data structures

data MonoidBoost (k::Nat) basemodel = MonoidBoost
    { dataL :: Seq.Seq (Datapoint basemodel)
    , modelL :: Seq.Seq basemodel
    , weightL :: Seq.Seq (Ring basemodel)
    , boost_numdp :: Int
    }
    
deriving instance (Read (Datapoint basemodel), Read (Ring basemodel), Read basemodel) => Read (MonoidBoost k basemodel)
deriving instance (Show (Datapoint basemodel), Show (Ring basemodel), Show basemodel) => Show (MonoidBoost k basemodel)
deriving instance (Eq   (Datapoint basemodel), Eq   (Ring basemodel), Eq   basemodel) => Eq   (MonoidBoost k basemodel)
deriving instance (Ord  (Datapoint basemodel), Ord  (Ring basemodel), Ord  basemodel) => Ord  (MonoidBoost k basemodel)

instance 
    ( HomTrainer basemodel
    , Arbitrary (Datapoint basemodel)
    , SingI k
    ) => Arbitrary (MonoidBoost k basemodel) 
        where
    arbitrary = train <$> listOf arbitrary    

-------------------------------------------------------------------------------
-- algebra

testassociativity = quickCheck ((\m1 m2 m3 -> m1<>(m2<>m3)==(m1<>m2)<>m3) 
    :: MonoidBoost 3 (Normal Rational)
    -> MonoidBoost 3 (Normal Rational)
    -> MonoidBoost 3 (Normal Rational)
    -> Bool
    )

leave :: Int -> Seq.Seq a -> Seq.Seq a
leave k xs = Seq.drop (Seq.length xs - k) xs

instance 
    ( HomTrainer basemodel
    , SingI k
    ) => Monoid (MonoidBoost k basemodel) 
        where
    mempty = MonoidBoost mempty mempty mempty 0
    mb1 `mappend` mb2 = MonoidBoost
        { dataL     = dataL'
        , modelL    = modelL mb1 <> newmodel <> modelL mb2
        , weightL   = mempty
        , boost_numdp     = boost_numdp'
        }
        where
            boost_numdp' = boost_numdp mb1 + boost_numdp mb2
            dataL' = dataL mb1 <> dataL mb2
            
            newmodel = Seq.fromList $ newmodels $ leave (2*k) (dataL mb1) <> Seq.take (2*k) (dataL mb2)
            newmodels xs = if Seq.length xs >= modelsize
                then (train (Seq.take modelsize xs)):(newmodels $ Seq.drop 1 xs)
                else []

            modelsize = 2*k+1
            k = fromIntegral $ fromSing (sing::Sing k)
--             frontL mb = Seq.take k $ dataL mb
--             backL mb  = Seq.drop (Seq.length (dataL mb) - k) (dataL mb)

-------------------------------------------------------------------------------
-- model

instance 
    ( SingI k
    , HomTrainer basemodel
    ) => HomTrainer (MonoidBoost k basemodel) 
        where
    type Datapoint (MonoidBoost k basemodel) = Datapoint basemodel
    train1dp dp = MonoidBoost
        { dataL = mempty |> dp
        , modelL = mempty
        , weightL = mempty
        , boost_numdp = 1
        }
    
-------------------------------------------------------------------------------
-- classification

instance Probabilistic (MonoidBoost k basemodel) where
    type Probability (MonoidBoost k basemodel) = Probability basemodel

instance
    ( ProbabilityClassifier basemodel
    , Monoid (ResultDistribution basemodel)
    ) => ProbabilityClassifier (MonoidBoost k basemodel)
        where
    type ResultDistribution (MonoidBoost k basemodel) = ResultDistribution basemodel    
    probabilityClassify mb dp = reduce $ fmap (flip probabilityClassify dp) $ modelL mb
    
-------------------------------------------------------------------------------
-- distribution

instance 
    ( PDF basemodel
    , Fractional (Probability basemodel)
    ) => PDF (MonoidBoost k basemodel)
        where
    pdf mb dp = ave $ fmap (flip pdf dp) $ modelL mb
        where
            ave xs = (F.foldl1 (+) xs) / (fromIntegral $ Seq.length xs)

instance 
    ( PlottableDistribution basemodel
    , Fractional (Probability basemodel)
    ) => PlottableDistribution (MonoidBoost k basemodel)
        where
    
    samplePoints mb = concat $ map samplePoints $ F.toList $ modelL mb
    plotType _ = plotType (undefined :: basemodel)
    