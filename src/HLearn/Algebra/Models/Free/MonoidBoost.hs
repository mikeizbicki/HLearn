{-# LANGUAGE DataKinds #-}

module HLearn.Algebra.Models.Free.MonoidBoost
    where

import Control.Applicative
import Data.List
import Data.Monoid
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import Data.Sequence (fromList)
import GHC.TypeLits
import Debug.Trace

import Test.QuickCheck

import HLearn.Algebra.Functions
import HLearn.Algebra.Models.HomTrainer
import HLearn.Algebra.Structures.Modules
import HLearn.Algebra.Structures.Triangles
import HLearn.Models.Distributions.Visualization.Gnuplot
import HLearn.Models.Distributions
import HLearn.Models.Classifiers.Common

-------------------------------------------------------------------------------
-- data structures

data DataTracker ring dp = DataTracker
    { totalGain :: !ring
    , totalLoss :: !ring
    , dataTracker :: !dp
    }
    deriving (Read,Show,Eq,Ord)

data MonoidBoost (k::Nat) basemodel = MonoidBoost
    { dataLeft     :: Seq.Seq (Ring basemodel, Ring basemodel, Datapoint basemodel)
    , dataRight    :: Seq.Seq (Ring basemodel, Ring basemodel, Datapoint basemodel)
    , modelL            :: Seq.Seq basemodel
    , boost_numdp       :: Int
    }
--     deriving (Read,Show,Eq,Ord)
deriving instance (Read (Datapoint basemodel), Read (Ring basemodel), Read basemodel) => Read (MonoidBoost k basemodel)
deriving instance (Show (Datapoint basemodel), Show (Ring basemodel), Show basemodel) => Show (MonoidBoost k basemodel)
deriving instance (Eq   (Datapoint basemodel), Eq   (Ring basemodel), Eq   basemodel) => Eq   (MonoidBoost k basemodel)
deriving instance (Ord  (Datapoint basemodel), Ord  (Ring basemodel), Ord  basemodel) => Ord  (MonoidBoost k basemodel)

instance 
    ( HomTrainer basemodel
    , HasRing basemodel
    , Arbitrary (Datapoint basemodel)
    , SingI k
    ) => Arbitrary (MonoidBoost k basemodel) 
        where
    arbitrary = train <$> listOf arbitrary    

-------------------------------------------------------------------------------
-- algebra

instance 
    ( HomTrainer basemodel
    , SingI k
    ) => Monoid (MonoidBoost k basemodel) 
        where
    mempty = MonoidBoost mempty mempty mempty 0
    a `mappend` b = MonoidBoost
        { dataLeft = dataLeft a <> rightLeft
        , dataRight = dataRight b <> leftRight
        , modelL = undefined
        , boost_numdp = boost_numdp a + boost_numdp b
        }
        where
            rightLeft = undefined
            leftRight = undefined
{-leave :: Int -> Seq.Seq a -> Seq.Seq a
leave k xs = Seq.drop (Seq.length xs - k) xs

instance 
    ( HomTrainer basemodel
    , Datapoint basemodel ~ datapoint
    , SingI k
    ) => Monoid (MonoidChain k basemodel datapoint) 
        where
    mempty = MonoidChain mempty mempty 0
    mb1 `mappend` mb2 = MonoidChain
        { dataL     = dataL'
        , modelL    = modelL mb1 <> newmodel <> modelL mb2
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
            k = fromIntegral $ fromSing (sing::Sing k)-}

-------------------------------------------------------------------------------
-- model

instance 
    ( SingI k
    , HasRing basemodel
    , HomTrainer basemodel
    ) => HomTrainer (MonoidBoost k basemodel) 
        where
    type Datapoint (MonoidBoost k basemodel) = Datapoint basemodel
    train1dp dp = MonoidBoost
        { dataLeft = (1::Ring basemodel,1::Ring basemodel,dp) <| mempty
        , dataRight = mempty |> (1::Ring basemodel,1::Ring basemodel,dp)
        , modelL = mempty
        , boost_numdp = 1
        }
    
-------------------------------------------------------------------------------
-- classification

{-instance (Ord prob) => Probabilistic (MonoidChain k weight basemodel prob) where
    type Probability (MonoidChain k weight basemodel prob) = prob

instance
    ( Classifier basemodel
    , Probability basemodel ~ weight
    , Ord (Label basemodel)
    , Ord prob
    , Num prob
    , weight ~ prob
    ) => Classifier (MonoidChain k weight basemodel prob)
        where
--     type Label (MonoidChain k weight basemodel prob) = Label basemodel
--     type UnlabeledDatapoint (MonoidChain k weight basemodel prob) = UnlabeledDatapoint basemodel
    type ResultDistribution (MonoidChain k weight basemodel prob) = ResultDistribution basemodel
    
    probabilityClassify mb dp = reduce $ fmap (flip probabilityClassify dp) $ modelL mb-}
    
-------------------------------------------------------------------------------
-- distribution

-- instance 
--     ( PDF basemodel
--     , Datapoint basemodel ~ prob
--     , Probability basemodel ~ prob
--     , Ord prob
--     , Fractional prob
--     ) => PDF (MonoidChain k weight basemodel prob)
--         where
--     pdf mb dp = ave $ fmap (flip pdf dp) $ modelL mb
--         where
--             ave xs = (F.foldl1 (+) xs) / (fromIntegral $ Seq.length xs)

-- instance 
--     ( PlottableDistribution basemodel prob prob
--     , Fractional prob
--     ) => PlottableDistribution (MonoidChain k weight basemodel prob) prob prob
--         where
--     minx mb = minimum $ F.toList $ fmap minx $ modelL mb
--     maxx mb = maximum $ F.toList $ fmap maxx $ modelL mb    
    