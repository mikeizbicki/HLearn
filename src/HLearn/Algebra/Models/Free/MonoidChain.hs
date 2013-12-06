{-# LANGUAGE DataKinds #-}

module HLearn.Algebra.Models.Free.MonoidChain
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
import HLearn.Algebra.Structures.Triangles
import HLearn.Models.Distributions.Visualization.Gnuplot
import HLearn.Models.Distributions
-- import HLearn.Models.Classifiers.Common

-------------------------------------------------------------------------------
-- data structures

data MonoidChain (k::Nat) basemodel = MonoidChain
    { dataL :: Seq.Seq (Datapoint basemodel)
    , modelL :: Seq.Seq basemodel
    , boost_numdp :: Int
    }
--     deriving (Read,Show,Eq,Ord)
    
deriving instance (Read (Datapoint basemodel), Read basemodel) => Read (MonoidChain k basemodel)
deriving instance (Show (Datapoint basemodel), Show basemodel) => Show (MonoidChain k basemodel)
deriving instance (Eq   (Datapoint basemodel), Eq   basemodel) => Eq   (MonoidChain k basemodel)
deriving instance (Ord  (Datapoint basemodel), Ord  basemodel) => Ord  (MonoidChain k basemodel)

instance 
    ( HomTrainer basemodel
    , Arbitrary (Datapoint basemodel)
    , SingI k
    ) => Arbitrary (MonoidChain k basemodel) 
        where
    arbitrary = train <$> listOf arbitrary    

-------------------------------------------------------------------------------
-- algebra

leave :: Int -> Seq.Seq a -> Seq.Seq a
leave k xs = Seq.drop (Seq.length xs - k) xs

instance 
    ( HomTrainer basemodel
    , SingI k
    ) => Monoid (MonoidChain k basemodel) 
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
            k = fromIntegral $ fromSing (sing::Sing k)
--             frontL mb = Seq.take k $ dataL mb
--             backL mb  = Seq.drop (Seq.length (dataL mb) - k) (dataL mb)

-------------------------------------------------------------------------------
-- model

instance 
    ( SingI k
    , HomTrainer basemodel
    ) => HomTrainer (MonoidChain k basemodel) 
        where
    type Datapoint (MonoidChain k basemodel) = Datapoint basemodel
    train1dp dp = MonoidChain
        { dataL = mempty |> dp
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
    