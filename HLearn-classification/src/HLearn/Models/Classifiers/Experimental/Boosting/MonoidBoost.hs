{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

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

data MonoidBoost (k::Nat) weight basemodel datapoint = MonoidBoost
    { dataL :: Seq.Seq datapoint
    , modelL :: Seq.Seq basemodel
    , weightL :: Seq.Seq weight
    , boost_numdp :: Int
    }
    deriving (Read,Show,Eq,Ord)

instance 
    ( HomTrainer basemodel
    , Datapoint basemodel ~ datapoint
    , Arbitrary datapoint
    , SingI k
    ) => Arbitrary (MonoidBoost k weight basemodel datapoint) 
        where
    arbitrary = train <$> listOf arbitrary    

-------------------------------------------------------------------------------
-- algebra

testassociativity = quickCheck ((\m1 m2 m3 -> m1<>(m2<>m3)==(m1<>m2)<>m3) 
    :: MonoidBoost 3 Rational (Normal Rational) Rational
    -> MonoidBoost 3 Rational (Normal Rational) Rational
    -> MonoidBoost 3 Rational (Normal Rational) Rational
    -> Bool
    )

leave :: Int -> Seq.Seq a -> Seq.Seq a
leave k xs = Seq.drop (Seq.length xs - k) xs

instance 
    ( HomTrainer basemodel
    , Datapoint basemodel ~ datapoint
    , SingI k
    ) => Monoid (MonoidBoost k weight basemodel datapoint) 
        where
    mempty = MonoidBoost {-mempty mempty-} mempty mempty mempty 0
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
    , Datapoint basemodel ~ datapoint
    ) => HomTrainer (MonoidBoost k weight basemodel datapoint) 
        where
    type Datapoint (MonoidBoost k weight basemodel datapoint) = datapoint
    train1dp dp = MonoidBoost
        { dataL = mempty |> dp
        , modelL = mempty
        , weightL = mempty
        , boost_numdp = 1
        }
    
-------------------------------------------------------------------------------
-- classification

instance (Ord prob) => Probabilistic (MonoidBoost k weight basemodel prob) where
    type Probability (MonoidBoost k weight basemodel prob) = prob

instance
    ( Classifier basemodel
    , Probability basemodel ~ weight
    , Ord (Label basemodel)
    , Ord prob
    , Num prob
    , weight ~ prob
    ) => Classifier (MonoidBoost k weight basemodel prob)
        where
    type Label (MonoidBoost k weight basemodel prob) = Label basemodel
    type UnlabeledDatapoint (MonoidBoost k weight basemodel prob) = UnlabeledDatapoint basemodel
    type ResultDistribution (MonoidBoost k weight basemodel prob) = ResultDistribution basemodel
    
    probabilityClassify mb dp = reduce $ fmap (flip probabilityClassify dp) $ modelL mb
    
-------------------------------------------------------------------------------
-- distribution

instance 
    ( PDF basemodel
    , Datapoint basemodel ~ prob
    , Probability basemodel ~ prob
    , Ord prob
    , Fractional prob
    ) => PDF (MonoidBoost k weight basemodel prob)
        where
    pdf mb dp = ave $ fmap (flip pdf dp) $ modelL mb
        where
            ave xs = (F.foldl1 (+) xs) / (fromIntegral $ Seq.length xs)

-- instance 
--     ( PlottableDistribution basemodel prob prob
--     , Fractional prob
--     ) => PlottableDistribution (MonoidBoost k weight basemodel prob) prob prob
--         where
--     minx mb = minimum $ F.toList $ fmap minx $ modelL mb
--     maxx mb = maximum $ F.toList $ fmap maxx $ modelL mb    
    