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

module HLearn.Models.Classifiers.Boosting.MonoidBoost
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
import HLearn.Gnuplot.Distributions
import HLearn.Models.Distributions hiding (Uniform)
import HLearn.Models.Distributions.Uniform
import HLearn.Models.Classification

-------------------------------------------------------------------------------
-- data structures

data MonoidBoost (k::Nat) weight basemodel datapoint = MonoidBoost
    {-{ frontL :: Seq.Seq datapoint
    , backL :: Seq.Seq datapoint
    ,-} 
    { dataL :: Seq.Seq datapoint
    , modelL :: Seq.Seq basemodel
    , weightL :: Seq.Seq weight
    , numdp :: Int
    }
    deriving (Read,Show,Eq,Ord)

instance 
    ( DefaultHomTrainer baseparams datapoint basemodel
    , Arbitrary datapoint
    , SingI k
    ) => Arbitrary (MonoidBoost k weight basemodel datapoint) 
        where
    arbitrary = train <$> listOf arbitrary    

-------------------------------------------------------------------------------
-- algebra

testassociativity = quickCheck ((\m1 m2 m3 -> m1<>(m2<>m3)==(m1<>m2)<>m3) 
    :: MonoidBoost 3 Rational (Moments Rational) Rational
    -> MonoidBoost 3 Rational (Moments Rational) Rational
    -> MonoidBoost 3 Rational (Moments Rational) Rational
    -> Bool
    )

-- m1=MonoidBoost {frontL = fromList [-54], backL = fromList [-54], dataL = fromList [], modelL = fromList [], weightL = fromList [], numdp = 1} :: MonoidBoost 2 Int Int Int
-- m2=MonoidBoost {frontL = fromList [-46,7], backL = fromList [7,11], dataL = fromList [], modelL = fromList [], weightL = fromList [], numdp = 3} :: MonoidBoost 2 Int Int Int
-- m3=MonoidBoost {frontL = fromList [22,21], backL = fromList [-49,64], dataL = fromList [-1], modelL = fromList [], weightL = fromList [], numdp = 5} :: MonoidBoost 2 Int Int Int

instance 
    ( DefaultHomTrainer baseparams datapoint basemodel
    , SingI k
    ) => Semigroup (MonoidBoost k weight basemodel datapoint) 
        where
    mb1 <> mb2 = MonoidBoost
--         { frontL    = Seq.take k $ frontL mb1 <> frontL mb2
--         , backL     = leave    k $ backL  mb1 <> backL  mb2
--         , dataL     = dataL  mb1 <> newdata  <> dataL  mb2
        { dataL     = dataL'
        , modelL    = modelL mb1 <> newmodel <> modelL mb2
        , weightL   = mempty
        , numdp     = numdp'
        }
        where
            numdp' = numdp mb1 + numdp mb2
            dataL' = dataL mb1 <> dataL mb2
            
            newmodel = Seq.fromList $ newmodels $ leave (2*k) (dataL mb1) <> Seq.take (2*k) (dataL mb2)
            newmodels xs = if Seq.length xs >= modelsize
                then (train (Seq.take modelsize xs)):(newmodels $ Seq.drop 1 xs)
                else []

            modelsize = 2*k+1
            k = fromIntegral $ fromSing (sing::Sing k)
--             frontL mb = Seq.take k $ dataL mb
--             backL mb  = Seq.drop (Seq.length (dataL mb) - k) (dataL mb)


leave :: Int -> Seq.Seq a -> Seq.Seq a
leave k xs = Seq.drop (Seq.length xs - k) xs

test12 = train [1,2] :: MonoidBoost 2 Double (Gaussian Double) Double
test34 = train [3,4] :: MonoidBoost 2 Double (Gaussian Double) Double
test56 = train [5,6] :: MonoidBoost 2 Double (Gaussian Double) Double
test78 = train [7,8] :: MonoidBoost 2 Double (Gaussian Double) Double
ta = train [-5,3] :: MonoidBoost 2 Double (Gaussian Double) Double
tb = train [-4,2] :: MonoidBoost 2 Double (Gaussian Double) Double

instance 
    (DefaultHomTrainer baseparams datapoint basemodel
    , SingI k
    ) => Monoid (MonoidBoost k weight basemodel datapoint) 
        where
    mempty = MonoidBoost {-mempty mempty-} mempty mempty mempty 0
    mappend = (<>)

-------------------------------------------------------------------------------
-- model

instance ModelParams (NoParams MonoidBoost) (MonoidBoost k weight basemodel datapoint) where
    getparams _ = NoParams
    
instance DefaultParams (NoParams MonoidBoost) (MonoidBoost k weight basemodel datapoint) where
    defparams = NoParams
    
instance 
    ( SingI k
    , DefaultHomTrainer baseparams datapoint basemodel
    ) => HomTrainer (NoParams MonoidBoost) datapoint (MonoidBoost k weight basemodel datapoint) 
        where
    train1dp' _ dp = MonoidBoost
        { dataL = mempty |> dp
        , modelL = mempty
        , weightL = mempty
        , numdp = 1
        }
    
-------------------------------------------------------------------------------
-- classification

instance
    ( ProbabilityClassifier basemodel datapoint label prob
    , Ord label
    , Num prob
    ) => ProbabilityClassifier (MonoidBoost k weight basemodel prob) datapoint label prob
        where
    
    probabilityClassify mb dp = reduce $ fmap (flip probabilityClassify dp) $ modelL mb
    
-------------------------------------------------------------------------------
-- distribution

instance 
    ( PDF basemodel prob prob
    , Fractional prob
    ) => PDF (MonoidBoost k weight basemodel  prob)  prob prob 
        where
    pdf mb dp = ave $ fmap (flip pdf dp) $ modelL mb
        where
            ave xs = (F.foldl1 (+) xs) / (fromIntegral $ Seq.length xs)

instance 
    ( PlottableDistribution basemodel prob prob
    , Fractional prob
    ) => PlottableDistribution (MonoidBoost k weight basemodel prob) prob prob
        where
    minx mb = minimum $ F.toList $ fmap minx $ modelL mb
    maxx mb = maximum $ F.toList $ fmap maxx $ modelL mb    

    
-------------------------------------------------------------------------------
-- obsolete

-- data MonoidMaker (k::Nat) datapoint = MonoidMaker
--     { front :: Seq.Seq datapoint
--     , back :: Seq.Seq datapoint
--     }
--     deriving (Read,Show,Eq,Ord)
-- instance Semigroup (MonoidMaker k datapoint) where
--     mm1 <> mm2 = MonoidMaker
--         { 
--         }
--         where
--             k = 3
-- 
-- instance Monoid (MonoidMaker k datapoint) where
--     mempty = MonoidMaker mempty mempty
--     mappend = (<>)
{-instance ModelParams (NoParams (MonoidMaker k datapoint)) (MonoidMaker k datapoint) where
    getparams _ = NoParams
    
instance DefaultParams (NoParams (MonoidMaker k datapoint)) (MonoidMaker k datapoint) where
    defparams = NoParams
    
instance HomTrainer (NoParams (MonoidMaker k datapoint)) datapoint (MonoidMaker k datapoint) where
    train1dp' _ dp = MonoidMaker
        { front = mempty |> dp
        , back = mempty |> dp
        }-}
    