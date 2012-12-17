{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HLearn.Models.DistributionContainer
    where

import Debug.Trace
import GHC.Float (double2Float, float2Double)
import Numeric.SpecFunctions (logFactorial)
import Test.QuickCheck hiding (classify)

import qualified Data.Foldable as F
import qualified Data.Map as Map

import Prelude hiding (log)

import HLearn.Algebra
import HLearn.DataContainers
import HLearn.Models.Classification
import HLearn.Models.Distributions

-------------------------------------------------------------------------------

data MaybeDistributionParams baseparams = MaybeDistributionParams baseparams

instance (Model baseparams basedist) => Model (MaybeDistributionParams baseparams) (MaybeDistribution basedist) where
    getparams (MaybeDistribution basedist) = MaybeDistributionParams $ getparams basedist
    
instance (DefaultModel baseparams basedist) => DefaultModel (MaybeDistributionParams baseparams) (MaybeDistribution basedist) where
    defparams = MaybeDistributionParams $ defparams

newtype MaybeDistribution basedist = MaybeDistribution basedist
    deriving (Eq,Show,Read,Semigroup)

instance (Monoid basedist) => Monoid (MaybeDistribution basedist)
    where 
        mempty = MaybeDistribution mempty
        mappend (MaybeDistribution d1) (MaybeDistribution d2) = MaybeDistribution $ d1 `mappend` d2

instance 
    ( DefaultHomTrainer baseparams datatype basedist
    , Monoid basedist
    ) => HomTrainer (MaybeDistributionParams baseparams) (Maybe datatype) (MaybeDistribution basedist)
        where
    
    train1dp' (MaybeDistributionParams baseparams) Nothing  = MaybeDistribution mempty
    train1dp' (MaybeDistributionParams baseparams) (Just x) = MaybeDistribution $ train1dp x
-- instance (DistributionEstimator basedist datatype) => DistributionEstimator (MaybeDistribution basedist) (Maybe datatype) where
--     add1sample (MaybeDistribution basedist) dp = case dp of
--         Nothing -> MaybeDistribution $ basedist
--         Just x  -> MaybeDistribution $ add1sample basedist x

instance 
    ( Distribution basedist sampletype Double
    ) => Distribution (MaybeDistribution basedist) (Maybe sampletype) Double
        where
              
    pdf (MaybeDistribution basedist) dp = case dp of
        Nothing -> 1
        Just x  -> pdf basedist x
        
    cdf (MaybeDistribution basedist) dp = case dp of
        Nothing -> 0
        Just x  -> cdf basedist x
        
    cdfInverse (MaybeDistribution basedist) prob = Just $ cdfInverse basedist prob

--     drawSample (MaybeDistribution basedist) = do
--         sample <- drawSample basedist
--         return $ Just sample

---------------------------------------
    
newtype ContinuousDistribution basedist = ContinuousDistribution (MaybeDistribution basedist)
    deriving (Eq,Show,Read,Semigroup)

data ContinuousDistributionParams baseparams = ContinuousDistributionParams 

instance Model (ContinuousDistributionParams baseparams) (ContinuousDistribution basedist) where
    getparams _ = ContinuousDistributionParams

instance DefaultModel (ContinuousDistributionParams baseparams) (ContinuousDistribution basedist) where
    defparams = ContinuousDistributionParams

instance 
    ( DefaultHomTrainer (MaybeDistributionParams baseparams) (Maybe Double) (MaybeDistribution basedist)
    , Monoid basedist
    , Semigroup basedist
    ) => 
    HomTrainer (ContinuousDistributionParams baseparams) DataItem  (ContinuousDistribution basedist) 
        where

    train1dp' ContinuousDistributionParams dp = case dp of
        Missing      -> ContinuousDistribution $ train1dp (Nothing :: Maybe Double)
        Continuous x -> ContinuousDistribution $ train1dp $ Just x
        Discrete   x -> error "ContinuousDistribution.add1sample: cannot add discrete"
        

instance 
    ( Distribution (MaybeDistribution basedist) (Maybe Double) Double
    , Monoid basedist
    ) => 
    Distribution (ContinuousDistribution basedist) DataItem Double
        where
              
    pdf (ContinuousDistribution basedist) dp = case dp of
        Missing      -> pdf basedist (Nothing :: Maybe Double)
        Continuous x -> pdf basedist $ Just x
        Discrete   x -> error "ContinuousDistribution.pdf: cannot sample discrete"
        
    cdf = error "ContinuousDistribution.cdf: not implemented"

--     cdfInverse = error "ContinuousDistribution.cdfInverse: not implemented"
    cdfInverse (ContinuousDistribution maybedist) prob = case cdfInverse maybedist prob of
        Nothing -> Missing
        Just x -> Continuous x
--     cdfInverse (ContinuousDistribution maybedist) (Discrete x) = error "ContinuousDistribution.cdfInverse: cannot discrete"
--     cdfInverse (ContinuousDistribution maybedist) (Continuous x) = cdfInverse maybedist $ Just x

--     drawSample (ContinuousDistribution basedist) = do
--         sample <- drawSample basedist
--         return $ case sample of
--             Nothing -> Missing
--             Just x -> Continuous x

instance (NFData basedist) => NFData (ContinuousDistribution basedist) where
    rnf (ContinuousDistribution (MaybeDistribution basedist)) = rnf basedist

instance (RegularSemigroup basedist) => RegularSemigroup (ContinuousDistribution basedist) where
    inverse (ContinuousDistribution (MaybeDistribution basedist)) = ContinuousDistribution $ MaybeDistribution $ inverse basedist

instance (Monoid basedist) => Monoid (ContinuousDistribution basedist) where
    mempty = ContinuousDistribution $ MaybeDistribution mempty
    
    mappend (ContinuousDistribution (MaybeDistribution basedist1)) (ContinuousDistribution (MaybeDistribution basedist2)) = 
        ContinuousDistribution $ MaybeDistribution $ mappend basedist1 basedist2
    
-------------------------------------------------------------------------------
-- DistContainer
   
data DistContainer = UnknownDist
                   | DistContainer (ContinuousDistribution (Gaussian Double))
                   | DistDiscrete (Categorical DataItem Double)
--                    | DistContainer Poisson
    deriving (Show{-,Read,Eq-})

data DistContainerParams = DistContainerParams

instance Semigroup DistContainer where
    (<>) = mappend 

instance Monoid DistContainer where
    mempty = UnknownDist
    mappend UnknownDist b = b
    mappend a UnknownDist = a
    mappend (DistContainer a) (DistContainer b) = DistContainer $ mappend a b
    mappend (DistDiscrete a) (DistDiscrete b) = DistDiscrete $ mappend a b -- error "DistContiner.mappend (DistDiscrete) not yet implemented"

instance RegularSemigroup DistContainer where
    inverse UnknownDist = UnknownDist
    inverse (DistContainer x) = DistContainer $ inverse x
    inverse (DistDiscrete x) = DistDiscrete $ inverse x

instance Model DistContainerParams DistContainer where
    getparams _ = DistContainerParams

instance DefaultModel DistContainerParams DistContainer where
    defparams = DistContainerParams

instance HomTrainer DistContainerParams DataItem DistContainer where
--     train1dp' DistContainerParams (Gaussian x) = 
    train1dp' DistContainerParams Missing = trace "Distribution.add1sample: Warning, cannot determine which type of distribution to select." UnknownDist
    train1dp' DistContainerParams di@(Discrete x) = DistDiscrete $ train1dp di
    train1dp' DistContainerParams di@(Continuous x) = DistContainer $ train1dp di

-- instance DistributionEstimator DistContainer DataItem where
--     {-# INLINE add1sample #-}
--     add1sample UnknownDist di = 
--         case di of
--              Missing -> trace "Distribution.add1sample: Warning, cannot determine which type of distribution to select." UnknownDist
--              Discrete x -> DistDiscrete $ add1sample (mempty::Categorical DataItem) di
--              Continuous x -> DistContainer $ add1sample (mempty{-:: ContinuousDistribution (Gaussian Double)-}) di
--     add1sample (DistContainer dist) di = DistContainer $ add1sample dist di
--     add1sample (DistDiscrete dist) di = DistDiscrete $ add1sample dist di

instance Distribution DistContainer DataItem Double where

    {-# INLINE pdf #-}
    pdf UnknownDist _ = trace "Distribution.pdf: Warning sampling from an UnkownDist" 0.3
    pdf (DistContainer dist) di = pdf dist di
    pdf (DistDiscrete dist) di = pdf dist di
    
    cdf (DistContainer dist) = cdf dist
    cdf (DistDiscrete dist) = cdf dist
    
    cdfInverse (DistContainer dist) = cdfInverse dist
    cdfInverse (DistDiscrete dist)  = cdfInverse dist
    
{-    drawSample (DistContainer dist) = drawSample dist
    drawSample (DistDiscrete dist) = drawSample dist
    drawSample (UnknownDist) = return Missing-}
--     serializationIndex dist = 0
        
{-instance Binary DistContainer where
    put (UnknownDist) = put (0::Word8)
    put (DistContainer dist) = put (serializationIndex dist) >> put dist
    get = do 
        tag <- getWord8
        case tag of
             0 -> return UnknownDist
             1 -> liftM DistContainer get
             2 -> liftM DistContainer get-}
             
instance NFData DistContainer where
    rnf (UnknownDist) = ()
    rnf (DistContainer dist) = rnf dist
    rnf (DistDiscrete dist) = rnf dist
        

-------------------------------------------------------------------------------
-- DiscretePDF
          
-- data DiscretePDF = DiscretePDF
--         { pdf :: Map.Map (Maybe String) Int
--         } 
--     deriving (Show,Read,Eq)
-- 
-- instance Distribution DiscretePDF DataItem where
--     
-- --     serializationIndex d = 0
--     
--     {-# INLINE add1sample #-}
--     add1sample d Missing = DiscretePDF $ Map.insertWith (+) (Nothing) 1 (pdf d)
--     add1sample d (Discrete x) = DiscretePDF $ Map.insertWith (+) (Just x) 1 (pdf d) -- error "add1sample: cannot add discrete DataItem to (Gaussian Double)"
--     add1sample d (Continuous x) = error "add1sample: cannot add continuous DataItem to DiscretePDF"
-- 
--     {-# INLINE pdf #-}
--     pdf d Missing = getProb Nothing $ pdf d
--     pdf d (Discrete x) = getProb (Just x) $ pdf d--error "pdf: cannot sample a discrete DataItem from a (Gaussian Double)"
--     pdf d (Continuous x) = error "pdf: cannot sample a continuous DataItem from a DiscretePDF"
-- 
-- getProb :: (Maybe String) -> Map.Map (Maybe String) Int -> Probability
-- getProb key pdf = logFloat $ 0.0001+((fi val)/(fi tot)::Double)
--     where
--         val = case Map.lookup key pdf of
--             Nothing -> 0
--             Just x  -> x
--         tot = F.foldl' (+) 0 pdf
-- 
-- instance Semigroup DiscretePDF where
--     (<>) d1 d2 = DiscretePDF $ Map.unionWith (+) (pdf d1) (pdf d2)
--     
-- instance Monoid DiscretePDF where
--     mempty = DiscretePDF mempty
--     mappend = (<>)
-- 
-- instance NFData DiscretePDF where
--     rnf d = rnf $ pdf d


