{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module HMine.Distribution
    where

import Control.DeepSeq
import Control.Monad
import Data.Binary
import Data.Semigroup
import Data.Number.LogFloat
import Data.VectorSpace
import Debug.Trace
import GHC.Float (double2Float, float2Double)
import Numeric.SpecFunctions (logFactorial)
import Test.QuickCheck hiding (classify)

import qualified Data.Foldable as F
import qualified Data.Map as Map

import Prelude hiding (log)

import HMine.Base hiding (mean,stddev,var)
import HMine.DataContainers
import HMine.Math.Algebra
import HMine.Math.TypeClasses
import HMine.Models.Distributions.Dirichlet
import HMine.Models.Distributions.Gaussian
import HMine.Models.Distributions.Poisson

-------------------------------------------------------------------------------

instance (Distribution dist Double) => Distribution dist (Maybe Double) where
    add1sample dist dp = case dp of
        Nothing -> dist
        Just x  -> add1sample dist x

    sampleProb dist dp = case dp of
        Nothing -> 1
        Just x  -> sampleProb dist x
    
instance {-(Distribution dist (Maybe Double)) => -}Distribution Gaussian DataItem where
    add1sample dist dp = case dp of
        Missing      -> add1sample dist (Nothing :: Maybe Double)
        Continuous x -> add1sample dist $ Just x
        Discrete   x -> error "Gaussian.add1sample: cannot add discrete"
        
    sampleProb dist dp = case dp of
        Missing      -> sampleProb dist (Nothing :: Maybe Double)
        Continuous x -> sampleProb dist $ Just x
        Discrete   x -> error "Gaussian.sampleProb: cannot sample discrete"
        

{-instance 
    ( OnlineTrainer GaussianParams Gaussian () (Maybe Double)
    , Classifier Gaussian (Maybe Double) LogFloat
    ) => 
    Distribution Gaussian DataItem 
        where
    add1sample dist Missing        = runHMine 10 $ add1dp undefined GaussianParams dist (Nothing,())
    add1sample dist (Continuous x) = runHMine 10 $ add1dp undefined GaussianParams dist (Just x,())
    add1sample dist (Discrete x)   = error "Cannot insert discrete items into continuous distribution"
    
    sampleProb dist Missing        = classify dist (Nothing :: Maybe Double)
    sampleProb dist (Continuous x) = classify dist $ Just x
    sampleProb dist (Discrete x)   = error "Cannot classify discrete items from continuous distribution"-}
    
--     serializationIndex x = 1
    
-------------------------------------------------------------------------------
-- DistContainer
   
data DistContainer = UnknownDist
                   | DistContainer Gaussian
                   | DistDiscrete (Dirichlet DataItem)
--                    | DistContainer Poisson
    deriving (Show,Read,Eq)

instance Monoid DistContainer where
    mempty = UnknownDist
    mappend UnknownDist b = b
    mappend a UnknownDist = a
    mappend (DistContainer a) (DistContainer b) = DistContainer $ mappend a b
    mappend (DistDiscrete a) (DistDiscrete b) = DistDiscrete $ mappend a b -- error "DistContiner.mappend (DistDiscrete) not yet implemented"

instance Invertible DistContainer where
    inverse UnknownDist = UnknownDist
    inverse (DistContainer x) = DistContainer $ inverse x

instance Distribution DistContainer DataItem where
    {-# INLINE add1sample #-}
--     add1sample UnknownDist di = UnknownDist
    add1sample UnknownDist di = 
        case di of
             Missing -> trace "Distribution.add1sample: Warning, cannot determine which type of distribution to select." UnknownDist
             Discrete x -> DistDiscrete $ add1sample (mempty::Dirichlet DataItem) di
             Continuous x -> DistContainer $ add1sample (mempty::Gaussian) di
    add1sample (DistContainer dist) di = DistContainer $ add1sample dist di
    add1sample (DistDiscrete dist) di = DistDiscrete $ add1sample dist di

    {-# INLINE sampleProb #-}
    sampleProb UnknownDist _ = trace "Distribution.sampleProb: Warning sampling from an UnkownDist" 0.3
    sampleProb (DistContainer dist) di = sampleProb dist di
    sampleProb (DistDiscrete dist) di = sampleProb dist di
    
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
--     add1sample d (Discrete x) = DiscretePDF $ Map.insertWith (+) (Just x) 1 (pdf d) -- error "add1sample: cannot add discrete DataItem to Gaussian"
--     add1sample d (Continuous x) = error "add1sample: cannot add continuous DataItem to DiscretePDF"
-- 
--     {-# INLINE sampleProb #-}
--     sampleProb d Missing = getProb Nothing $ pdf d
--     sampleProb d (Discrete x) = getProb (Just x) $ pdf d--error "sampleProb: cannot sample a discrete DataItem from a Gaussian"
--     sampleProb d (Continuous x) = error "sampleProb: cannot sample a continuous DataItem from a DiscretePDF"
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


