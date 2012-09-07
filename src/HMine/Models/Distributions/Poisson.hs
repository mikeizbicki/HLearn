{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HMine.Models.Distributions.Poisson
    where

import Control.Monad
import Control.DeepSeq
import Data.Binary
import Data.Hashable
import Data.Semigroup
import Data.Number.LogFloat
import Debug.Trace
import Numeric.SpecFunctions (logFactorial)
import Test.QuickCheck

import Prelude hiding (log)

import HMine.DataContainers
import HMine.Math.Algebra
import HMine.Math.TypeClasses

-------------------------------------------------------------------------------
-- PoissonParams

data PoissonParams

-------------------------------------------------------------------------------
-- Poisson
          
data Poisson = Poisson 
        { m1 :: {-# UNPACK #-} !Double -- often also called lambda
        , n :: {-# UNPACK #-} !Int
        }
    deriving (Show,Read,Eq)

instance Binary Poisson where
    put (Poisson m1 n) = put m1 >> put n
    get = liftM2 Poisson get get
    
instance NFData Poisson where
    rnf (Poisson m1 n) = seq (rnf m1) (rnf n)

-------------------------------------------------------------------------------
-- Training

instance EmptyTrainer PoissonParams Poisson (Maybe Double) where
    emptyModel desc params = Poisson 0 0

instance OnlineTrainer PoissonParams Poisson () (Maybe Double) where
--    add1dp :: DataDesc label -> modelparams -> model -> Labeled datatype label -> HMine model
    add1dp desc modelparams model dp = case fst dp of
        Nothing -> return model
        Just x  -> return $ Poisson m1' n'
            where
                (Poisson m1 n) = model
                m1'=m1+(x-m1)/(fromIntegral n')
                n' =n+1

-------------------------------------------------------------------------------
-- Classification

instance Classifier Poisson (Maybe Double) LogFloat where
    classify (Poisson m1 n) Nothing  = 1
    classify (Poisson m1 n) (Just x) = logToLogFloat $ (fromIntegral k)*(log $ m1) -m1 - (logFactorial k)
        where
            k = round x

-- -- instance Distribution Poisson where
-- --     
-- --     serializationIndex p = 2
-- --     
-- --     {-# INLINE add1sample #-}
-- --     add1sample p Missing = p
-- --     add1sample p (Discrete x) = error "add1sample: cannot add discrete DataItem to Poisson"
-- --     add1sample 
-- 
--     {-# INLINE sampleProb #-}
--     sampleProb p Missing = 1
--     sampleProb p (Discrete x) = error "sampleProb: cannot sample a discrete DataItem from a Gaussian"
--     sampleProb p@(Poisson m1 n) (Continuous x) = {-trace ("k="++show k++", m1="++show m1++", n="++show n) $ -}logToLogFloat $ (fromIntegral k)*(log $ m1) -m1 - (logFactorial k)
--         where
--             k = round x

-------------------------------------------------------------------------------
-- Algebra

instance Invertible Poisson where
    inverse (Poisson m1 n) = Poisson m1 (-n)

instance Semigroup Poisson where
    (<>) p1@(Poisson m1a na) p2@(Poisson m1b nb) = Poisson m1' n'
        where
            m1' = m1a*(fromIntegral na/fromIntegral n')+m1b*(fromIntegral nb/fromIntegral n')
            n'  = na+nb
            