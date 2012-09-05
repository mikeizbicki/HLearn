{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

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
import Test.QuickCheck

import qualified Data.Foldable as F
import qualified Data.Map as Map

import Prelude hiding (log)

import HMine.Base hiding (mean,stddev,var)
import HMine.DataContainers
import HMine.Math.Algebra
import HMine.Math.TypeClasses

-------------------------------------------------------------------------------
-- Distribution
          
class Distribution dist where
    add1sample :: dist -> DataItem -> dist
    sampleProb :: dist -> DataItem -> LogFloat
    
    serializationIndex :: dist -> Word8
       
class 
    ( OnlineTrainer distparams dist (Maybe Double) Double
    , Classifier dist (Maybe Double) Double
    ) => 
    RealDistribution distparams dist where

       
-------------------------------------------------------------------------------
-- DistContainer
   
data DistContainer = UnknownDist
                   | DistContainer Gaussian
                   | DistDiscrete DiscretePDF
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

instance Distribution DistContainer where
    {-# INLINE add1sample #-}
--     add1sample UnknownDist di = UnknownDist
    add1sample UnknownDist di = 
        case di of
             Missing -> trace "Distribution.add1sample: Warning, cannot determine which type of distribution to select." UnknownDist
             Discrete x -> DistDiscrete $ add1sample (mempty::DiscretePDF) di
             Continuous x -> DistContainer $ add1sample (mempty::Gaussian) di
    add1sample (DistContainer dist) di = DistContainer $ add1sample dist di
    add1sample (DistDiscrete dist) di = DistDiscrete $ add1sample dist di

    {-# INLINE sampleProb #-}
    sampleProb UnknownDist _ = trace "Distribution.sampleProb: Warning sampling from an UnkownDist" 0.3
    sampleProb (DistContainer dist) di = sampleProb dist di
    sampleProb (DistDiscrete dist) di = sampleProb dist di
    
    serializationIndex dist = 0
        
instance Binary DistContainer where
    put (UnknownDist) = put (0::Word8)
    put (DistContainer dist) = put (serializationIndex dist) >> put dist
    get = do 
        tag <- getWord8
        case tag of
             0 -> return UnknownDist
             1 -> liftM DistContainer get
             2 -> liftM DistContainer get
             
instance NFData DistContainer where
    rnf (UnknownDist) = ()
    rnf (DistContainer dist) = rnf dist
    rnf (DistDiscrete dist) = rnf dist
        
-------------------------------------------------------------------------------
-- Gaussian
          
data GaussianParams = GaussianParams
          
data Gaussian = Gaussian 
        { m1 :: {-# UNPACK #-} !Double
        , m2 :: {-# UNPACK #-} !Double
        , n :: {-# UNPACK #-} !Int
        } 
    deriving (Show,Read)

instance Eq Gaussian where
    (==) (Gaussian m1a m2a na) (Gaussian m1b m2b nb) = 
        ((m1a==0 && m1b==0) || (abs $ m1a-m1b)/(m1a+m1b) < 1e-10) &&
        ((m2a==0 && m2b==0) || (abs $ m2a-m2b)/(m2a+m2b) < 1e-10) &&
        na==nb

instance Arbitrary Gaussian where
    arbitrary = do
        m1 <- choose (0,10000)
        m2 <- choose (0,10000)
        n <- choose (0,10000)
        return $ Gaussian m1 m2 n

instance EmptyTrainer GaussianParams Gaussian (Maybe Double) where
    emptyModel desc params = mempty

instance OnlineTrainer GaussianParams Gaussian () (Maybe Double) where
--    add1dp :: DataDesc label -> modelparams -> model -> Labeled datatype label -> HMine model
    add1dp desc modelparams model dp = return $ add1sample model di
        where 
            di = case fst dp of
                Nothing -> Missing
                Just x -> Continuous x

instance Distribution Gaussian where
    
    serializationIndex g = 1
    
    {-# INLINE add1sample #-}
    add1sample g Missing = g
    add1sample g (Discrete x) = error "add1sample: cannot add discrete DataItem to Gaussian"
    add1sample (Gaussian m1 m2 n) (Continuous x) = Gaussian m1' m2' n'
        where
            xf={-double2Float-} x
            m1'=m1+(xf-m1)/(fromIntegral n')
            m2'=m2+(xf-m1)*(xf-m1')
            n' =n+1

    {-# INLINE sampleProb #-}
    sampleProb g Missing = 1
    sampleProb g (Discrete x) = error "sampleProb: cannot sample a discrete DataItem from a Gaussian"
--     sampleProb g (Continuous x) = trace ("x="++show x++"; m="++show m++"; v="++show v++"; ret="++show ret) $ {-logFloat -}ret
    sampleProb g (Continuous x) = ret
        where
--             ret = (logFloat $ 1/(sqrt $ 2*pi*v)) * (exp $ -(x-m)^2/(2*v))
            ret = (logFloat $ 1/(sqrt $ 2*pi*v)) * (logToLogFloat $ -(x-m)^2/(2*v))
            m = mean g
            v = varianceSample g
        
mean :: Gaussian -> Double
mean (Gaussian m1 m2 n) = {-float2Double-} m1

varianceSample :: Gaussian -> Double
varianceSample (Gaussian m1 m2 n) = {-trace ("n="++show n) $-} {-float2Double $-} 
    if m2==0
       then abs $ (max m1 1)/(fromIntegral n)
       else m2/(fromIntegral $ n-1)

-- variancePop :: Gaussian -> Double
-- variancePop (Gaussian m1 m2 n) = {-float2Double $-} m2/(fromIntegral n)

instance Invertible Gaussian where
    inverse (Gaussian m1 m2 n) = Gaussian m1 (-m2) (-n)
--     inverse g = subG mempty g
--         where
--             subG (Gaussian m1' m2' n') (Gaussian m1a m2a na) = Gaussian m1b m2b nb
--                 where
--                     m1b=(m1'*(fromIntegral n'))/(fromIntegral nb) - (m1a*(fromIntegral na))/(fromIntegral nb)
--                     m2b=(-1)*(m2a-m2'+(fromIntegral $ na*nb)/(fromIntegral n')*(m1a-m1b)^2)
--                     nb=n'-na

instance Semigroup Gaussian where
    (<>) = mappend

instance Monoid Gaussian where
    mempty = Gaussian 0 0 0
    mappend g1@(Gaussian m1a m2a na) g2@(Gaussian m1b m2b nb) = 
        if n'==0
           then mempty
           else Gaussian m1' m2' n'
        where
            m1' = m1a*(fromIntegral na/fromIntegral n')+m1b*(fromIntegral nb/fromIntegral n')
            m2' = m2a+m2b+(fromIntegral $ na*nb)/(fromIntegral n')*(m1a-m1b)^2
            n'  = na+nb
                        
x=foldl (add1sample) mempty $ map Continuous [1..10] :: Gaussian
y=foldl (add1sample) mempty $ map Continuous [1..6] :: Gaussian
z=foldl (add1sample) mempty $ map Continuous [7..10] :: Gaussian
                        
instance Binary Gaussian where
    put (Gaussian m1 m2 n) = put m1 >> put m2 >> put n
    get = liftM3 Gaussian get get get
    
instance NFData Gaussian where
    rnf (Gaussian m1 m2 n) = seq (rnf m1) $ seq (rnf m2) (rnf n)

---------------------------------------                                        -------------------------------------------------------------------------------
-- Gaussian
          
data DiscretePDF = DiscretePDF
        { pdf :: Map.Map (Maybe String) Int
        } 
    deriving (Show,Read,Eq)

instance Distribution DiscretePDF where
    
    serializationIndex d = 0
    
    {-# INLINE add1sample #-}
    add1sample d Missing = DiscretePDF $ Map.insertWith (+) (Nothing) 1 (pdf d)
    add1sample d (Discrete x) = DiscretePDF $ Map.insertWith (+) (Just x) 1 (pdf d) -- error "add1sample: cannot add discrete DataItem to Gaussian"
    add1sample d (Continuous x) = error "add1sample: cannot add continuous DataItem to DiscretePDF"

    {-# INLINE sampleProb #-}
    sampleProb d Missing = getProb Nothing $ pdf d
    sampleProb d (Discrete x) = getProb (Just x) $ pdf d--error "sampleProb: cannot sample a discrete DataItem from a Gaussian"
    sampleProb d (Continuous x) = error "sampleProb: cannot sample a continuous DataItem from a DiscretePDF"

getProb :: (Maybe String) -> Map.Map (Maybe String) Int -> Probability
getProb key pdf = logFloat $ 0.0001+((fi val)/(fi tot)::Double)
    where
        val = case Map.lookup key pdf of
            Nothing -> 0
            Just x  -> x
        tot = F.foldl' (+) 0 pdf

instance Semigroup DiscretePDF where
    (<>) d1 d2 = DiscretePDF $ Map.unionWith (+) (pdf d1) (pdf d2)
    
instance Monoid DiscretePDF where
    mempty = DiscretePDF mempty
    mappend = (<>)

instance NFData DiscretePDF where
    rnf d = rnf $ pdf d

-- data ScaledGaussian = ScaledGaussian
--     { s_m1 :: Float
--     , s_m2 :: Float
--     , s_n :: Float
--     , scale :: Float
--     }
--     deriving (Read,Show)
-- 
-- instance Distribution ScaledGaussian where
--     
--     serializationIndex g = 1
--     
--     add1sample g Missing = g
--     add1sample g (Discrete x) = error "add1sample: cannot add discrete DataItem to Gaussian"
--     add1sample (ScaledGaussian m1 m2 n s) (Continuous x) = ScaledGaussian m1' m2' n' s
--         where
--             xf=double2Float x
--             m1'=m1+(xf-m1)/(n')
--             m2'=m2+(xf-m1)*(xf-m1')
--             n' =n+1
-- 
--     sampleProb g Missing = 1
--     sampleProb g (Discrete x) = error "sampleProb: cannot sample a discrete DataItem from a Gaussian"
--     sampleProb g (Continuous x) = logFloat $ 1/(sqrt $ 2*pi*v) * (exp $ -(x-m)^2/(2*v))
--         where
--             m = meanScaled g
--             v = varianceSampleScaled g
-- 
-- meanScaled :: ScaledGaussian -> Double
-- meanScaled (ScaledGaussian m1 m2 ns s) = float2Double m1
-- 
-- varianceSampleScaled :: ScaledGaussian -> Double
-- varianceSampleScaled (ScaledGaussian m1 m2 n s) = float2Double $ m2/(n-1)
-- 
-- instance AdditiveGroup ScaledGaussian where
--     zeroV = ScaledGaussian 0 0 0 1
--     (^+^) g1@(ScaledGaussian m1a m2a na sa) g2@(ScaledGaussian m1b m2b nb sb) = ScaledGaussian m1' m2' n' s'
--         where
--             na' = (na)*(sa{-/(sa+sb)-})
--             nb' = (na)*(sb{-/(sa+sb)-})
--             m1' = m1a*(na'/n')
--                 + m1b*(nb'/n')
--             m2' = m2a+m2b+(na'*nb')/(n')*(m1a-m1b)^2
--             n'  = na'{-*sa-}+nb'{-*sb-}
--             s'  = 1 -- (na*sa+nb*sb)/(na+nb)
--     negateV = undefined
-- 
-- instance Monoid ScaledGaussian where
--     mempty = zeroV
--     mappend = (^+^)
-- 
-- instance VectorSpace ScaledGaussian where
--     type Scalar ScaledGaussian = Float
-- --     (*^) a v = v { scale = a*(scale v) }
--     (*^) a v = v { s_n = a*(s_n v) }
-- 
-- f1=foldl add1sample (mempty::Gaussian) (map Continuous [1,2,3])
-- f2=foldl add1sample (mempty::Gaussian) (map Continuous [2,2,2])
-- g1=foldl add1sample (zeroV::ScaledGaussian) (map Continuous [1,2,3])
-- g2=foldl add1sample (zeroV::ScaledGaussian) (map Continuous [2,2,2])

-------------------------------------------------------------------------------
-- Poisson
          
data Poisson = Poisson 
        { poisson_m1 :: {-# UNPACK #-} !Double -- often also called lambda
        , poisson_n :: {-# UNPACK #-} !Int
        }
    deriving (Show,Read,Eq)

instance Invertible Poisson where
    inverse (Poisson m1 n) = Poisson m1 (-n)

instance Distribution Poisson where
    
    serializationIndex p = 2
    
    {-# INLINE add1sample #-}
    add1sample p Missing = p
    add1sample p (Discrete x) = error "add1sample: cannot add discrete DataItem to Poisson"
    add1sample (Poisson m1 n) (Continuous x) = Poisson m1' n'
        where
            xf =x
            m1'=m1+(xf-m1)/(fromIntegral n')
            n' =n+1

    {-# INLINE sampleProb #-}
    sampleProb p Missing = 1
    sampleProb p (Discrete x) = error "sampleProb: cannot sample a discrete DataItem from a Gaussian"
    sampleProb p@(Poisson m1 n) (Continuous x) = {-trace ("k="++show k++", m1="++show m1++", n="++show n) $ -}logToLogFloat $ (fromIntegral k)*(log $ m1) -m1 - (logFactorial k)
        where
            k = round x
        
instance Monoid Poisson where
    mempty = Poisson 0 0
    mappend p1@(Poisson m1a na) p2@(Poisson m1b nb) = Poisson m1' n'
        where
            m1' = m1a*(fromIntegral na/fromIntegral n')+m1b*(fromIntegral nb/fromIntegral n')
            n'  = na+nb
            
instance Binary Poisson where
    put (Poisson m1 n) = put m1 >> put n
    get = liftM2 Poisson get get
    
instance NFData Poisson where
    rnf (Poisson m1 n) = seq (rnf m1) (rnf n)
