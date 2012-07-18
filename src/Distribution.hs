module Distribution
    where

import Control.DeepSeq
import Control.Monad
import Data.Binary
import Data.Monoid
import Data.Number.LogFloat
import Debug.Trace
import GHC.Float (double2Float, float2Double)
import Numeric.SpecFunctions

import Prelude hiding (log)

import Base

-------------------------------------------------------------------------------
-- Distribution
          
class (Monoid dist) => Distribution dist where
    oneSample :: DataItem -> dist
    oneSample x = add1sample mempty x
    
    add1sample :: dist -> DataItem -> dist
    sampleProb :: dist -> DataItem -> LogFloat
    
    serializationIndex :: dist -> Word8
       
-------------------------------------------------------------------------------
-- DistContainer
   
data DistContainer = UnknownDist
                   | DistContainer Poisson
    deriving (Show,Read)

instance Monoid DistContainer where
    mempty = UnknownDist
    mappend UnknownDist b = b
    mappend a UnknownDist = a
    mappend (DistContainer a) (DistContainer b) = DistContainer $ mappend a b

instance Distribution DistContainer where
    add1sample UnknownDist di = DistContainer $ add1sample (mempty{-::Gaussian-}) di
    add1sample (DistContainer dist) di = DistContainer $ add1sample dist di

    sampleProb (DistContainer dist) di = sampleProb dist di
    
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
        
-------------------------------------------------------------------------------
-- Gaussian
          
data Gaussian = Gaussian 
        { m1 :: {-# UNPACK #-} !Float
        , m2 :: {-# UNPACK #-} !Float
        , n :: {-# UNPACK #-} !Int
        }
    deriving (Show,Read)
        
instance Distribution Gaussian where
    
    serializationIndex g = 1
    
    add1sample g Missing = g
    add1sample g (Discrete x) = error "add1sample: cannot add discrete DataItem to Gaussian"
    add1sample (Gaussian m1 m2 n) (Continuous x) = Gaussian m1' m2' n'
        where
            xf=double2Float x
            m1'=m1+(xf-m1)/(fromIntegral n')
            m2'=m2+(xf-m1)*(xf-m1')
            n' =n+1

    sampleProb g Missing = 1
    sampleProb g (Discrete x) = error "sampleProb: cannot sample a discrete DataItem from a Gaussian"
    sampleProb g (Continuous x) = logFloat $ 1/(sqrt $ 2*pi*v) * (exp $ -(x-m)^2/(2*v))
        where
            m = mean g
            v = varianceSample g
        
mean :: Gaussian -> Double
mean (Gaussian m1 m2 n) = float2Double m1

varianceSample :: Gaussian -> Double
varianceSample (Gaussian m1 m2 n) = float2Double $ m2/(fromIntegral $ n-1)

variancePop :: Gaussian -> Double
variancePop (Gaussian m1 m2 n) = float2Double $ m2/(fromIntegral n)

instance Monoid Gaussian where
    mempty = Gaussian 0 0 0
    mappend g1@(Gaussian m1a m2a na) g2@(Gaussian m1b m2b nb) = Gaussian m1' m2' n'
        where
            m1' = m1a*(fromIntegral na/fromIntegral n')+m1b*(fromIntegral nb/fromIntegral n')
            m2' = m2a+m2b+(fromIntegral $ na*nb)/(fromIntegral n')*(m1a-m1b)^2
            n'  = na+nb
            
instance Binary Gaussian where
    put (Gaussian m1 m2 n) = put m1 >> put m2 >> put n
    get = liftM3 Gaussian get get get
    
instance NFData Gaussian where
    rnf (Gaussian m1 m2 n) = seq (rnf m1) $ seq (rnf m2) (rnf n)

-------------------------------------------------------------------------------
-- Gaussian
          
data Poisson = Poisson 
        { poisson_m1 :: {-# UNPACK #-} !Double -- often also called lambda
        , poisson_n :: {-# UNPACK #-} !Int
        }
    deriving (Show,Read)
        
instance Distribution Poisson where
    
    serializationIndex p = 2
    
    add1sample p Missing = p
    add1sample p (Discrete x) = error "add1sample: cannot add discrete DataItem to Poisson"
    add1sample (Poisson m1 n) (Continuous x) = Poisson m1' n'
        where
            xf =x
            m1'=m1+(xf-m1)/(fromIntegral n')
            n' =n+1

    sampleProb p Missing = 1
    sampleProb p (Discrete x) = error "sampleProb: cannot sample a discrete DataItem from a Gaussian"
--     sampleProb p@(Poisson m1 n) (Continuous x) = let y=(logToLogFloat $ ((fromIntegral k)*(log $ m1)+(-m1)) - (logFactorial k)) in trace ("y="++show y++"; k="++show k++"; p="++show p) y
    sampleProb p@(Poisson m1 n) (Continuous x) = logToLogFloat $ (fromIntegral k)*(log $ m1) -m1 - (logFactorial k)
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
