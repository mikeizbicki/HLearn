{-# LANGUAGE MultiParamTypeClasses, IncoherentInstances, BangPatterns #-}

module LazyNBayes
    where
         
import ClassificationAlgebra
import Base
         
import Control.DeepSeq
import Control.Monad
import Control.Monad.ST.Strict
import Control.Monad.Primitive
import Data.Binary
import Data.List
-- import Data.Monoid
import Data.Semigroup
import Data.Number.LogFloat
import Data.STRef
import Data.Vector.Binary
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Stream as Stream
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Mutable as VM
import qualified Data.ByteString as BS
import qualified Data.Primitive.Array as PA
import Debug.Trace
import System.IO

import Distribution
import MiscUtils


-------------------------------------------------------------------------------
-- data types

data NBayesST s = NBayesST
    { dataDescST :: !DataDesc
    , labelCountST :: !(VM.STVector s Int)
    , attrDistST :: !(VM.STVector s (VM.STVector s DistContainer))
    }

instance MutableTrainer NBayes NBayesST Int where
    mkST desc = thaw $ emptyNBayes desc
    
    thaw nb = do
        lc <- V.thaw $ labelCount nb
        ad <- join $ liftM V.thaw $ vSequence $ V.map V.thaw $ attrDist nb
        return $ NBayesST
            { dataDescST = dataDesc nb
            , labelCountST = lc
            , attrDistST = ad
            }

    freeze nbst = do
        lc <- V.freeze $ labelCountST nbst
        ad <- V.freeze $ attrDistST nbst
        ad2 <- vSequence $ V.map V.freeze ad
        return $ NBayes
            { dataDesc = dataDescST nbst
            , labelCount = lc
            , attrDist = ad2
            }

--     add1dpST :: NBayesST s -> (Int,DataPointSparse) -> ST s (NBayesST s)
    add1dpST nbst (label,dp) = do
        trace "add1dpST" $ return ()
        
        -- update labelCount    
        oldLabelCount <- VM.read (labelCountST nbst) label
        VM.write (labelCountST nbst) label (oldLabelCount+1)
        
        -- update attrDist
        attrDistLabel <- VM.read (attrDistST nbst) label
        vm_accum add1sample attrDistLabel dp
        
        return nbst
    
data NBayes = NBayesUndefined
            | NBayes { dataDesc :: !DataDesc
                     , labelCount :: !(V.Vector Int)
                     , attrDist :: !(V.Vector (V.Vector DistContainer))
                     }
    deriving (Read,Show)

instance Semigroup NBayes where
    (<>) a NBayesUndefined = a
    (<>) NBayesUndefined b = b
    (<>) a b =
        if (dataDesc a)/=(dataDesc b)
           then error "mappend.NBayes: cannot combine nbayes with different sizes!"
           else NBayes
                    { dataDesc = dataDesc a
                    , labelCount = V.zipWith (+) (labelCount a) (labelCount b)
                    , attrDist = V.zipWith (V.zipWith mappend) (attrDist a) (attrDist b)
                    }

instance Monoid NBayes where
    mempty = NBayesUndefined
    mappend = (<>)

instance CommutativeMonoid NBayes

instance Binary NBayes where
    put nb = do
        put $ dataDesc nb
        put $ labelCount nb
        put $ attrDist nb
    get = liftM3 NBayes get get get

instance NFData NBayes where
    rnf nb = seq (rnf $ attrDist nb) $ seq (rnf $ dataDesc nb) (rnf $ labelCount nb)

nbayes2file :: String -> NBayes -> IO ()
nbayes2file = encodeFile
-- nbayes2file file nb = writeFile file (show nb)

file2nbayes :: String -> IO (NBayes)
file2nbayes = decodeFile
-- file2nbayes file = liftM read $ readFile file

-------------------------------------------------------------------------------
-- Training

instance OnlineTrainer NBayes Int where
    emptyModel = NBayesUndefined

    add1dp desc NBayesUndefined (label,dp) = add1dp desc (emptyNBayes desc) (label,dp)
    add1dp desc nb (label,dp) = 
        nb  { labelCount = (labelCount nb) V.// [(label,(labelCount nb V.! label)+1)]
            , attrDist = (attrDist nb) V.// [(label,newLabelVec)] 
            }
        where
            newLabelVec = V.accum add1sample (attrDist nb V.! label) dp
    

emptyNBayes :: DataDesc -> NBayes
emptyNBayes dataDesc = NBayes
    { dataDesc = dataDesc
    , labelCount = V.replicate (numLabels dataDesc) 0
    , attrDist = V.fromList [V.fromList [mempty | y<-[1..numAttr dataDesc]] | x<-[1..numLabels dataDesc]]
    }

unsafeAdd1dp :: NBayes -> (Int,DataPointSparse) -> NBayes
unsafeAdd1dp nb (label,dp) = runST $ do
    lc <- V.unsafeThaw $ labelCount nb
    ad <- join $ liftM V.unsafeThaw $ vSequence $ V.map V.unsafeThaw $ attrDist nb

    -- update labelCount    
    oldLabelCount <- VM.read lc label
    VM.write lc label (oldLabelCount+1)
    
    -- update attrDist
    attrDistLabel <- VM.read ad label
    vm_accum add1sample attrDistLabel dp
--     VGM.accum (\x y -> x) attrDistLabel $ Stream.fromList dp
--     VGM.accum add1sample attrDistLabel $ Stream.fromList dp

    lc <- V.unsafeFreeze lc
    ad <- V.unsafeFreeze ad
    ad2 <- vSequence $ V.map V.unsafeFreeze ad

    return nb

-------------------------------------------------------------------------------
-- Classification

probClassifyNB :: NBayes -> DataPointSparse -> [(Int,Probability)]
probClassifyNB nb dp = trace ("length dp="++(show $ length dp)) normedAnswer
    where
        labelProbGivenDp label = {-trace ("labelProbGivenNothing="++(show $ labelProbGivenNothing label)) $ 
                                 trace ("dpProbGivenLabel="++(show $ dpProbGivenLabel label)) $ 
                                 trace ("dpProbGivenLabel==0"++(show $ (dpProbGivenLabel label)==0)) $ 
                                 trace ("labelProbGivenDp==0"++(show $ (labelProbGivenNothing label)*(dpProbGivenLabel label)==0)) $ -}
                                (labelProbGivenNothing label)*(dpProbGivenLabel label)
        labelProbGivenNothing label = logFloat ((fromIntegral $ labelCount nb V.! label)/(fromIntegral $ V.sum $ labelCount nb) :: Double)
        dpProbGivenLabel label = -- trace ("attrProbL "++show label++(show $ attrProbL label)) $
                                foldl (*) (logFloat (1::Double)) (attrProbL label)
        attrProbL label = [ sampleProb (attrDist nb V.! label V.! attrIndex) di  {-logFloat (0.3::Double)-} | (attrIndex,di) <- dp]
--         attrProbL label = [ attrProb di (attrDist nb V.! label V.! attrIndex) {-logFloat (0.3::Double)-} | (attrIndex,di) <- dp]

        answer = [ (label, labelProbGivenDp label) | label <- [0..(numLabels $ dataDesc nb)-1]]
        normedAnswer = zip [0..] $ normalizeL [labelProbGivenDp label | label <- [0..(numLabels $ dataDesc nb)-1]]

-- attrProb :: DataItem -> Gaussian -> Probability
-- attrProb Missing _ = 1
-- attrProb (Discrete _) _ = error "cannot classify discrete attributes yet."
-- -- attrProb (Continuous x) (UnknownDist) = 0.001
-- attrProb (Continuous x) (Gaussian m s k) = logFloat $ 
--         if s>0
--            then {-prob-} if prob < 0.001
--                    then            0.001
--                    else prob 
--            else 0.1
--     where
--         prob = 1/(sqrt $ 2*pi*var) * (exp $ -(x-m)^2/(2*var))
--         var = s/(k-1)

-------------------------------------------------------------------------------
-- testing

desc = DataDesc 2 3
dp 0 = (0::Int,[(0::Int,Continuous 6)   ,(1::Int,Continuous 180),(2::Int,Continuous 12)])
dp 1 = (0::Int,[(0::Int,Continuous 5.92),(1::Int,Continuous 190),(2::Int,Continuous 11)])
dp 2 = (0::Int,[(0::Int,Continuous 5.58),(1::Int,Continuous 170),(2::Int,Continuous 12)])
dp 3 = (0::Int,[(0::Int,Continuous 5.92),(1::Int,Continuous 165),(2::Int,Continuous 10)])
dp 4 = (1::Int,[(0::Int,Continuous 5)   ,(1::Int,Continuous 100),(2::Int,Continuous 6)])
dp 5 = (1::Int,[(0::Int,Continuous 5.5) ,(1::Int,Continuous 150),(2::Int,Continuous 8)])
dp 6 = (1::Int,[(0::Int,Continuous 5.42),(1::Int,Continuous 130),(2::Int,Continuous 7)])
dp 7 = (1::Int,[(0::Int,Continuous 5.75),(1::Int,Continuous 150),(2::Int,Continuous 9)])

sample = [(0::Int,Continuous 6),(1::Int,Continuous 130),(2::Int,Continuous 8)]

td = map dp [0..7]

testnb = train desc td :: NBayes

-- testwriter = do
--     encodeFile "testwriter.nbayes" testnb
--     testnb' <- decodeFile "testwriter.nbayes"
--     print (testnb==testnb')

-- testthaw = runST $ do
--     trace "test1" $ return ()
--     nbst <- thaw $ {-testnb -- -}emptyNBayes $ DataDesc 100 50000 :: ST s (NBayesST s)
--     trace "test2" $ return ()
--     freeze nbst :: ST s (NBayes)
--     trace "test3" $ return ()
