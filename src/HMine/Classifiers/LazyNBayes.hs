{-# LANGUAGE MultiParamTypeClasses, IncoherentInstances, BangPatterns, FlexibleInstances, UndecidableInstances #-}

module HMine.Classifiers.LazyNBayes
    ( NBayes (..)
    , NBayesParams (..), defNBayesParams
    , file2nbayes, nbayes2file
    )
    where
         
import Control.DeepSeq
import Control.Monad
import Control.Monad.ST.Strict
import Control.Monad.Primitive
import Data.Binary
import Data.List
import Data.List.Extras
import Data.Semigroup
import Data.Number.LogFloat
import Data.STRef
-- import Data.Vector.Binary
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Stream as Stream
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Mutable as VM
import qualified Data.ByteString as BS
import Debug.Trace
-- import qualified Numeric.Algebra as Alg
import System.IO
import Test.QuickCheck

import HMine.Algebra
import HMine.Base
import HMine.Classifiers.TypeClasses
import HMine.DataContainers
import HMine.Distribution
import HMine.MiscUtils

instance (VG.Vector v a, Binary a) => Binary (v a) where
    put v = put $ VG.toList v
    get = liftM VG.fromList get

-------------------------------------------------------------------------------
-- NBayesParams

data NBayesParams = NBayesParams
    deriving (Read,Show,Eq)

instance NFData NBayesParams where
    rnf params = ()

defNBayesParams = NBayesParams

-------------------------------------------------------------------------------
-- NBayes

data NBayes label = NBayes
    { dataDesc :: !(DataDesc label)
    , labelCount :: !(V.Vector Int)
    , attrDist :: !(V.Vector (V.Vector DistContainer)) -- ^ The inner vector corresponds to attributes and the outer vector labels
    }
    deriving (Read,Show,Eq)
    
-- arbitraryNBayes :: DataDesc -> Gen NBayes
-- arbitraryNBayes desc = do
--     let nb = emptyNBayes desc
--     labelCount' = V.mapM (\x -> choose (0,100))
-- --     labelL <- vector (numLabels desc)
-- --     attrL <- vector (numLabels desc) 
-- --         []
--     return nb -- $ NBayes desc (V.fromList labelL) undefined

-- instance Arbitrary NBayes where
--     arbitrary = do
--         labels <- choose (0,100)
--         let desc = DataDesc {numLabels = 5, numAttr = 5}
--         m2 <- choose (0,10000)
--         n <- choose (0,10000)
--         return $ NBayes desc labelV attrV


instance Invertible (NBayes label) where
    inverse nb = nb
        { labelCount = V.map inverse $ labelCount nb
        , attrDist = V.map (V.map inverse) $ attrDist nb
        }

instance (Label label) => Semigroup (NBayes label) where
--     (<>) a NBayesUndefined = a
--     (<>) NBayesUndefined b = b
    (<>) a b =
        if (dataDesc a)/=(dataDesc b)
           then error $ "mappend.NBayes: cannot combine nbayes with different sizes! lhs="++(show $ dataDesc a)++"; rhs="++(show $ dataDesc b)
           else NBayes
                    { dataDesc = dataDesc a
                    , labelCount = V.zipWith (+) (labelCount a) (labelCount b)
                    , attrDist = V.zipWith (V.zipWith mappend) (attrDist a) (attrDist b)
                    }

instance (Binary label) => Binary (NBayes label) where
    put nb = do
        put $ dataDesc nb
        put $ labelCount nb
        put $ attrDist nb
    get = liftM3 NBayes get get get

instance (NFData label) => NFData (NBayes label) where
    rnf nb = seq (rnf $ attrDist nb) $ seq (rnf $ dataDesc nb) (rnf $ labelCount nb)

nbayes2file :: (Label label) => String -> NBayes label -> IO ()
nbayes2file = encodeFile
-- nbayes2file file nb = writeFile file (show nb)

file2nbayes :: (Label label) => String -> IO (NBayes label)
file2nbayes = decodeFile
-- file2nbayes file = liftM read $ readFile file

-------------------------------------------------------------------------------
-- NBayesST

data NBayesST label s = NBayesST
    { dataDescST :: !(DataDesc label)
    , labelCountST :: !(VM.STVector s Int)
    , attrDistST :: !(VM.STVector s (VM.STVector s DistContainer))
    }

instance MutableTrainer NBayesParams (NBayes Int) (NBayesST Int) Int where
--     mkST modelparams desc = thaw $ emptyNBayes desc
    mkST modelparams desc = do
        lc <- VM.replicate (numLabels desc) 0
        ad <- VM.replicateM (numLabels desc) $ VM.replicate (numAttr desc) mempty
        return $ NBayesST
            { dataDescST = desc
            , labelCountST = lc
    --          , attrDist = V.fromList [V.fromList [mempty | y<-[1..numAttr dataDesc]] | x<-[1..numLabels dataDesc]]
            , attrDistST = ad
            }
            
    thaw nb = do
        lc <- V.unsafeThaw $ labelCount nb
--         lc <- V.thaw $ labelCount nb
        ad <- join $ liftM V.unsafeThaw $ vSequence $ V.map V.unsafeThaw $ attrDist nb
--         ad <- join $ liftM V.thaw $ vSequence $ V.map V.thaw $ attrDist nb
        return $ NBayesST
            { dataDescST = dataDesc nb
            , labelCountST = lc
            , attrDistST = ad
            }

    freeze nbst = do
        lc <- V.unsafeFreeze $ labelCountST nbst
--         lc <- V.freeze $ labelCountST nbst
        ad <- V.unsafeFreeze $ attrDistST nbst
        ad2 <- vSequence $ V.map V.unsafeFreeze ad
--         ad <- V.freeze $ attrDistST nbst
--         ad2 <- vSequence $ V.map V.freeze ad
        return $ NBayes
            { dataDesc = dataDescST nbst
            , labelCount = lc
            , attrDist = ad2
            }

    {-# INLINE add1dpST #-}
--     add1dpST :: NBayesST s -> (Int,DataPointSparse) -> ST s (NBayesST s)
    add1dpST nbst (label,dp) = {-trace "add1dpST" $-} do
        
        -- update labelCount    
        oldLabelCount <- VM.read (labelCountST nbst) label
        VM.write (labelCountST nbst) label (oldLabelCount+1)
        
        -- update attrDist
        attrDistLabel <- VM.read (attrDistST nbst) label
        vm_accum add1sample attrDistLabel dp
        
--         trace "added" $ return nbst
        return nbst

-------------------------------------------------------------------------------
-- Training

instance (OnlineTrainer NBayesParams (NBayes label) label) => BatchTrainer NBayesParams (NBayes label) label where
    trainBatch = trainOnline

instance OnlineTrainer NBayesParams (NBayes Int) Int where
    emptyModel desc NBayesParams = emptyNBayes desc

--     add1dp desc NBayesUndefined (label,dp) = add1dp desc (emptyNBayes desc) (label,dp)
    add1dp desc modelparams nb (label,dp) = return $
        nb  { labelCount = (labelCount nb) V.// [(label,(labelCount nb V.! label)+1)]
            , attrDist = (attrDist nb) V.// [(label,newLabelVec)] 
            }
        where
            newLabelVec = V.accum add1sample (attrDist nb V.! label) dp
    

emptyNBayes :: DataDesc label -> NBayes label
emptyNBayes dataDesc = NBayes
    { dataDesc = dataDesc
    , labelCount = V.replicate (numLabels dataDesc) 0
    , attrDist = V.fromList [V.fromList [mempty | y<-[1..numAttr dataDesc]] | x<-[1..numLabels dataDesc]]
    }

unsafeAdd1dp :: NBayes label -> (Int,DPS) -> NBayes label
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

instance Classifier (NBayes Int) Int where
    classify model dp = fst $ argmaxBy compare snd $ probabilityClassify model dp

instance ProbabilityClassifier (NBayes Int) Int where
    probabilityClassify nb dp = probClassifyNB nb dp

probClassifyNB :: NBayes Int -> DPS -> [(Int,Probability)]
probClassifyNB nb dp = {-trace ("length dp="++(show $ length dp)) -} 
    if (V.sum $ labelCount nb) == 0
        then [(i,1/(fromIntegral $ numLabels $ dataDesc nb)) | i<-labelL $ dataDesc nb]
        else {-trace ("V.sum="++show (V.sum $ labelCount nb))-} normedAnswer
    where
        labelProbGivenDp label = {-trace ("labelProbGivenNothing="++(show $ labelProbGivenNothing label)) $ 
                                 trace ("dpProbGivenLabel="++(show $ dpProbGivenLabel label)) $ 
                                 trace ("dpProbGivenLabel==0"++(show $ (dpProbGivenLabel label)==0)) $ 
                                 trace ("labelProbGivenDp==0"++(show $ (labelProbGivenNothing label)*(dpProbGivenLabel label)==0)) $ -}
                                (labelProbGivenNothing label)*(dpProbGivenLabel label)
        labelProbGivenNothing label = logFloat ((fromIntegral $ labelCount nb V.! label)/(fromIntegral $ V.sum $ labelCount nb) :: Double)
        dpProbGivenLabel label = -- trace ("attrProbL "++show label++(show $ attrProbL label)) $
                                foldl (*) (logFloat (1::Double)) (attrProbL label)
        attrProbL label = [ sampleProb (attrDist nb V.! label V.! attrIndex) di {-logFloat (0.3::Double)-} | (attrIndex,di) <- dp]
--         attrProbL label = [ attrProb di (attrDist nb V.! label V.! attrIndex) {-logFloat (0.3::Double)-} | (attrIndex,di) <- dp]

        answer = [ (label, labelProbGivenDp label) | label <- [0..(numLabels $ dataDesc nb)-1]]
        normedAnswer = {-trace "dD" $-} zip [0..] $ normalizeL [labelProbGivenDp label | label <- [0..(numLabels $ dataDesc nb)-1]]

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

-- 
-- testnb = train defNBayesParams td :: NBayes

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
