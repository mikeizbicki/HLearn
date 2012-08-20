module LazyNBayes
    where
         
import AI.Classification

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.Binary
import Data.List
import Data.STRef
import qualified Data.Array.IArray as A
-- import qualified Data.Vector.Mutable as VM
import qualified Data.ByteString as BS
import System.IO

-------------

type TrainingDataSparse = [(Int,DataPointSparse)]
type DataPointSparse = [(Int,DataItem)]

data DataDesc = DataDesc
    { numLabels :: Int
    , numAttr :: Int
    }
    deriving (Eq,Read,Show)

instance Binary DataDesc where
    put desc = do
        put $ numLabels desc
        put $ numAttr desc
    get = liftM2 DataDesc get get

instance NFData DataDesc where
    rnf desc = seq (rnf $ numLabels desc) (rnf $ numAttr desc)

-------------------------------------------------------------------------------
-- data types

{-data NBayesST s = NBayesST
    { dataDescST :: DataDesc
    , labelCountST :: VM.STVector s Int
    , attrDistST :: VM.STVector s (VM.STVector s NBayesDistribution)
    }

thawNBayes :: NBayes -> ST s (NBayesST s)
thawNBayes nb = do
    lc <- V.thaw $ labelCount nb
--     ad <- liftM V.thaw $ vSequence $ V.map V.thaw $ attrDist nb
--     ad <- V.thaw $ V.map V.thaw $ attrDist nb
    return $ NBayesST
        { dataDescST = dataDesc nb
        , labelCountST = lc
--         , attrDistST = ad
        }-}
            
data NBayes = NBayes
    { dataDesc :: DataDesc
    , labelCount :: A.Array Int Int
    , attrDist :: A.Array (Int,Int) NBayesDistribution
    }
    deriving (Eq,Read,Show)

instance Binary NBayes where
    put nb = do
        put $ dataDesc nb
        Data.Binary.put $ labelCount nb
--         put $ attrDist nb
--     get = liftM3 NBayes get get get

instance NFData NBayes where
    rnf nb = seq (rnf $ attrDist nb) $ seq (rnf $ dataDesc nb) (rnf $ labelCount nb)

-- instance (NFData b) => NFData (A.Array b) where
-- --     rnf x = rnf (bounds x, Data.Array.elems x)
-- --     rnf vec = Data.Vector.map rnf vec `seq` ()
--     rnf vec = rnf $ V.toList vec

-- instance NFData a => NFData (A.Array a) where
--   rnf v = A.foldl' (\x y -> y `deepseq` x) () v

data NBayesDistribution 
    = UnknownDist
    | Gaussian 
        { m :: Double
        , s :: Double
        , k :: Double
        }
    deriving (Eq,Show,Read)

instance Binary NBayesDistribution where
    put UnknownDist = put (0 :: Word8)
    put (Gaussian m s k) = put (1 :: Word8) >> put m >> put s >> put k
    get = do 
        tag <- getWord8
        case tag of
             0 -> return UnknownDist
             1 -> liftM3 Gaussian get get get

instance NFData NBayesDistribution where
    rnf UnknownDist = ()
    rnf (Gaussian m s k) = seq (rnf m) $ seq (rnf s) (rnf k)

nbayes2file :: String -> NBayes -> IO ()
nbayes2file = encodeFile

file2nbayes :: String -> IO (NBayes)
file2nbayes = decodeFile

-------------------------------------------------------------------------------
-- Training

emptyNBayes :: DataDesc -> NBayes
emptyNBayes dataDesc = NBayes
    { dataDesc = dataDesc
    , labelCount = A.listArray (0,(numLabels dataDesc)-1) [0..]
    , attrDist = A.listArray ((0,(numLabels dataDesc)-1),(0,(numAttr dataDesc)-1)) $ repeat UnknownDist
    }

train :: DataDesc -> [(Int,DataPointSparse)] -> NBayes
train desc dpL = foldl add1dp (emptyNBayes desc) dpL

train' :: DataDesc -> [(Int,DataPointSparse)] -> NBayes
train' desc dpL = foldl' add1dp (emptyNBayes desc) dpL

trainIO :: DataDesc -> [(Int,DataPointSparse)] -> IO (NBayes)
trainIO desc = trainIOItr 0 (emptyNBayes desc)
    where
        trainIOItr :: Int -> NBayes -> [(Int,DataPointSparse)] -> IO (NBayes)
        trainIOItr itr nb [] = return nb
        trainIOItr itr nb dpL = do 
            putStrLn $ "trainIOItr"++show itr
            let nb' = add1dp nb $ head dpL
            deepseq nb' $ 
                trainIOItr (itr+1) nb' (tail dpL)
    
train1dp :: DataDesc -> (Int,DataPointSparse) -> NBayes
train1dp dataDesc dp = add1dp (emptyNBayes dataDesc) dp

add1dp :: NBayes -> (Int,DataPointSparse) -> NBayes
add1dp nb (label,dp) = nb { labelCount = (labelCount nb) A.// [(label,(labelCount nb A.! label)+1)]
                          , attrDist = A.accum nbUpdateDistribution (attrDist nb) updateL
                          }
    where
        updateL = [((label,attrI),attrV) | (attrI,attrV)<-dp]
--         newLabelVec = A.accum nbUpdateDistribution ((attrDist nb) A.! label) dp

-- add1dpST :: NBayes -> (Int,DataPointSparse) -> NBayesST
-- add1dpST nb (label,dp) = do
--     lc <- V.thaw $ labelCount nb
--     ad <- V.thaw $ attrDist nb
--     let nb' = 
--     return lc
--     let nb' = nb { labelCount = lc }
--     return nb'

nbUpdateDistribution :: NBayesDistribution -> DataItem -> NBayesDistribution
nbUpdateDistribution dist val = 
    case dist of
        UnknownDist -> 
            case val of
                Missing -> UnknownDist
                Discrete _ -> error "can't make discrete from unknown"
                Continuous x -> Gaussian x 0 1
        Gaussian m s k -> 
            case val of
                Missing -> Gaussian m s k
                Discrete _ -> error "using a discrete variable in an attribute that's already continuous; there is probably something wrong with either your dataset or the way you loaded it"
                Continuous x -> 
                    let k'=k+1
                        m'=m+(x-m)/k'
                        s'=s+(x-m)*(x-m')
                    in Gaussian m' s' k'

-------------------------------------------------------------------------------
-- Classification

probClassifyNB :: NBayes -> DataPointSparse -> [(Int,Double)]
probClassifyNB nb dp = [ (label, labelProbGivenDp label) | label <- [0..(numLabels $ dataDesc nb)-1]]
    where
        labelProbGivenDp label = (labelProbGivenNothing label)*(dpProbGivenLabel label)
        labelProbGivenNothing label = (fromIntegral $ labelCount nb A.! label)/(fromIntegral $ sum $ A.elems $ labelCount nb)
        dpProbGivenLabel label = foldl (*) 1 (attrProbL label)
        attrProbL label = [ attrProb di (attrDist nb A.! (label,attrIndex)) | (attrIndex,di) <- dp]

attrProb :: DataItem -> NBayesDistribution -> Double
attrProb Missing _ = 1
attrProb (Discrete _) _ = error "cannot classify discrete attributes yet."
attrProb (Continuous x) (Gaussian m s k) = 1/(sqrt $ 2*pi*var) * (exp $ -(x-m)^2/(2*var))
    where var = s/(k-1)

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

testnb = LazyNBayes.train desc td

testwriter = do
    encodeFile "testwriter.nbayes" testnb
    testnb' <- decodeFile "testwriter.nbayes"
    print (testnb==testnb')

-- import Data.Array.Repa
-- import Data.Array.Repa.Repr.Vector
-- 
-- data NBayes = NBayes 
--     { mL :: Array V DIM2 Double
--     , sL :: Array V DIM2 Double
--     }

-- import Data.Array
-- import Data.Array.ST
-- 
-- data NBayes = NBayes
--     { numLabels :: Int
--     , numAttr :: Int
--     , totalSamples :: Array Int Int
--     , mL :: Array (Int,Int) Double
--     , sL :: Array (Int,Int) Double
--     }
--     
-- emptyNBayes :: Int -> Int -> NBayes
-- emptyNBayes numLabels numAttr = NBayes
--     { numLabels = numLabels
--     , numAttr = numAttr
--     , totalSamples = array (1,numLabels)
--     , mL = array ((1,1),(numLabels,numAttr)) [((i,j),0) | i <- [1..numLabels], j<-[1..numAttr]]
--     , sL = array ((1,1),(numLabels,numAttr)) [((i,j),0) | i <- [1..numLabels], j<-[1..numAttr]]
--     }