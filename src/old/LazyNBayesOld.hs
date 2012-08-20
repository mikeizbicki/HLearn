module LazyNBayesOld
    where
         
import AI.Classification

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Binary
import Data.List
import Data.Monoid
import Data.Number.LogFloat
import Data.STRef
import Data.Vector.Binary
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Mutable as VM
import qualified Data.ByteString as BS
import Debug.Trace
import System.IO

-------------

type Probability = LogFloat
-- type Probability = Double

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

data NBayesST s = NBayesST
    { dataDescST :: DataDesc
    , labelCountST :: VM.STVector s Int
    , attrDistST :: VM.STVector s (VM.STVector s NBayesDistribution)
    }

thawNBayes :: NBayes -> ST s (NBayesST s)
thawNBayes nb = do
    lc <- V.thaw $ labelCount nb
    ad <- thaw2 $ attrDist nb
    return $ NBayesST
        { dataDescST = dataDesc nb
        , labelCountST = lc
        , attrDistST = ad
        }
        
thaw2 :: V.Vector (V.Vector a) -> ST s (VM.STVector s (VM.STVector s a))
thaw2 vv = join $ liftM V.thaw $ vSequence $ V.map V.thaw vv

thaw3 :: V.Vector (V.Vector a) -> ST s (VM.STVector s (VM.STVector s a))
thaw3 vv = do
    newvec <- VM.unsafeNew $ V.length vv
    sequence 
        [ do
            v <- V.thaw $ vv V.! i
            VM.write newvec i v
        | i <- [0..(V.length vv)-1]
        ]
    return newvec
        
vSequence :: Monad m => V.Vector (m a) -> m (V.Vector a)
vSequence v = liftM V.fromList $ sequence $ V.toList v

freezeNBayes :: NBayesST s -> ST s NBayes
freezeNBayes nbst = do
    lc <- V.freeze $ labelCountST nbst
--     ad <- liftM (V.map V.freeze) $ V.freeze $ attrDistST nbst
--     ad <- join $ liftM V.freeze $ vSequence $ VG.map VG.freeze $ attrDistST nbst
    ad <- V.freeze $ attrDistST nbst
    ad2 <- vSequence $ V.map V.freeze ad
    return $ NBayes
        { dataDesc = dataDescST nbst
        , labelCount = lc
        , attrDist = ad2
        }
    
testthaw = runST $ do
    trace "test1" $ return ()
    nbst <- thawNBayes $ {-testnb -- -}emptyNBayes $ DataDesc 100 50000
    trace "test2" $ return ()
    freezeNBayes nbst
    trace "test3" $ return ()

data NBayes = NBayes
    { dataDesc :: DataDesc
    , labelCount :: V.Vector Int
    , attrDist :: V.Vector (V.Vector NBayesDistribution)
    }
    deriving (Eq,Read,Show)

instance Monoid NBayes where
    mappend a b = a
    mempty = undefined

instance Binary NBayes where
    put nb = do
        put $ dataDesc nb
        put $ labelCount nb
        put $ attrDist nb
    get = liftM3 NBayes get get get

instance NFData NBayes where
    rnf nb = seq (rnf $ attrDist nb) $ seq (rnf $ dataDesc nb) (rnf $ labelCount nb)

instance NFData a => NFData (V.Vector a) where
  rnf v = V.foldl' (\x y -> y `deepseq` x) () v

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
    , labelCount = V.replicate (numLabels dataDesc) 0
    , attrDist = V.fromList [V.fromList [UnknownDist | y<-[1..numAttr dataDesc]] | x<-[1..numLabels dataDesc]]
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
add1dp nb (label,dp) = nb { labelCount = (labelCount nb) V.// [(label,(labelCount nb V.! label)+1)]
                          , attrDist = (attrDist nb) V.// [(label,newLabelVec)] 
                          }
    where
        newLabelVec = V.accum nbUpdateDistribution (attrDist nb V.! label) dp

trainST :: DataDesc -> [(Int,DataPointSparse)] -> NBayes
trainST desc dpL = runST $ do
    nbst <- thawNBayes $ emptyNBayes desc
    foldMTrace add1dpST nbst dpL
    freezeNBayes nbst

foldMTrace :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldMTrace f x ys = foldMTraceItr 0 (length ys) f x ys
    where
        foldMTraceItr itr len f x [] = return x
        foldMTraceItr itr len f x (y:ys) = do
            fm <- f x y
            fm2 <- if (itr `mod` 10==0)
                      then trace ("itr="++show itr++"/"++show len) $ f x y
                      else f x y
            foldMTraceItr (itr+1) len f fm ys

add1dpST :: NBayesST s -> (Int,DataPointSparse) -> ST s (NBayesST s)
add1dpST nbst (label,dp) = do
    -- update labelCount
    
    oldLabelCount <- VM.read (labelCountST nbst) label
    VM.write (labelCountST nbst) label (oldLabelCount+1)
    
    -- update attrDist
    attrDistLabel <- VM.read (attrDistST nbst) label
    vm_accum nbUpdateDistribution attrDistLabel dp
    
    return nbst
    
vm_accum :: (Control.Monad.Primitive.PrimMonad m) =>
            (a -> b -> a) -> VM.MVector (PrimState m) a -> [(Int, b)] -> m (VM.MVector (PrimState m) a)
vm_accum f vec [] = return vec
vm_accum f vec ((i,v):xs) = do
    oldV <- VM.read vec i
    VM.write vec i $ f oldV v
    vm_accum f vec xs
            
            

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
        attrProbL label = [ attrProb di (attrDist nb V.! label V.! attrIndex) {-logFloat (0.3::Double)-} | (attrIndex,di) <- dp]

        answer = [ (label, labelProbGivenDp label) | label <- [0..(numLabels $ dataDesc nb)-1]]
        normedAnswer = zip [0..] $ normalizeL [labelProbGivenDp label | label <- [0..(numLabels $ dataDesc nb)-1]]

attrProb :: DataItem -> NBayesDistribution -> Probability
attrProb Missing _ = 1
attrProb (Discrete _) _ = error "cannot classify discrete attributes yet."
attrProb (Continuous x) (UnknownDist) = 0.001
attrProb (Continuous x) (Gaussian m s k) = logFloat $ 
        if s>0
           then {-prob-} if prob < 0.001
                   then            0.001
                   else prob 
           else 0.1
    where
        prob = 1/(sqrt $ 2*pi*var) * (exp $ -(x-m)^2/(2*var))
        var = s/(k-1)

normalizeL :: [Probability] -> [Probability]
normalizeL xs = {-trace ("xs==0"++show (map (==0) xs)) $ 
                trace ("sum==0"++show (s==0)) $ 
                trace ("normxs="++show (map (/s) xs)) $ 
                trace ("max normxs="++show (maximum $ map (/s) xs)) $ 
                -}map (/s) xs
    where
        s = sum xs

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

testnb = LazyNBayesOld.train desc td

testwriter = do
    encodeFile "testwriter.nbayes" testnb
    testnb' <- decodeFile "testwriter.nbayes"
    print (testnb==testnb')
