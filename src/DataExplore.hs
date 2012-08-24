module Main
    where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Data.Binary
import Data.List
import Data.List.Split
import Data.Number.LogFloat
import Statistics.Sample
import System.Console.GetOpt
import System.Environment
import System.IO

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V

-- import HMine.Base
import HMine.Classifiers.LazyNBayes
import HMine.DataContainers
import HMine.DataContainers.DS_List
import HMine.MiscUtils
import HMine.RandUtils
import HMine.Evaluation.CrossValidation
import HMine.Math.TypeClasses

    
-------------------------------------------------------------------------------
-- main
    
main=do 
    args <- getArgs
    (opts,_) <- compilerOpts args
    
    if optCreateModel opts
       then createModel defFeatures
       else return ()
       
    if optTestModel opts
       then testModel defFeatures
       else return ()
       
    if optCreateSubmission opts
       then useModel defFeatures
       else return ()

defFeatures :: DPS -> DPS
defFeatures x = x

featurizeData :: DPS -> DPS
featurizeData td = featurize td
    where
        featurize dp = [(0,Continuous $ fromIntegral $ length dp)
                       ,(1,Continuous $ sum $ map (disect . snd) dp)]
        
        disect (Continuous x) = x

-------------------------------------------------------------------------------
-- option handling

data Options = Options
    { optCreateModel :: Bool
    , optCreateSubmission :: Bool
    , optTestModel :: Bool
    , optModelFile :: Maybe FilePath
    , optTestDataFile :: Maybe FilePath
    , optTrainingDataFile :: Maybe FilePath
    }
    
defOptions = Options
    { optCreateModel = False
    , optCreateSubmission = False
    , optTestModel = False
    , optModelFile = Just "model.def"
    , optTestDataFile = Just "/home/user/proj/haskell-kaggle/data/emc/test_data.csv"
    , optTrainingDataFile = Just "/home/user/proj/haskell-kaggle/data/emc/train_data.csv"
    }
    
options :: [OptDescr (Options -> Options)]
options =
    [ Option
        ['c']
        ["create"]        
        (NoArg (\opts -> opts { optCreateModel=True } ))
        "create a new model from training data"
    , Option
        ['s']
        ["submission"]        
        (NoArg (\opts -> opts { optCreateSubmission=True } ))
        "create a submission from test data and model"
    , Option
        ['t']
        ["test"]        
        (NoArg (\opts -> opts { optTestModel=True } ))
        "test the model using k-fold validation"
    , Option
        ['m']      
        ["model"]        
        (ReqArg (\arg opts -> opts { optModelFile=Just arg }) "FILE")
        "FILE is the model to be read from/saved to"
    ]
    
compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (foldl (flip id) defOptions o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: ic [OPTION...] files..."
    
-------------------------------------------------------------------------------
-- create submission

useModel preprocessor = do
    putStrLn "loading test data"
    csrTest <- loadCSR "/home/user/proj/haskell-kaggle/data/emc/test_data.csv"
    let dpL = csr2dpSparse csrTest
    let dpLfeaturized = map preprocessor dpL
    
    putStrLn "loading model.nbayes"
    nbayes <- file2nbayes "model.nbayes.poisson.featurized"
    deepseq nbayes $ return ()
    
--     print nbayes
    
    putStrLn "classifying"
    hout <- openFile "submission.csv" WriteMode
    createSubmission hout 1 nbayes dpLfeaturized
    hClose hout
    putStrLn "done."

createSubmission hout itr nbayes [] = return ()
createSubmission hout itr nbayes (dp:dpL) = do
-- createSubmission hout itr nbayes dpL = do
    putStrLn $ "createSubmission itr="++show itr
    let probL = map (fromLogFloat . snd) $ probabilityClassify nbayes dp :: [Double]
--     let probL = map (fromLogFloat . snd) $ probClassifyNB nbayes (dpL !! 50) :: [Double]
    hPutStrLn hout $ (show itr)++","++(list2csv probL)
    createSubmission hout (itr+1) nbayes dpL

list2csv xs = tail $ reverse $ tail $ reverse $ show xs

-------------------------------------------------------------------------------
-- test model

testModel preprocessor = do

--     print $ foldl1' (+) $ unstream (decode $ encode $ Stream [1..10000000::Integer] :: Stream Integer)

--     test<-decodeFile "src/test" :: IO (Stream Integer)
--     print (foldl1' (+) $ unstream test)

--     (l1,l2) <- evalRandIO $ randSplitUnsafe 0.8 (take 10000 $ ld td)
--     let (l1,l2) = evalRand (randSplitLUnsafe 0.5 [1..10000000]) (mkStdGen 10)
--     let model=trainST defNBayesParams l1
--     print $ probabilityClassify model $ snd $ head l2
--     print (foldl1' (+) l1, foldl1' (+) l2)





--     let xs = [1..10000000]
--     let ys = [1..10000000]
--     let (l1x,l2x) = partition ((==) 1 . flip mod 2) $ xs
--     let (l1y,l2y) = partition ((==) 1 . flip mod 2) $ ys
--     print (foldl1' (+) l1x, foldl1' (+) l2y)

    let xs = [1..100000]
    let ys = [1..100000]
    (l1x,l2x) <- partitionM (return . (==) 1 . flip mod 2) $ xs
    (l1y,l2y) <- partitionM (return . (==) 1 . flip mod 2) $ ys
    print (foldl1' (+) l1x, foldl1' (+) l2y)
    
--     td <- loadSparseData
-- --     let cvres = evalRand (crossValidation defNBayesParams (takeFirst 10000 td) 0.8 1) $ mkStdGen 1
--     let cvres = evalRand (crossValidation defNBayesParams td 0.8 1) $ mkStdGen 1
--     let m=mean $ V.fromList cvres
--     let s=stdDev $ V.fromList cvres
--     
--     writeFile "testModel.out" $ (show cvres) ++"\n mean="++(show m)++"\n stddev="++show s

    putStrLn "done."

-------------------------------------------------------------------------------
-- create model

createModel preprocessor = do
    td <- loadSparseData
    putStrLn "begin training"
--     let nbayes = trainST defNBayesParams $ td {ld = {-take 30000 $-} ld td}
    let nbayes = trainST defNBayesParams td
    deepseq nbayes $ putStrLn "end training"
    putStrLn "writing file"
    nbayes2file "model.nbayes.poisson.featurized" nbayes
    putStrLn "done."

-------------------------------------------------------------------------------
-- helper functions

loadSparseData :: IO (DS_List Int (LDPS Int))
loadSparseData = do
    labelL  <- liftM (map (read . BS.unpack :: BS.ByteString -> Int)    . bs2list) $ BS.readFile "/home/user/proj/haskell-kaggle/data/emc/train_labels.csv"
    valL    <- liftM (map (read . BS.unpack :: BS.ByteString -> Double) . bs2list) $ BS.readFile "/home/user/proj/haskell-kaggle/data/emc/train_val.csv"
    rowL    <- liftM (map (read . BS.unpack :: BS.ByteString -> Int)    . bs2list) $ BS.readFile "/home/user/proj/haskell-kaggle/data/emc/train_row.csv"
    colL    <- liftM (map (read . BS.unpack :: BS.ByteString -> Int)    . bs2list) $ BS.readFile "/home/user/proj/haskell-kaggle/data/emc/train_col.csv"

    let zipL = zip rowL $ map Continuous valL
    let csr = listSplit2 0 zipL (tail colL)
    return $ DS_List
        { dsDesc = DataDesc 97 [0..97] 592158
        , dsL = zip labelL csr
        , dsLen = length labelL
        } 
    where 
        bs2list=BS.split $ toEnum $ fromEnum ','

        listSplit2 prevPtr zipL [] = []
        listSplit2 prevPtr zipL ptrL = next:(listSplit2 curPtr rest $ tail ptrL)
            where
                (next,rest) = splitAt (curPtr-prevPtr) zipL
                curPtr = (head ptrL)


-- loadSparseDataOld :: IO (TrainingDataSparse)
-- loadSparseDataOld = do
--     labelL <- loadLabels "/home/user/proj/haskell-kaggle/data/emc/train_labels.csv"
--     csr <- loadCSR "/home/user/proj/haskell-kaggle/data/emc/train_data.csv"
--     
--     let labelL_fix = map (read . BS.unpack :: BS.ByteString -> Int) labelL
--     let csr_fix = map (map (\(attrI,attr) -> ((read . BS.unpack :: BS.ByteString -> Int) attrI
--                                              ,Continuous $ (read . BS.unpack :: BS.ByteString -> Double) attr))) csr
--     
--     return $ LabeledDataSparse 
--         { ldDesc = DataDesc 97 592158
--         , ld = zip labelL_fix csr_fix
--         }

csr2dpSparse :: [[(BS.ByteString,BS.ByteString)]] -> [DPS]
csr2dpSparse csr = map (map (\(attrI,attr) -> ((read . BS.unpack :: BS.ByteString -> Int) attrI
                                              ,Continuous $ (read . BS.unpack :: BS.ByteString -> Double) attr))) csr

loadLabels :: String -> IO [BS.ByteString]
loadLabels file = do
    hin <- openFile file ReadMode
    labelL <- liftM (BS.split $ toEnum $ fromEnum ',') $ BS.hGetLine hin
    hClose hin
    return labelL
    
loadCSR :: String -> IO [[(BS.ByteString,BS.ByteString)]]
loadCSR file = do
    hin <- openFile file ReadMode
    putStrLn $ "loadCSR: CSR file opened: " ++ file
    
    [rows,cols] <- liftM str2list $ hGetLine hin
    putStrLn $ "(rows,cols)=" ++ (show $ (rows,cols))
    
    valL <- liftM (BS.split $ toEnum $ fromEnum ',') $ BS.hGetLine hin
    rowIndexL <- liftM (BS.split $ toEnum $ fromEnum ',') $ BS.hGetLine hin
    colPointerL <- liftM (BS.split $ toEnum $ fromEnum ',') $ BS.hGetLine hin
    
    let zipL = zip rowIndexL valL
    
    hClose hin
    putStrLn $ "CSR file closed: "++file
    
    return $ listSplit 0 zipL (tail colPointerL)

str2list = splitOn ","

-- listSplit :: Int -> [(BS.ByteString,BS.ByteString)] -> [BS.ByteString] -> [[(BS.ByteString,BS.ByteString)]]
-- listSplit :: Int -> [(a,b)] -> [BS.ByteString] -> [[(BS.ByteString,BS.ByteString)]]
listSplit prevPtr zipL [] = []
listSplit prevPtr zipL ptrL = next:(listSplit curPtr rest $ tail ptrL)
    where
        (next,rest) = splitAt (curPtr-prevPtr) zipL
        curPtr = (read $ BS.unpack $ head ptrL)

testloads = do
    csrTest <- loadCSR "/home/user/proj/haskell-kaggle/data/emc/test_data.csv"
    csrTrain <- loadCSR "/home/user/proj/haskell-kaggle/data/emc/train_data.csv"
    putStrLn "done."
