module Main
    where

import Control.DeepSeq
import Control.Monad
import Data.List.Split
import Data.Number.LogFloat
import qualified Data.ByteString.Char8 as BS
import System.Console.GetOpt
import System.Environment
import System.IO

import Base
import ClassificationAlgebra
import LazyNBayes

    
-------------------------------------------------------------------------------
-- main
    
main=do 
    args <- getArgs
    (opts,_) <- compilerOpts args
    
    if optCreateModel opts
       then createModel defFeatures $ DataDesc 97 592158
       else return ()
       
--     if optTestModel opts
--        then testModel
--        else return ()
       
    if optCreateSubmission opts
       then useModel defFeatures
       else return ()

defFeatures :: DataPointSparse -> DataPointSparse
defFeatures x = x

featurizeData :: DataPointSparse -> DataPointSparse
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
    , optTestDataFile = Just "data/test_data.csv"
    , optTrainingDataFile = Just "data/train_data.csv"
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
    csrTest <- loadCSR "data/test_data.csv"
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
    let probL = map (fromLogFloat . snd) $ probClassifyNB nbayes dp :: [Double]
--     let probL = map (fromLogFloat . snd) $ probClassifyNB nbayes (dpL !! 50) :: [Double]
    hPutStrLn hout $ (show itr)++","++(list2csv probL)
    createSubmission hout (itr+1) nbayes dpL

list2csv xs = tail $ reverse $ tail $ reverse $ show xs

-------------------------------------------------------------------------------
-- create model

createModel preprocessor dataDesc = do
    td <- loadSparseData
    let tdFeaturized = map (\(x,y) -> (x,preprocessor y)) td
    putStrLn "begin training"
    let nbayes = trainST dataDesc td
--     let nbayes = trainST (DataDesc 97 2) {-$ take 10-} tdFeaturized
    deepseq nbayes $ putStrLn "end training"
--     nbayes <- LazyNBayes.trainIO (DataDesc 97 592158) {-$ take 10-} td
--     putStrLn "end training"
    putStrLn "writing file"
    nbayes2file "model.nbayes.poisson.featurized" nbayes
    putStrLn "done."

-------------------------------------------------------------------------------
-- helper functions

loadSparseData :: IO (TrainingDataSparse)
loadSparseData = do
    labelL <- loadLabels "data/train_labels.csv"
    csr <- loadCSR "data/train_data.csv"
    
    let labelL_fix = map (read . BS.unpack :: BS.ByteString -> Int) labelL
    let csr_fix = map (map (\(attrI,attr) -> ((read . BS.unpack :: BS.ByteString -> Int) attrI
                                             ,Continuous $ (read . BS.unpack :: BS.ByteString -> Double) attr))) csr
    
    return $ zip labelL_fix csr_fix

csr2dpSparse :: [[(BS.ByteString,BS.ByteString)]] -> [DataPointSparse]
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
    putStrLn $ "CSR file opened: " ++ file
    
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

listSplit :: Int -> [(BS.ByteString,BS.ByteString)] -> [BS.ByteString] -> [[(BS.ByteString,BS.ByteString)]]
listSplit prevPtr zipL [] = []
listSplit prevPtr zipL ptrL = next:(listSplit curPtr rest $ tail ptrL)
    where
        (next,rest) = splitAt (curPtr-prevPtr) zipL
        curPtr = (read $ BS.unpack $ head ptrL)
        
testloads = do
    csrTest <- loadCSR "data/test_data.csv"
    csrTrain <- loadCSR "data/train_data.csv"
    putStrLn "done."
