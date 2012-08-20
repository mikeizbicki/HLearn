module Main
    where

import Control.Monad
import System.IO
import qualified Data.ByteString as BS

path="/home/user/proj/haskell-kaggle/data/emc/"

fileSplit=path++"train_data.csv"
fileValL=path++"train_val.csv"
fileRowL=path++"train_row.csv"
fileColL=path++"train_col.csv"

splitFile = do
    hin <- openFile fileSplit ReadMode
    line1 <- BS.hGetLine hin
    
    putStrLn fileValL
    line2 <- BS.hGetLine hin
    BS.writeFile fileValL line2
    
    putStrLn fileRowL
    line2 <- BS.hGetLine hin
    BS.writeFile fileRowL line2
    
    putStrLn fileColL
    line2 <- BS.hGetLine hin
    BS.writeFile fileColL line2
    
    hClose hin