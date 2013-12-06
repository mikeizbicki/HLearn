{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import Control.Monad.Random
import Data.List
import System.Environment
import System.IO

maxval = 1000
minval = -1000

main = do 
    dpstr:dimstr:xs <- getArgs
    let numdp = read dpstr :: Int
    let numdim = read dimstr :: Int
    printdata numdp numdim

printdata_spaceleak numdp numdim = do
    (xs :: [[Double]]) <- replicateM numdp $ replicateM numdim $ randomRIO (minval,maxval)
    let str = fmap list2str xs
    sequence_ $ fmap putStrLn str

printdata :: Int -> Int -> IO ()
printdata numdp numdim = forM_ [1..numdp] $ \i -> do
    (xs :: [Double]) <- replicateM numdim $ randomRIO (minval,maxval)
    putStrLn $ list2str xs
--     print str 

printdata_grid :: Int -> Int -> IO ()
printdata_grid n 2 = forM_ (take n [(x,y) | x <- [1..w], y <- [1..w]]) $ \(w,h) -> do
    putStrLn $ show w ++ "," ++ show h
    where
        n' = fromIntegral n
        w = ceiling $ n' ** (1/2)

list2str xs = concat $ intersperse "," $ map show xs
-- list2str (x:[]) = show x
-- list2str (x:xs) = show x ++ ',':list2str xs

