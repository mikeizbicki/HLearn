module HLearn.RandUtils
    where

import Control.Monad
import Control.Monad.Random
import Control.Monad.ST
import System.Random
import Data.Array.ST
import GHC.Arr

import Data.Binary
import Data.List
import System.IO.Unsafe

import qualified Data.Foldable as F

-- import HLearn.MiscUtils


{-poisson :: (Random a, Ord a, Floating a, Num b, RandomGen g) => a -> Rand g b
poisson lambda = go 0 1
    where
        limit = exp (-lambda)
        go k p = do
            u <- getRandomR (0,1)
            let p'=p*u
            if p<=limit
               then return (k-1)
               else go (k+1) p'-}
               
-- | returns a random sample from a Poisson distribution parameterized by its mean (often called λ).  This function uses Knuth's algorithm in TAOCP, and runs in time O(λ).  For very large λ, it may be more efficient to sample from a normal distribution with (mean=stddev=λ), which is a good approximation of the Poisson distribution.
poisson :: (Random a, Ord a, Floating a, Num b, RandomGen g) => a -> Rand g b
poisson lambda = go 0 0
    where
        limit = (-lambda)
        go k p = do
            u <- getRandomR (0,1)
            let p'=p+log u
            if p'<=limit
               then return k
               else go (k+1) p'


sampleL :: (RandomGen g, Show a,Eq a) => Int -> [(a,Double)] -> Rand g [a]
sampleL num xs = replicateM num (fromList $ fmap (\(x,w) -> (x,toRational w)) xs)

{-sampleL :: (RandomGen g, Show a,Eq a) => Int -> [(a,Double)] -> Rand g [a]
sampleL n xs = do
    randL <- replicateM n $ getRandomR (0,1)
    return $ sampleWalk 0 xs randL
    where 
          totalWeights = sum $ map snd xs-}
    
sampleWalk :: (Show a) => Double -> [(a,Double)] -> [Double] -> [a]
sampleWalk tally [] _  = []
sampleWalk tally _  [] = []
sampleWalk tally (x:xs) (y:ys) = 
    if not sanity
       then error $ "sample: One of the sampling weights is either NaN or Infinity:" -- ++(show xs) ++ " -- "++(show ys)
       else if ((snd x)+tally)>(y)
               then (fst x):(sampleWalk tally (x:xs) $ ys)
               else sampleWalk (tally+(snd x)) (xs) (y:ys)
    where 
        sanity = (isNumber $ snd x) && (isNumber y)
        diff = (snd x)+tally-y

isNumber :: Double -> Bool
isNumber x = if x/=x -- x is NaN
                then False
                else if x==x+1 -- x is +/- Infinity
                        then False
                        else True

randList :: (Random a, Eq a) => StdGen -> Int -> (a,a) -> [a]
randList rgen 0 interval = []
randList rgen n interval = if r==r
                              then r:(randList rgen' (n-1) interval)
                              else error "randList: r/=r --> r==NaN"
    where (r,rgen') = randomR interval rgen


-------------------------------------------------------------------------------
-- Random utils

-- randSplitTrainingData :: 
--     ( RandomGen g
--     , DataSparse Int lds (LDPS Int)
--     ) => 
--     Double -> lds (LDPS Int) -> Rand g (lds (LDPS Int) ,lds (LDPS Int) )
-- randSplitTrainingData factor lds = do
--     (ld1,ld2) <- randSplit{-Unsafe-} factor ld
--     return (LabeledDataSparse desc ld1,LabeledDataSparse desc ld2)
-- 

randf factor = (\x -> fmap (<factor) $ getRandomR (0,1))

-- partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a],[a])
-- partitionM f xs = do
--     boolL <- mapM f xs
--     let l1 = map snd $ filter fst $ zip boolL xs
--     let l2 = map snd $ filter (not . fst) $ zip boolL xs
--     return (l1,l2)

partitionM               :: (Monad m) => (a -> m Bool) -> [a] -> m ([a],[a])
-- {-# INLINE partitionM #-}
partitionM p xs = F.foldrM (select p) ([],[]) xs

select :: (Monad m) => (a -> m Bool) -> a -> ([a], [a]) -> m ([a], [a])
-- select p x ~(ts,fs) | p x       = (x:ts,fs)
--                     | otherwise = (ts, x:fs)
select p x ~(ts,fs) = do
    res <- p x
    return $ if res 
                then (x:ts,fs)
                else (ts,x:fs)

randSplitL :: (RandomGen g) => Double -> [a] -> Rand g ([a],[a])
randSplitL factor xs = do
    randL <- mapM (\x -> fmap (<factor) $ getRandomR (0,1)) xs
--     randL <- sequence $ repeat $ fmap (<factor) $ getRandomR (0,1)
    let l1 = map snd $ filter fst $ zip randL xs
    let l2 = map snd $ filter (not . fst) $ zip randL xs
--     let ret= (l1,l2)
    return (l1,l2)
    where
        xs' = xsThunk (1::Int)
        xsThunk (a) = xs
-- 
-- randSplit2 :: (RandomGen g) => Double -> ([a],[a]) -> Rand g ([a],[a])
-- randSplit2 factor (xs1,xs2) = do
-- --     randL <- mapM (\x -> fmap (<factor) $ getRandomR (0,1)) xs
--     randL <- sequence $ repeat $ fmap (<factor) $ getRandomR (0,1)
--     let l1 = map snd $ filter fst $ zip randL xs1
--     let l2 = map snd $ filter (not . fst) $ zip randL xs2
-- --     let ret= (l1,l2)
--     return {-$ deepseq l1 $ deepseq l2 $-}(l1,l2)
-- 
-- randSplitLUnsafe :: (RandomGen g) => Double -> [a] -> Rand g ([a],[a])
-- randSplitLUnsafe factor xs = do
--     randL <- mapM (\x -> fmap (<factor) $ getRandomR (0,1)) xs
-- --     randL <- sequence $ replicate (length xs) $ fmap (<factor) $ getRandomR (0,1)
--     let (fileL1,fileL2) = unsafePerformIO $ do
--         randNum <- getRandom
--         let tmpFile = "tmp/randL."++show (randNum::Word32)
--         putStrLn "encodeFile randL"
-- --         putStrLn $ "randL = "++(show randL)
--         encodeFile tmpFile $ Stream randL
--         putStrLn "done."
--         fileL1 <- {-liftM unstream $-} lazyDecodeFile tmpFile :: IO [Bool]
--         fileL2 <- {-liftM unstream $ -}lazyDecodeFile tmpFile :: IO [Bool]
--         putStrLn "handles acquired"
--         return (fileL1,fileL2)
--     let l1 = map snd $ filter fst $ zip fileL1 xs
--     let l2 = map snd $ filter (not . fst) $ zip fileL2 xs
--     return (l1,[])
-- --     return (l1,l2)
--     where
--         xs' = xsThunk (1::Int)
--         xsThunk (a) = xs

-- randSplitCrash :: (RandomGen g) => Double -> [a] -> Rand g ([a],[a])
-- randSplitCrash factor xs = foldM listItr ([],[]) xs
--     where
--         listItr (l1,l2) x = do
--             rand <- getRandomR (0,1) 
--             if rand < factor
--                 then return (x:l1,l2)
--                 else return (l1,x:l2)
-- 
-- | Shuffle algorithm taken from http://www.haskell.org/haskellwiki/Random_shuffle
        
shuffle :: RandomGen g => [a] -> Rand g [a]
shuffle xs = do
    let l = length xs
    rands <- take l `fmap` getRandomRs (0, l-1)
    let ar = runSTArray $ do
        ar <- thawSTArray $ listArray (0, l-1) xs
        forM_ (zip [0..(l-1)] rands) $ \(i, j) -> do
            vi <- readSTArray ar i
            vj <- readSTArray ar j
            writeSTArray ar j vi
            writeSTArray ar i vj
        return ar
    return (elems ar)
        
-------------------
        
-- randSplit:: (RandomGen g) => Double -> [a] -> Rand g ([a],[a])
-- randSplit factor xs = do
--     rgen <- liftM mkStdGen getRandom
--     let is = randList2 rgen (floor $ factor*(fromIntegral $ length xs)) (length xs-1) S.empty
--     return $ randSplitWalk 0 (length xs - 1) xs is ([],[])
--     
-- randList2 :: StdGen -> Int -> Int -> S.Set Int -> [Int]
-- randList2 rgen total m set = 
--     if S.size set == total
--        then S.toList set
--        else randList2 rgen' total m (S.insert r set)
--            where (r,rgen') = randomR (0, m) rgen
-- 
-- randSplitWalk :: Int -> Int -> [a] -> [Int] -> ([a],[a]) -> ([a],[a])
-- randSplitWalk itr stop xs     []     (s1,s2) = (s1,xs++s2)
-- randSplitWalk itr stop (x:xs) (y:ys) (s1,s2) = 
--     if itr>stop
--        then (s1,s2)
--        else if itr==y
--                then randSplitWalk (itr+1) stop xs ys     (x:s1,s2)
--                else randSplitWalk (itr+1) stop xs (y:ys) (s1,x:s2)
