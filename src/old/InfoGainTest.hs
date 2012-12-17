
import Control.Monad
import Control.Monad.Random

import HMine.MiscUtils
import HMine.Classifiers.DTree

infoGainSimple :: (Ord label) => [label] -> [[label]] -> Double
infoGainSimple xs ys = ret {-trace ("xs="++show xs++"; ys="++show ys) $-} 
    where
        ret = infoGainHistogram (histogram xs) (map histogram ys)

t2l (a,b) = [a,b]

-- xs = [1,1,1,1,1,1,1,2,1,1,2,2,2,2,2,1,2,2,1,2,1,2,1,1,1,2,2,2,2,2,2,1,2,2,2,2,2,2]

-- xs = take 50 $ concat $ map t2l $ zip (repeat 1) (repeat 2)
xs = replicate 50 1

xsaddrand :: (RandomGen g) => [Int] -> Rand g [Int]
xsaddrand xs = do
    i <- getRandomR (1,length xs)
--     v <- getRandomR (1,2)
    let v=2
    return $ insertItem i v xs

xsadd 1 = insertItem 10 1 xs
xsadd i 
    | i <= 10 = insertItem 10 1 (xsadd $ i-1)
    | i <= 18 = insertItem 40 2 (xsadd $ i-1)
    | otherwise = insertItem 55 2 (xsadd $ i-1)

xsL = drop 16 $ map xsadd [1..25]

xsL' = evalRand xsLrand $ mkStdGen 10

xsLrand :: (RandomGen g) => Rand g [[Int]]
xsLrand = go [xs] 20
    where 
        
        go :: (RandomGen g) => [[Int]] -> Int -> Rand g [[Int]]
        go xss 0 = return xss
        go xss i = do
            xs' <- xsaddrand $ head xss
--             let xs' = head xss
            go (xs':xss) (i-1)

insertItem i item xs = a++[item]++b
    where
        (a,b)=splitAt i xs

infoL xs = map (\i -> infoGainSimple xs $ t2l $ splitAt i xs) [1..length xs]