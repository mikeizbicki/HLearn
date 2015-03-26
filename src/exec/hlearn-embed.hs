{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}

import SubHask
import SubHask.Algebra.Parallel
import SubHask.Compatibility.Cassava
import SubHask.Compatibility.Containers
import SubHask.Compatibility.Vector
import SubHask.Compatibility.Vector.Lebesgue
-- import SubHask.Monad as S
import Control.Monad

import HLearn.Data.LoadData
import HLearn.History (HistoryMonad(..),DynamicHistory(..),runDynamicHistory)
import HLearn.History.DisplayMethods
import HLearn.History.Timing
import HLearn.Optimization.Common
import HLearn.Optimization.GradientDescent
import HLearn.Optimization.LineMinimization.Univariate
import HLearn.Optimization.LineMinimization.Multivariate

import Data.List (replicate,tail,words,intersperse,zip)
import System.IO
import Debug.Trace

import System.IO.Unsafe

-------------------------------------------------------------------------------

import qualified Control.Monad as M
import Control.Monad.Random hiding (fromList)
import Control.Monad.Identity (Identity)

type UVector = UnboxedVector
type BVector = BoxedVector

-- _replicateM :: (Unfoldable as, Elem as~a, Monad Hask m) => Int -> m a -> m as
_replicateM n = fmap fromList . replicateM n

{-
-- type HistoryMonad_ m = (Monad Hask m, HistoryMonad m)
---------

instance Functor Hask (RandT g Identity) where
    fmap = M.fmap

instance Then (RandT g Identity) where
    (>>) = (M.>>)

instance Monad Hask (RandT g Identity) where
    join = M.join
    return_ = M.return

---------

instance Functor Hask DynamicHistory where
    fmap f (DynamicHistory h) = DynamicHistory $ M.fmap f h

instance Then DynamicHistory where
    (>>) (DynamicHistory a) (DynamicHistory b) = DynamicHistory $ (M.>>) a b

instance Monad Hask DynamicHistory where
    join = M.join
    return_ a = DynamicHistory $ M.return a
-}

-------------------------------------------------------------------------------

main = derivMain

-------------------------------------------------------------------------------

derivMain = do
    let f :: Double -> Double
        f  x = x*x
        f' x = 2*x

    print $ testDerivativeHilbert f f' 1000 (1e-6::Double) (1e-2::Double)

    putStrLn "done"

-- testDerivativeHilbert ::
--     ( Hilbert a
--     , Lattice_ a
--     ) => (a -> Scalar a) -- ^ function to test
--       -> (a -> a)        -- ^ derivative to test
--       -> a               -- ^ test location
--       -> a               -- ^ direction to test
--       -> Scalar a        -- ^ scale of test
--       -> Logic a
testDerivativeHilbert f f' x dir scale =
--     trace ("hi="++show hi) $
--     trace ("lo="++show lo) $
    trace ("f'1="++show f'1) $
    trace ("nd="++show numderiv) $
    trace ("f'2="++show f'2) $

    (f'1 >= numderiv && f'2 <= numderiv) || (f'1 <= numderiv && f'2 >= numderiv)
    where
        f'1 = normalize dir <> f' x
        f'2 = normalize dir <> f' (x + dir)
        numderiv = (size $ f x - f (x+dir)) / size dir

-------------------------------------------------------------------------------

embed = do

    -- load data
    xls :: Array (Labeled' (UVector Double) Int)
--        <- loadCSVLabeled "/home/user/proj/hlearn/datasets/csv/uci/haberman.data" 3
       <- loadCSVLabeled "/home/user/proj/hlearn/datasets/csv/uci/wine.csv" 0

    let xs = ifor xls $ \_ (Labeled' v _) -> v

    ys :: Array (UVector Double)
       <- _replicateM (size xs)
        . fmap listToVector
        . replicateM 2
        $ getRandomR (-1,1)

--     ys :: Array (UVector Double)
--        <- fmap fromList
--         . forM [0..size xs-1] $ \i ->
--             fmap listToVector $ do
--                 a <- getRandomR (-1,1)
--                 return [fromIntegral i,a]

    -- run optimization

    let (f,f') = mksne xs

    cgd <- runDynamicHistory linearTrace $ fmap (jitter 40) $ conjugateGradientDescent_
        ( lineSearchBrent ( brentTollerance 1e-3 || maxIterations 10 ))
        polakRibiere
        (\ v ->            f  (ArrayT v) )
        (\ v -> unArrayT $ f' (ArrayT v) )
        (unArrayT ys)
        ( maxIterations 500 )

    cgd' <- runDynamicHistory linearTrace $ conjugateGradientDescent_
        ( lineSearchBrent ( brentTollerance 1e-3 || maxIterations 10 ))
        polakRibiere
        (\ v ->            f  (ArrayT v) )
        (\ v -> unArrayT $ f' (ArrayT v) )
        (__x1 cgd)
        ( maxIterations 500 )

    let ys' = ArrayT $ __x1 cgd'

    -- save output
    let yls' = ifor ys' $ \i y -> Labeled' y (yLabeled' $ xls!i)
    hout <- openFile "hlearn-embed.dat" WriteMode
    forM_ (toList yls') $ \(Labeled' y l) -> do
        hPutStr hout (show l)
        hPutStr hout " "
        hPutStr hout $ concat $ intersperse " " $ tail $ words $ replace ((=='[')||(==']')||(== ',')) ' ' $ show y
        hPutStrLn hout ""
    hClose hout

    putStrLn "done"

-------------------------------------------------------------------------------

replace :: (a -> Bool) -> a -> [a] -> [a]
replace f a xs = map (\b -> if f b then a else b) xs

for :: [a] -> (a -> b) -> [b]
for xs f = map f xs

ifor  xs f = imap f xs
ifor_ xs f = ifor xs (\k v -> f k)

isum  xs f = sum $ ifor xs f
isum_ xs f = isum xs (\k v -> f k)

sum_ = sumOver

sumOver :: (Elem as~a, Foldable as, Monoid b) => as -> (a -> b) -> b
sumOver as f = sum $ for (toList as) f

lg x = log x / log 2

----------------------------------------

jitter n a = a { __x1 = randShiftVector n $ __x1 a }

randShiftVector :: Double -> BVector (UVector Double) -> BVector (UVector Double)
randShiftVector n v = unArrayT $ fromList $ map go $ toList $ ArrayT v
    where
        go v = unArrayT $ fromList $ map go2 $ zip [0..] $ toList $ ArrayT v

        go2 (i,v) = v+rand i
            where
                {-# NOINLINE rand #-}
                rand i = (/n) $ unsafePerformIO $ randomRIO (-sumarr!i,sumarr!i)

        sumarr :: Array Double
        sumarr = fromList . toList $ ArrayT $ maxarr-minarr
        maxarr = foldl1' sup $ ArrayT v
        minarr = foldl1' inf $ ArrayT v

----------------------------------------

divergence x y = let z = distance x y in z*z

mksne
    :: Array (UVector Double)
    -> ( Array (UVector Double) -> Double
       , Array (UVector Double) -> Array (UVector Double)
       )
mksne xs =
    trace ("max sigma="++show (maximum [sigma!i | i <- [0..size sigma-1]])) $
    trace ("min sigma="++show (minimum [sigma!i | i <- [0..size sigma-1]])) $
    (sne,sne')
    where
        sne :: Array (UVector Double) -> Double
        sne ys = isum_ xs $ \i ->
                 isum_ xs $ \j ->
                    p i j * (logp i j - logq i j)

            where
                logq i j = (- divergence (ys!i) (ys!j)) - (yarrlog!i)

                yarrlog = ifor_ ys $ \i ->
                           maximum $
                           for (toList ys) $ \y -> -divergence (ys!i) y -- / (2 * sigma!i)

        sne' :: Array (UVector Double) -> Array (UVector Double)
        sne' ys = ifor_ ys $ \i ->
                  isum_ ys $ \j ->
                     (ys!i - ys!j) .* (p i j - q i j + p j i - q j i)

            where
                q i j = exp (- divergence (ys!i) (ys!j)) / (yarr!i)

                yarr = ifor_ ys $ \i ->
                        sum_ ys $ \y -> exp ( -divergence (ys!i) y ) -- / (2 * sigma!i))

        -- common pij
        p i j = exp (- divergence (xs!i) (xs!j) / (2 * sigma!i)) / (xarr!i)

        xarr = arr xs
        arr zs = ifor_ zs $ \i ->
                  sum_ zs $ \z -> exp ( -divergence (zs!i) z  / (2 * sigma!i))

        -- log of above
        logp i j = (- divergence (xs!i) (xs!j)) / (2 * sigma!i) - (xarrlog!i)

        xarrlog = arrlog xs
        arrlog zs = ifor_ zs $ \i ->
                     maximum $
                     for  (toList zs) $ \z -> -divergence (zs!i) z / (2 * sigma!i)

        -- sigma
        targetPerplexity = 15

--         sigma :: UArray Double
        sigma = ifor_ xs $ \i -> let s=go 1000000 1 10 i in s*s
            where
                go :: Double -> Double -> Double -> Int -> Double
                go !hi !lo 0     _ = lo+(hi-lo)/10
                go !hi !lo !itr !i = if perplexity > targetPerplexity
                    then go ave lo  (itr-1) i
                    else go hi  ave (itr-1) i

                    where
                        perplexity = 2 ** (-isum_ xs $ \j -> let pji = p j i in pji * lg pji)

                        ave = lo+(hi-lo)/10

                        p i j = exp (- divergence (xs!i) (xs!j) / (2*ave*ave))
                              / (xarr!i)

                        xarr = arr xs
                        arr zs = ifor_ zs $ \i ->
                                  sum_ zs $ \z -> exp ( -divergence (zs!i) z / (2*ave*ave))


----------------------------------------

mktsne
    :: Array (UVector Double)
    -> ( Array (UVector Double) -> Double
       , Array (UVector Double) -> Array (UVector Double)
       )
mktsne xs = (tsne,tsne')
    where
        tsne :: Array (UVector Double) -> Double
        tsne ys = isum_ xs $ \i ->
                  isum_ xs $ \j ->
                    p i j * (logp i j - log (q i j))

            where
                q i j = (yarr!i)/(1 + divergence (ys!i) (ys!j))
                yarr = ifor_ ys $ \i ->
                        sum_ ys $ \y -> 1/(1+divergence (ys!i) y)


        tsne' :: Array (UVector Double) -> Array (UVector Double)
        tsne' ys = ifor_ ys $ \i ->
                   isum_ ys $ \j ->
                     (ys!i - ys!j) .* (p i j - q i j + p j i - q j i)

            where
                q i j = (yarr!i)/(1 + divergence (ys!i) (ys!j))

                yarr = ifor_ ys $ \i ->
                        sum_ ys $ \y -> 1/(1+divergence (ys!i) y)

        -- common pij
        p i j = exp (- divergence (xs!i) (xs!j) / (2 * sigma!i)) / (xarr!i)

        xarr = arr xs
        arr zs = ifor_ zs $ \i ->
                  sum_ zs $ \z -> exp ( -divergence (zs!i) z  / (2 * sigma!i))

        -- log of above
        logp i j = (- divergence (xs!i) (xs!j)) - (xarrlog!i)

        xarrlog = arrlog xs
        arrlog zs = ifor_ zs $ \i ->
                     maximum $
                     for  (toList zs) $ \z -> -divergence (zs!i) z

        -- sigma
        targetPerplexity = 10

        sigma = ifor_ xs $ \i -> go 5 50 5 i
            where
                go :: Double -> Double -> Double -> Int -> Double
                go hi lo 0   _ = (hi+lo)/2
                go hi lo itr i = if perplexity < targetPerplexity
                    then go (hi-ave/2) lo (itr-1) i
                    else go hi (lo+ave/2) (itr-1) i

                    where
                        perplexity = 2 ** entropy i

                        ave = (hi+lo)/2

                        p i j = exp (- divergence (xs!i) (xs!j) / (2*ave*ave))
                              / (xarr!i)

                        xarr = arr xs
                        arr zs = ifor_ zs $ \i ->
                                  sum_ zs $ \z -> exp ( -divergence (zs!i) z / (2*ave*ave))

                        entropy i = -isum_ xs $ \j -> let pji = p j i in pji * lg pji


----------------------------------------
----------------------------------------

tsne :: Array (UVector Double)
     -> Array (UVector Double)
     -> Double
tsne xs ys
--     = trace ("tsne ret="++show ret) $ ret
    = ret
    where
        ret = isum_ xs $ \i ->
              isum_ xs $ \j ->
                p i j * (logp i j - log (q i j))

        p i j = exp (- divergence (xs!i) (xs!j)) / (xarr!i)
        q i j = (yarr!i)/(1 + divergence (ys!i) (ys!j))

        logp i j = (- divergence (xs!i) (xs!j)) - (xarrlog!i)

        -- memoize the denominator
        xarr = ifor_ xs $ \i ->
                sum_ xs $ \x -> exp ( -divergence (xs!i) x )

        yarr = ifor_ ys $ \i ->
                sum_ ys $ \y -> 1/(1+divergence (ys!i) y)

        xarrlog = arrlog xs
        arrlog zs = ifor_ zs $ \i ->
                     maximum $
                     for  (toList zs) $ \z -> -divergence (zs!i) z

tsne' :: Array (UVector Double)
      -> Array (UVector Double)
      -> Array (UVector Double)
tsne' xs ys = ret
    where
        ret = ifor_ ys $ \i ->
              isum_ ys $ \j ->
                  (ys!i - ys!j) .* (p i j - q i j + p j i - q j i)

        p i j = exp (- divergence (xs!i) (xs!j)) / (xarr!i) / (2*(fromIntegral $ size xs))
        q i j = (yarr!i)/(1 + divergence (ys!i) (ys!j))

        logp i j = (- divergence (xs!i) (xs!j)) - (xarrlog!i)

        -- memoize the denominator
        xarr = ifor_ xs $ \i ->
                sum_ xs $ \x -> exp ( -divergence (xs!i) x )

        yarr = ifor_ ys $ \i ->
                sum_ ys $ \y -> 1/(1+divergence (ys!i) y)

        xarrlog = arrlog xs
        arrlog zs = ifor_ zs $ \i ->
                     maximum $
                     for  (toList zs) $ \z -> -divergence (zs!i) z

