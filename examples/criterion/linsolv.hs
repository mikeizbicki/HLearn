{-# OPTIONS_GHC -O2 -fllvm #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Criterion
import Criterion.Main
import System.IO
import qualified Prelude as P

import System.IO.Unsafe
import Debug.Trace

import qualified Data.Vector.Storable as VS
import qualified Numeric.LinearAlgebra as HM

import SubHask
import SubHask.Algebra.HMatrix
import HLearn.History
import HLearn.History.DisplayMethods
import HLearn.Optimization.Common
import HLearn.Optimization.GradientDescent
import HLearn.Optimization.LineMinimization.Univariate
import HLearn.Optimization.LineMinimization.Multivariate

deriving instance Typeable HM.Matrix
type instance Scalar (HM.Matrix r) = r

{-# NOINLINE emptyOptimize #-}
emptyOptimize :: (HistoryMonad m, Reportable m Double) => Int -> m Double
emptyOptimize n = optimize return (10::Double) (maxIterations n)

emptyDynamicHistory :: Int -> Double
-- emptyDynamicHistory n = unsafePerformIO $ runDynamicHistory (linearTrace) $ emptyOptimize n
emptyDynamicHistory n = unsafePerformIO $ runDynamicHistory idDisplayMethod $ emptyOptimize n

emptySimpleHistory :: Int -> Double
emptySimpleHistory n = runSimpleHistory $ emptyOptimize n

-- | ax = b
-- linsolveH :: 
--     ( v ~ Matrix r
--     , InnerProductSpace v
--     , Field (Scalar v)
--     , Ord (Scalar v)
--     , Typeable (Scalar v)
--     , Typeable v
--     ) => v
--       -> v
--       -> History v
-- linsolveH a b = undefined
linsolveP !a !b =  fmap (^.x1) $ conjugateGradientDescent_
    ( cgdQuadraticStep a b )
--     ( backtracking (wolfe 1e-4 1e-1) )
--     ( lineSearchBrent ( brentTollerance 1e-6 || maxIterations 20) )
    steepestDescent 
--     fletcherReeves -- polakRibiere
    f
    f'
    b
    ( maxIterations 50
--     , multiplicativeTollerance 1e-10
    )
    where
        f :: Matrix Double -> Double
        -- f  x = toSingleton $ 0.5 *. trans x * a * x - trans b * x
        f !x = abs $ f' x

        f' !x = a*x - b
--         f' !x = a*x - b

data CGD v = CGD
    { r0 :: !v
    , d0 :: !v
    , x0 :: !v
    , alpha0 :: !(Scalar v)
    , fx0 :: !(Scalar v)
    , a :: !(Outer v)
    }
    deriving (Typeable)

instance Has_fx1 CGD v where
    fx1 = lens Main.fx0 undefined -- Main.fx0 undefined

-- linsolveH :: 
--     ( HistoryMonad m 
--     , Reportable m (CGD (Matrix Double))
--     ) => Matrix Double -> Matrix Double -> m (Matrix Double)
linsolveH a b = fmap x0 $ optimize
    ( \cgd -> do 
        let x1 = x0 cgd + alpha0 cgd *. r0 cgd
            r1 = r0 cgd - alpha0 cgd *. a * r0 cgd
--             r1 = b - a*x1
--             r1 = -(f' x1) -- (b - a*x1)
            alpha1 = r1 <> r1 / (toSingleton $ trans r1 * a * r1)
        alpha1' <- lineSearchBrent (maxIterations 20) f f' x1 (f' x1) 1 
        trace ( ""
          ++ "; r0<>r1 = "++show (r0 cgd <> r1)
          ++ "; alpha1 = "++show (alpha1)
--           ++ "; alpha1' = "++show (alpha1')
          ++ "; f x = "++show (f x1)
--           ++ "; |f' x|2 = "++show (abs $ innerProductNorm $ f' x1)
--           ++ "\n; x1 = "++show (x1)
--           ++ "\n; r1="++show (r1)
--           ++ "\n; r0="++show (r0 cgd)
--           ++ "\n"
--           ++ "; |f (x1+0.5*alpha1*r1)| = "++show (abs $ f $ x1+0.5*alpha1*.r1)++"\n"
--           ++ "; |f (x1+0.6*alpha1*r1)| = "++show (abs $ f $ x1+0.6*alpha1*.r1)++"\n"
--           ++ "; |f (x1+0.7*alpha1*r1)| = "++show (abs $ f $ x1+0.7*alpha1*.r1)++"\n"
--           ++ "; |f (x1+0.8*alpha1*r1)| = "++show (abs $ f $ x1+0.8*alpha1*.r1)++"\n"
--           ++ "; |f (x1+0.9*alpha1*r1)| = "++show (abs $ f $ x1+0.9*alpha1*.r1)++"\n"
--           ++ "; |f (x1+1.0*alpha1*r1)| = "++show (abs $ f $ x1+1.0*alpha1*.r1)++"\n"
--           ++ "; |f (x1+1.1*alpha1*r1)| = "++show (abs $ f $ x1+1.1*alpha1*.r1)++"\n"
--           ++ "; |f (x1+1.2*alpha1*r1)| = "++show (abs $ f $ x1+1.2*alpha1*.r1)++"\n"
--           ++ "; |f (x1+1.3*alpha1*r1)| = "++show (abs $ f $ x1+1.3*alpha1*.r1)++"\n"
--           ++ "; |f (x1+1.4*alpha1*r1)| = "++show (abs $ f $ x1+1.4*alpha1*.r1)++"\n"
--           ++ "; |f (x1+1.5*alpha1*r1)| = "++show (abs $ f $ x1+1.5*alpha1*.r1)++"\n"
--           ++ "\n\n------"
          ) $
          return $ cgd
            { r0 = r1
            , d0 = r1
            , x0 = x1
            , alpha0 = alpha1
            }
    )
    ( let x0 = zero -- mkMatrix 2 1 [0.01,-0.5]
          r0 = b -a*x0
      in CGD
        { r0 = r0 
        , d0 = r0
        , x0 = x0
        , alpha0 = r0 <> r0 / (toSingleton $ trans r0 * a * r0)
        , Main.fx0 = f x0
        , a = a
--         , alpha0 = negate $ runSimpleHistory $ lineSearchBrent (maxIterations 20) f f' x0 (f' x0) 1
        } :: CGD (Matrix Double)
    )
    ( maxIterations 10 )
    where 
        f x = toSingleton $ 0.5 *. trans x * a * x - trans b * x
--         f x = abs $ a*x - b
        f' x = (a*x - b)

-- backtrack :: (Ord r, Field r) => (r -> r) -> r -> r -> r -> r
backtrack f fx gamma x = go x
    where
        go x = 
--           trace ("backtrack: x="++show x) $
          if fx > f x'
            then x'
            else go x'
            where 
                x' = gamma * x

type StepSize m v =
    ( InnerProductSpace v 
    , HistoryMonad m
    ) => (v -> Scalar v)
      -> (v -> v)
      -> (v -> Outer v)
      -> CGD v
      -> m (Scalar v)

lsGradientDescentOptimal :: StepSize m (Matrix Double)
lsGradientDescentOptimal f f' _ cgd = do
    let alpha1 = r0 cgd <> r0 cgd / (toSingleton $ trans (d0 cgd) * a cgd * d0 cgd)
    return alpha1

lsBrent :: 
    ( Reportable m (Scalar v)
    , Reportable m (LineBracket (Scalar v))
    , Reportable m (Brent (Scalar v))
    ) => StopCondition m (Brent (Scalar v)) -> StepSize m v
lsBrent !stops !f _ _ !cgd = do
    let g y = f $ x0 cgd + y *. d0 cgd
    let stepGuess = alpha0 cgd
    bracket <- lineBracket g (stepGuess/2) (stepGuess*2)
    brent <- brent g bracket stops
    return $ _x brent

type ConjMethod = forall v. InnerProductSpace v => v -> CGD v -> Scalar v

type Direction = forall v. InnerProductSpace v => CGD v -> v

cgd :: 
    ( InnerProductSpace v
    , HistoryMonad m 
    , Reportable m (Scalar v)
    , Reportable m (LineBracket (Scalar v))
    , Reportable m (Brent (Scalar v))
    , Reportable m (CGD v)
    , v ~ Matrix Double
    ) => (v -> Scalar v) 
      -> (v -> v) 
      -> (v -> Outer v) 
      -> m (v)
cgd = cgd' lsGradientDescentOptimal undefined

cgd' :: 
    ( InnerProductSpace v
    , HistoryMonad m 
    , Reportable m (Scalar v)
    , Reportable m (LineBracket (Scalar v))
    , Reportable m (Brent (Scalar v))
    , Reportable m (CGD v)
    , v ~ Matrix Double
    ) => StepSize m v
      -> Direction 
      -> (v -> Scalar v) 
      -> (v -> v) 
      -> (v -> Outer v) 
      -> m (v)
cgd' !step !dir !f !f' !f'' = fmap x0 $ optimize
    ( \cgd -> do
        alpha1 <- step f f' f'' cgd 
--         let alpha1 = r0 cgd <> r0 cgd / (toSingleton $ trans (d0 cgd) * a cgd * d0 cgd)

        let x1 = x0 cgd + alpha1 *. d0 cgd
            r1 = -f' x1
            beta1 = r1 <> r1 / r0 cgd <> r0 cgd
--             beta1 = conj r1 cgd
            d1 = r1 + beta1 *. d0 cgd

--         trace ( ""
--           ++ "; r0<>r1 = "++show (r0 cgd <> r1)
--           ++ "; alpha1 = "++show (alpha1)
-- --           ++ "; alpha1' = "++show (alpha1')
--           ++ "; f x = "++show (f x1)
--           ) $ 
        return $ cgd
            { x0 = x1
            , r0 = r1
            , d0 = d1
            , alpha0 = alpha1
            , Main.fx0 = f $ x1
            }
    )
    ( let x0 = zero
          f'x0 = f' zero
      in CGD
        { r0 = -f'x0
        , d0 = -f'x0
        , x0 = x0
        , alpha0 = 1
        , Main.fx0 = f x0
        , a = f'' x0
        }
    )
--     ( maxIterations 15 || multiplicativeTollerance 1e-6 )
    ( multiplicativeTollerance 1e-1 )

linsolveJ a b = fmap x0 $ optimize
    ( \cgd -> do 
        let x1 = x0 cgd + alpha0 cgd *. d0 cgd
            r1 = r0 cgd - alpha0 cgd *. a * d0 cgd
            beta1 = r1 <> r1 / r0 cgd <> r0 cgd
            d1 = r1 + beta1 *. d0 cgd

            alpha1 = r1 <> r1 / (toSingleton $ trans d1 * a * d1)
        return $ cgd
            { r0 = r1
            , d0 = d1
            , x0 = x1
            , alpha0 = alpha1
            }
    )
    ( let x0 = zero -- mkMatrix 2 1 [0.01,-0.5]
          r0 = b -a*x0
          d0 = r0
      in CGD
        { r0 = r0 
        , d0 = d0
        , x0 = x0
        , alpha0 = r0 <> r0 / (toSingleton $ trans d0 * a * d0)
        , Main.fx0 = f x0
        , a = a
        } :: CGD (Matrix Double)
    )
    ( maxIterations 11 )
    where 
--         f x = abs $ a*x - b
        f x = toSingleton $ 0.5 *. trans x * a * x - trans b * x
        f' x = (a*x - b)
-- g :: HM.Matrix Double -> Double
-- g  x = HM.sumElements $ HM.scale 0.5 (HM.trans x HM.<> a' HM.<> x) P.- (HM.trans x HM.<> b')
-- 
-- g' :: HM.Matrix Double -> HM.Matrix Double
-- g' x = a' HM.<> x P.- b'

-- linsolve ::
--     ( Ring v
--     , Field (Scalar v)
--     , Ord (Scalar v)
--     ) => v
--       -> v
--       -> v
-- linsolveDyn a b = unsafePerformIO $ runDynamicHistory (removeLineMin ||| linearTrace) (linsolveP a b)
-- linsolveDyn a b = unsafePerformIO $ runDynamicHistory (linearTrace) (linsolveP a b)
-- linsolveDyn !a !b = unsafePerformIO $ runDynamicHistory idDisplayMethod (linsolveH a b)

linsolveDyn :: Matrix Double -> Matrix Double -> Matrix Double
linsolveDyn !a !b = unsafePerformIO $ runDynamicHistory idDisplayMethod (cgd f f' f'')
-- linsolveDyn !a !b = unsafePerformIO $ runDynamicHistory linearTrace (cgd f f')
    where
        f :: Matrix Double -> Double
        f x = toSingleton $ 0.5 *. trans x * a * x - trans b * x

        f' :: Matrix Double -> Matrix Double
        f' x = (a*x - b)

        f'' _ = a

linsolveSimp :: Matrix Double -> Matrix Double -> Matrix Double
linsolveSimp !a !b = runSimpleHistory $ cgd f f' f''
    where
        f :: Matrix Double -> Double
        f x = toSingleton $ 0.5 *. trans x * a * x - trans b * x

        f' :: Matrix Double -> Matrix Double
        f' x = (a*x - b)

        f'' _ = a

-- xs = [1,2,2,2] :: [Double]
-- ys = [1,0.5,2,3] :: [Double]
-- zs = [0,1]     :: [Double]

-- xs = [3,2,2,6] :: [Double]
-- ys = xs
-- zs = [2,-8] :: [Double]
-- 
-- a = mkMatrix 2 2 xs
-- x = mkMatrix 2 1 ys
-- z = mkMatrix 2 1 [1,-0.5]  :: Matrix Double
-- q = mkMatrix 2 1 [0,1]  :: Matrix Double
-- -- r = mkMatrix 2 1 [-2,-1]  :: Matrix Double
-- b = mkMatrix 2 1 zs
-- 
-- a' = 2 HM.>< 2 $ xs
-- x' = 2 HM.>< 1 $ ys
-- z' = 2 HM.>< 1 $ [1,-0.5] :: HM.Matrix Double
-- q' = 2 HM.>< 1 $ [0,1] :: HM.Matrix Double
-- -- r' = 2 HM.>< 1 $ [-2,-1] :: HM.Matrix Double
-- b' = 2 HM.>< 1 $ zs

-- cgdQuadraticStep :: MultivariateLineSearch
cgdQuadraticStep a b _ _ !x0 !f'x0 _ = {-# SCC cgdQuadraticStep #-} do
    return $ r <> r / (toSingleton $ trans r * a * r)
--     return $ 0.5 *. (r <> r / (toSingleton $ trans f'x0 * a * f'x0))
--     return 1
    where
        r = b - a*x0 -- f'x0

randomVec :: MonadRandom m => Int -> m (VS.Vector Double)
randomVec n = liftM (VS.fromList . take n) getRandoms

randomPSD :: MonadRandom m => Int -> m (HM.Matrix Double)
-- randomPSD n = liftM (foldl1' (P.+)) $ replicateM n (do v <- randomVec n; return $ v `HM.outer` v )
randomPSD n = do
    xs <- replicateM n (randomVec n)
    return $ gramMatrix xs

gramMatrix :: [VS.Vector Double] -> HM.Matrix Double
gramMatrix xs = HM.trans m HM.<> m
    where 
        m = HM.fromLists $ map VS.toList xs

main = do

    putStrLn "------------------------"
    putStrLn "initializing matrices..."

    let dim=200::Int

--     let hm1 = dim HM.>< dim $ xs
--         hm2 = dim HM.>< dim $ ys
    let hm1 = evalRand (randomPSD dim) $ mkStdGen 1
        hm2 = evalRand (randomPSD dim) $ mkStdGen 2
    let zs = randoms (mkStdGen 2) :: [Double]
        hmv = dim HM.>< 1 $ zs
    deepseq hm1 $ deepseq hm2 $ deepseq hmv $ return ()

    let chol1 = HM.chol hm1
        chol2 = HM.chol hm2
    deepseq hm1 $ deepseq hm2 $ return ()

    let sm1 = fromHMatrix hm1 --mkMatrix dim dim xs
        sm2 = fromHMatrix hm2 --mkMatrix dim dim ys
        smv = mkMatrix dim 1 zs
    deepseq sm1 $ deepseq sm2 $ return ()

    let v1 = VS.fromList $ take dim zs
    deepseq v1 $ return ()

--     putStrLn $ "hm1="++show hm1++"\n\n\n"

    let x = HM.linearSolve hm1 hmv
    putStrLn $ "hm1 * x = hmv ==> x = " ++ show x
    putStrLn $ "hm1 * x - hmv = " ++ show ((hm1 HM.<> x) `HM.sub` hmv)
    putStrLn ""

    let x=HM.inv hm1 HM.<> hmv
    putStrLn $ "x = hm1^-1 * hmv = " ++ show x
    putStrLn $ "hm1 * x - hmv = " ++ show ((hm1 HM.<> x) `HM.sub` hmv)
    putStrLn ""

    let x = linsolveDyn sm1 smv
    putStrLn $ "sm1 * x = smv ==> x = " ++ show x
    putStrLn $ "sm1 * x - smv = " ++ show ((sm1 * x) - smv)
    putStrLn ""

    putStrLn "------------------------"
    putStrLn "begining tests..."

--     print $ emptySimpleHistory 1000

    defaultMain 
        [ bgroup "history"
            [
--             [ bench "emptyRunHistory - 10" $  nf emptyRunHistory 10
--             , bench "emptyRunHistory - 100" $  nf emptyRunHistory 100
--             , bench "emptyRunHistory - 1000" $  nf emptyRunHistory 1000
            ]
        , bgroup "multiply"
            [ bench "hmatrix" $ nf (hm1 HM.<>) hm2
            , bench "hmatrix - chol" $ nf (chol1 HM.<>) chol2
            , bench "subhask" $ nf (sm1 *) sm2
            ]
        , bgroup "linsolv"
            [ bench "hmatrix - linearSolve" $ nf (HM.linearSolve hm1) hmv
            , bench "hmatrix - cholSolve" $ nf (HM.cholSolve hm1) hmv
-- --             , bench "hmatrix - linearSolveLS" $ nf (HM.linearSolveLS hm1) hmv
-- --             , bench "hmatrix - linearSolveSVD" $ nf (HM.linearSolveSVD hm1) hmv
--             , bench "hmatrix - naive"    $ nf (HM.inv hm1 HM.<>) hmv
            , bench "subhask - linsolveSimp" $ nf (linsolveSimp sm1) smv
            , bench "subhask - linsolveDyn"  $ nf (linsolveDyn sm1) smv
--             , bench "subhask - naive"    $ nf (reciprocal sm1 *) smv
--             [
            ]
        ]

    putStrLn "done."
