{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

import SubHask
import SubHask.Algebra.Vector
import SubHask.Category.Trans.Derivative

import HLearn.History
import HLearn.Optimization.Univariate
import HLearn.Optimization.Multivariate

import System.IO

--------------------------------------------------------------------------------

main = do
    let x0 = unsafeToModule [1,2,1,2] :: SVector 4 Double
        f = rosenbrock
        lineSearch = lineSearch_brent ( stop_brent 1e-12 || maxIterations 20 )

        stop :: StopCondition a
        stop = maxIterations 20

    let cgd conj = evalHistory $ fminunc_cgd_ conj lineSearch stop x0 f

    putStrLn $ "steepestDescent = " ++ show (fx1 $ cgd steepestDescent)
    putStrLn $ "fletcherReeves  = " ++ show (fx1 $ cgd fletcherReeves)
    putStrLn $ "polakRibiere    = " ++ show (fx1 $ cgd polakRibiere)
    putStrLn $ "hestenesStiefel = " ++ show (fx1 $ cgd hestenesStiefel)

    putStrLn $ "bfgs = " ++ show (fx1 $ evalHistory $ fminunc_bfgs_ lineSearch stop x0 f )
