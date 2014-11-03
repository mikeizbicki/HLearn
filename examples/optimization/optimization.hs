{-# LANGUAGE RebindableSyntax,NoImplicitPrelude,OverloadedLists #-}

import SubHask
import SubHask.Algebra.HMatrix

import HLearn.History
import HLearn.History.DisplayMethods
import HLearn.Optimization.Common
import HLearn.Optimization.GradientDescent
-- import HLearn.Optimization.StochasticGradientDescent
import HLearn.Optimization.LineMinimization.Univariate
import HLearn.Optimization.LineMinimization.Multivariate

import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Generic as VG
import System.IO

sphere :: (Ring a, VG.Vector v a) => v a -> a
sphere v = VG.foldl' (+) zero $ VG.map (\x -> x*x) v

sphere' :: (Module (v a), VG.Vector v a) => v a -> v a
sphere' v = 2*.v

rosenbrock :: (Ring a, VG.Vector v a) => v a -> a
rosenbrock v = go 0 0
    where
        go i tot = if i==VG.length v-1
            then tot
            else go (i+1) $ 100*x*x + (v VG.! i - 1)*(v VG.! i -1)
            where
                x =(v VG.! (i+1) - (v VG.! i)*(v VG.! i))

rosenbrock' :: (Ring a, VG.Vector v a) => v a -> v a
rosenbrock' v = VG.imap go v
    where
        go i x = if i==VG.length v-1
            then pt2 
            else if i== 0
                then pt1
                else pt1+pt2
            where 
                pt1 = 400*x*x*x - 400*xp1*x + 2*x -2
                pt2 = 200*x - 200*xm1*xm1
                xp1 = v VG.! i+1
                xm1 = v VG.! i-1

main = do
    let { f=sphere; f'=sphere' }
--     let { f=rosenbrock; f'=rosenbrock' }

    runDynamicHistory 
        (   summaryStatistics 
        === linearTrace 
        === mkOptimizationPlot 
                (undefined::ConjugateGradientDescent (Vector Double))
                f
                "optplot.dat"
        )
--         ( linearTrace )
        ( conjugateGradientDescent_
            ( lineSearchBrent ( brentTollerance 1e-12 || maxIterations 2 ) )
--             ( backtracking ( maxIterations 5) )
--             ( backtracking ( amijo (1e-4::Double) ) )
            polakRibiere
            f
            f'
            ( [1..10] :: VS.Vector Double )
            ( maxIterations 5
--             || multiplicativeTollerance 1e-12
            )
        )
    putStrLn "done."
