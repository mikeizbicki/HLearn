{-# LANGUAGE DataKinds,TypeOperators,TypeFamilies #-}

import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import HLearn.Algebra
import HLearn.Models.Distributions.Univariate.KernelDensityEstimator
import HLearn.Models.Distributions.Kernels
import HLearn.Models.Distributions.Visualization.Gnuplot

import HLearn.NPHard.Scheduling
-- import HLearn.NPHard.Visualization

type Dist = KDE Cosine (20%10) Double Double

instance HasRing Double where
    type Ring Double = Double

instance Norm Double where
    magnitude x = x

main = do
    xs <- replicateM 1000000 $ randomRIO (0,1)
    print "dataset generated."
    let bp = parallel train xs :: Scheduling 4 Double
    deepseq bp $ print "trained."

kde = do
    let xs = [10,3,12,22,11,8,23,14,13,15,18,0,19,22,9,1]

    forM_ [0..5] $ \i -> do
        let ys = take i xs
        let m = train ys :: Dist 
        plotDistribution (plotFile ("pic"++show i) $ EPS) m

    plotDistribution (plotFile "picfinal" EPS) (train xs :: Dist)
