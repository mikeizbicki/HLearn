{-# OPTIONS_GHC -fllvm -O2 #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Criterion
import Criterion.Main
import Prelude (IO, toRational)

import qualified Data.Vector.Storable as VS

import SubHask
import SubHask.Algebra.HMatrix
import HLearn.History
import HLearn.History.DisplayMethods
import HLearn.Optimization.Common

-- {-# NOINLINE emptyReturn #-}
-- emptyReturn :: (HistoryMonad m, Reportable m Double) => Int -> m Double
emptyReturn n = optimize return (10::Double) (maxIterations n)

emptyReport n = optimize report (10::Double) (maxIterations n)

emptyCollectReports (n::Int) = optimize 
    (\itr -> optimize report itr ( maxIterations 10) )
    (10::Double) 
    (maxIterations $ round $ toRational n / 10)

main = do
    defaultMain 
        [ bgroup "simpleHistory"
            [ bgroup "emptyReturn"
                [ bench "10"      $ nf (runSimpleHistory . emptyReturn) 10
                , bench "100"     $ nf (runSimpleHistory . emptyReturn) 100
                , bench "1000"    $ nf (runSimpleHistory . emptyReturn) 1000
                , bench "10000"   $ nf (runSimpleHistory . emptyReturn) 10000
                , bench "100000"  $ nf (runSimpleHistory . emptyReturn) 100000
                , bench "1000000" $ nf (runSimpleHistory . emptyReturn) 1000000
                ]
            , bgroup "emptyReport"
                [ bench "10"      $ nf (runSimpleHistory . emptyReport) 10
                , bench "100"     $ nf (runSimpleHistory . emptyReport) 100
                , bench "1000"    $ nf (runSimpleHistory . emptyReport) 1000
                , bench "10000"   $ nf (runSimpleHistory . emptyReport) 10000
                , bench "100000"  $ nf (runSimpleHistory . emptyReport) 100000
                , bench "1000000" $ nf (runSimpleHistory . emptyReport) 1000000
                ]
            , bgroup "collectReports . emptyReport"
                [ bench "10"      $ nf (runSimpleHistory . collectReports . emptyReport) 10
                , bench "100"     $ nf (runSimpleHistory . collectReports . emptyReport) 100
                , bench "1000"    $ nf (runSimpleHistory . collectReports . emptyReport) 1000
                , bench "10000"   $ nf (runSimpleHistory . collectReports . emptyReport) 10000
                , bench "100000"  $ nf (runSimpleHistory . collectReports . emptyReport) 100000
                , bench "1000000" $ nf (runSimpleHistory . collectReports . emptyReport) 1000000
                ]
            , bgroup "emptyCollectReports"
                [ bench "10"      $ nf (runSimpleHistory . emptyCollectReports) 10
                , bench "100"     $ nf (runSimpleHistory . emptyCollectReports) 100
                , bench "1000"    $ nf (runSimpleHistory . emptyCollectReports) 1000
                , bench "10000"   $ nf (runSimpleHistory . emptyCollectReports) 10000
                , bench "100000"  $ nf (runSimpleHistory . emptyCollectReports) 100000
                , bench "1000000" $ nf (runSimpleHistory . emptyCollectReports) 1000000
                ]
            ]
        , bgroup "dynamicHistory"
            [ bgroup "emptyReturn"
                [ bench "10"      $ nfIO $ runDynamicHistory idDisplayMethod $ emptyReturn 10
                , bench "100"     $ nfIO $ runDynamicHistory idDisplayMethod $ emptyReturn 100
                , bench "1000"    $ nfIO $ runDynamicHistory idDisplayMethod $ emptyReturn 1000
                , bench "10000"   $ nfIO $ runDynamicHistory idDisplayMethod $ emptyReturn 10000
                , bench "100000"  $ nfIO $ runDynamicHistory idDisplayMethod $ emptyReturn 100000
                , bench "1000000" $ nfIO $ runDynamicHistory idDisplayMethod $ emptyReturn 1000000
                ]
            , bgroup "emptyReport"
                [ bench "10"      $ nfIO $ runDynamicHistory idDisplayMethod $ emptyReport 10
                , bench "100"     $ nfIO $ runDynamicHistory idDisplayMethod $ emptyReport 100
                , bench "1000"    $ nfIO $ runDynamicHistory idDisplayMethod $ emptyReport 1000
                , bench "10000"   $ nfIO $ runDynamicHistory idDisplayMethod $ emptyReport 10000
                , bench "100000"  $ nfIO $ runDynamicHistory idDisplayMethod $ emptyReport 100000
                , bench "1000000" $ nfIO $ runDynamicHistory idDisplayMethod $ emptyReport 1000000
                ]
            , bgroup "emptyCollectReports"
                [ bench "10"      $ nfIO $ runDynamicHistory idDisplayMethod $ emptyCollectReports 10
                , bench "100"     $ nfIO $ runDynamicHistory idDisplayMethod $ emptyCollectReports 100
                , bench "1000"    $ nfIO $ runDynamicHistory idDisplayMethod $ emptyCollectReports 1000
                , bench "10000"   $ nfIO $ runDynamicHistory idDisplayMethod $ emptyCollectReports 10000
                , bench "100000"  $ nfIO $ runDynamicHistory idDisplayMethod $ emptyCollectReports 100000
                , bench "1000000" $ nfIO $ runDynamicHistory idDisplayMethod $ emptyCollectReports 1000000
                ]
            ]
        ]
