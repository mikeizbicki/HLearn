import Criterion.Config
import Criterion.Main

import Data.List

myConfig = defaultConfig 
    { cfgPerformGC = ljust True
    , cfgSamples = ljust 5
    }
    
size = 10^6
main = defaultMainWith myConfig (return ())
    [ bench "sort 10000" $ whnf sort [1..100000]
    , bench "sort 20000" $ whnf sort [1..200000]
    , bench "sort 30000" $ whnf sort [1..300000]
    , bench "sort 40000" $ whnf sort [1..400000]
    , bench "sort 50000" $ whnf sort [1..500000]
    ]
