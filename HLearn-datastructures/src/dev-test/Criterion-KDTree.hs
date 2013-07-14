
import Criterion.Config
import Criterion.Main
import qualified Data.Vector as V

import HLearn.Algebra
import HLearn.DataStructures.KDTree

myConfig = defaultConfig 
    { cfgPerformGC = ljust True
    , cfgSamples = ljust 20 
    }

main = do
    m <- randmodel
    print $  "depth randmodel="++show (depth m)
    defaultMainWith myConfig (return ())
        [ bench "mindist" $ nf (mindist $ V.fromList [-1e0,-1e0]) m 
        , bench "mindist_noprune" $ nf (mindist_noprune $ V.fromList [-1e0,-1e0]) m 
        ]
