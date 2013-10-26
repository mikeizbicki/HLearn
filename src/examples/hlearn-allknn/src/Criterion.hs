{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

import Criterion.Config
import Criterion.Main

import qualified Data.Vector.Unboxed as VU

import HLearn.Algebra
import HLearn.Metrics.Lebesgue
    
myConfig = defaultConfig 
    { cfgPerformGC = ljust True
    , cfgSamples = ljust 100
    }
    
-- dp1 = VU.fromList [0,1,2,3,4,5::Float]
-- dp2 = VU.fromList [1,2,2,3,5,3::Float]

dp1 = VU.fromList [938.9631020880993,-233.25671801133808,-648.1827167669653,-228.29292507706248,-32.75766512157111,-851.3582790230391,-630.3710709651775,621.0358456363426,-830.6039289197575,-278.27300240428724,900.1007471115579,522.7556088089011,-423.36901546705303,-447.6307801685573,-921.3020269478691,352.49080369569515,108.63423421860193,227.2457251161893,393.20881883216884,-230.37943386957727 :: Float]

dp2 = VU.fromList [253.63016824481815,12.635772076712328,792.1728155747198,444.175100630162,-191.01848935888574,-300.6034639102227,-133.81795250857851,-84.277832883173,-356.77272051576267,926.1080947107837,-569.5068954204559,-984.932561515461,-485.2108919989828,866.0891839562528,-600.6717567986852,-330.22072306055566,-272.5317925901056,351.8343308265196,1.6537325992532033,859.423945530244 :: Float]

distancetests = 
    [ bench "L2" $ nf (distance (L2 dp1)) (L2 dp2)
    , bench "L2'" $ nf (distance (SquaredL2 dp1)) (SquaredL2 dp2)
    , bench "L1" $ nf (distance (L1 dp1)) (L1 dp2)
    , bench "Linf" $ nf (distance (Linf dp1)) (Linf dp2)
    ]

floattests x y =
    [ bench "id" $ nf id x
    , bench "+" $ nf (+x) y
    , bench "-" $ nf (\y -> x-y) y
    , bench "*" $ nf (*x) y
    , bench "/" $ nf (/x) y
    , bench "sqrt" $ nf sqrt x
    ]

main = defaultMainWith myConfig (return ()) distancetests
-- main = defaultMainWith myConfig (return ()) $ (floattests (1.1::Float) (48.78::Float)) ++ floattests (1.1::Double) (48.78::Double)
