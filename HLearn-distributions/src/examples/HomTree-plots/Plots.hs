import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Gnuplot.Distributions
    
g1 = train [1,2,3,4] :: Gaussian Double
g2 = train [2,3,4,5] :: Gaussian Double
g3 = train [4,5,6,7] :: Gaussian Double
g4 = train [7,8,9,10] :: Gaussian Double

gfat = train [0.5,4.5] :: Gaussian Double

main = do
    plotDistributionL (plotFile "g-highoverlap") [g1,g2]
    plotDistributionL (plotFile "g-lowoverlap") [g1,g3]
    
    plotDistributionL (plotFile "g-multidecision-fat") [g1,gfat]
    plotDistributionL (plotFile "g-multidecision-multi") [g1,g3,g4]
--     plotDistribution (plotFile "g1") g1
--     plotDistribution (plotFile "g2") g2
--     plotDistribution (plotFile "g3") g3
--     plotDistribution (plotFile "gfat") gfat