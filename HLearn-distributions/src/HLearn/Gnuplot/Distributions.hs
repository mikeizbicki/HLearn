{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HLearn.Gnuplot.Distributions
    where

import HLearn.Algebra
import HLearn.Models.Distributions

import qualified Data.Vector.Unboxed as VU

import Control.Monad
import System.IO
import System.Process

data PlotParams = PlotParams
    { dataFile :: FilePath
    , gnuFile :: FilePath
    , picFile :: FilePath
    , plotPoints :: [Double]
    }

class PlottableDistribution dist where
    genPlotPoints :: dist -> [Double]

instance PlottableDistribution (KDE Double) where
    genPlotPoints (SGJust kde) = VU.toList $ samplePoints $ params kde

genPlotParams :: (PlottableDistribution dist) => String -> dist -> PlotParams
genPlotParams str dist = PlotParams
    { dataFile = str++".dat"
    , gnuFile  = str++".gnu"
    , picFile  = str++".ps"
    , plotPoints = genPlotPoints dist
    }

defPlotParams = PlotParams
    { dataFile = "hlearn-distributions.dat"
    , gnuFile = "hlearn-distributions.gnu"
    , picFile = "hlearn-distributions.ps"
    , plotPoints = [-5..5]
    }
    
plotDistribution :: (Distribution dist Double Double) => PlotParams -> dist -> IO ()
plotDistribution params dist = do
    
    -- Create data file
    putStrLn "Creating data file..."
    datah <- openFile (dataFile params) WriteMode
    forM_ (plotPoints params) $ \x -> do
--     forM_ (map (/10) [-50..50]) $ \x -> do
        hPutStrLn datah $ show (x::Double) ++ " " ++ show (pdf dist x::Double)
    hClose datah
    
    -- Create gnuplot file
    putStrLn "Plotting data"
    gnuh <- openFile (gnuFile params) WriteMode
    hPutStrLn gnuh $ "set terminal postscript \"Times-Roman\" 25"
    hPutStrLn gnuh $ "set output \"" ++ (picFile params) ++ "\""
    hPutStrLn gnuh $ "unset xtics; unset ytics; unset key"
    hPutStrLn gnuh $ "set border 0; set xzeroaxis lt 1; set yzeroaxis lt 1"
    hPutStrLn gnuh $ "plot '"++(dataFile params)++"' using 1:2 lt 1 lw 4 lc rgb '#ccccff' with filledcurves, \\"
    hPutStrLn gnuh $ "     '"++(dataFile params)++"' using 1:2 lt 1 lw 4 lc rgb '#0000ff' with lines"
    hClose gnuh
    
    -- Run gnuplot, creating picture
    system $ "gnuplot "++(gnuFile params)
    putStrLn "done."
    return ()
    
-- testL = [1,1.1,1.2,0,5,-3,2,1,2.5,2.9::Double]
-- kde = train' (KDEParams 1 (VU.fromList $ map (/10) [-50..50::Double]) (KernelBox Gaussian)) testL :: KDE Double
-- 
-- pdfL = sequence_ $ do
--     x <- map (/10) [-50..50]
--     let y = pdf kde x :: Double
--     seq y (return ())
--     return $ putStrLn $ (show (x::Double)) ++ " " ++ show y
