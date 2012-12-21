{-# LANGUAGE FlexibleContexts #-}

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
    }
    
defPlotParams = PlotParams
    { dataFile = "hlearn-distributions.dat"
    , gnuFile = "hlearn-distributions.gnu"
    , picFile = "hlearn-distributions.ps"
    }
    
plotDistribution :: (Distribution dist Double Double) => PlotParams -> dist -> IO ()
plotDistribution params dist = do
    
    -- Create data file
    datah <- openFile (dataFile params) WriteMode
    forM_ (map (/10) [-50..50]) $ \x -> do
        hPutStrLn datah $ show (x::Double) ++ " " ++ show (pdf dist x::Double)
    hClose datah
    
    -- Create gnuplot file
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
    return ()
    
testL = [1,1.1,1.2,0,5,-3,2,1,2.5,2.9::Double]
kde = train' (KDEParams 2 (VU.fromList $ map (/10) [-50..50::Double]) (KernelBox Triangular)) testL :: KDE Double

pdfL = sequence_ $ do
    x <- map (/10) [-50..50]
    let y = pdf kde x :: Double
    seq y (return ())
    return $ putStrLn $ (show (x::Double)) ++ " " ++ show y
