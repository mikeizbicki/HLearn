{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | This file contains the functions for plotting distributions using Gnuplot.  The interface is still under heavy construction, so it's not very well documented as of yet.

module HLearn.Gnuplot.Distributions
    where

import HLearn.Algebra
import HLearn.Models.Distributions

import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as VU

import Control.Monad
import System.IO
import System.Process

data PlotParams = PlotParams
    { dataFile :: FilePath
    , gnuFile :: FilePath
    , picFile :: FilePath
    }

plotFile :: String -> PlotParams
plotFile str = PlotParams
    { dataFile = str++".dat"
    , gnuFile  = str++".gnu"
    , picFile  = str++".ps"
    }

-- | provided due to backwards compatibility with the nuclear weapons blog post.
genPlotParams :: String -> a -> PlotParams
genPlotParams str a = plotFile str

defPlotParams = PlotParams
    { dataFile = "hlearn-distributions.dat"
    , gnuFile = "hlearn-distributions.gnu"
    , picFile = "hlearn-distributions.ps"
    }

class PlottableDistribution dist where
    plotDistribution :: PlotParams -> dist -> IO ()
    plotDistribution params dist = do
        -- Create data file
        putStrLn "Creating data file..."
        datah <- openFile (dataFile params) WriteMode
        hPutStrLn datah $ plotdata dist
        hClose datah
        
        -- Create gnuplot file
        putStrLn "Plotting data"
        gnuh <- openFile (gnuFile params) WriteMode
        hPutStrLn gnuh $ gnuplot params dist
        hClose gnuh
        
        -- Run gnuplot, creating picture
        system $ "gnuplot "++(gnuFile params)
        putStrLn "done."
        return ()

    plotdata :: dist -> String
    gnuplot  :: PlotParams -> dist -> String
    
instance PlottableDistribution (KDE Double) where
    plotdata dist@(SGJust kde) = mconcat [show (x::Double) ++ " " ++ show (pdf dist x::Double) | x <- plotPoints]
        where
            plotPoints = VU.toList $ samplePoints $ params kde

    gnuplot params dist
        =  "set terminal postscript \"Times-Roman\" 25"
        ++ "set output \"" ++ (picFile params) ++ "\""
        ++ "unset xtics; unset ytics; unset key"
        ++ "set border 0; set xzeroaxis lt 1; set yzeroaxis lt 1"
        ++ "plot '"++(dataFile params)++"' using 1:2 lt 1 lw 4 lc rgb '#ccccff' with filledcurves, \\"
        ++ "     '"++(dataFile params)++"' using 1:2 lt 1 lw 4 lc rgb '#0000ff' with lines"
        
instance (Show label, Show prob, Num prob, Ord prob) => PlottableDistribution (Categorical label prob) where
    plotdata dist@(Categorical pdf) = concat $ do
        (label,prob) <- Map.toList pdf
        return $ show label++" "++show prob++"\n"
        
    gnuplot params (Categorical pdf)
        = "set terminal postscript \"Times-Roman\" 25; set output \""++ (picFile params) ++ "\"\n"
        ++"set tics scale 0; set xtics nomirror; unset ytics; unset key\n"
        ++"set border 2; set xzeroaxis lt 1\n"
        ++"set ylabel \"Probability\"\n"
        ++yrange
        ++"set style fill border -1\n"
        ++"set style data histogram; set style histogram cluster gap 1\n"
        ++"plot '"++(dataFile params)++"' using 2:xticlabels(1)\n"
        where
            positiveSamples = or $ map (\(k,v) -> v>0) $ Map.toList pdf
            negativeSamples = or $ map (\(k,v) -> v<0) $ Map.toList pdf
            yrange = if positiveSamples && negativeSamples
                then ""
                else if positiveSamples
                    then "set yrange [0:]\n"
                    else "set yrange [:0]\n"
{-        =  "set terminal postscript \"Times-Roman\" 25 \n"
        ++ "set output \"" ++ (picFile params) ++ "\" \n"
        ++ "unset xtics; unset ytics; unset key\n"
        ++ "set border 0; set xzeroaxis lt 1; set yzeroaxis lt 1\n"
        ++ "set style data histogram \n"
        ++ "plot '"++(dataFile params)++"' using 1:2 \n"-}
    
-- plotDistribution :: (Distribution dist Double Double) => PlotParams -> dist -> IO ()
-- plotDistribution params dist = do
--     
--     -- Create data file
--     putStrLn "Creating data file..."
--     datah <- openFile (dataFile params) WriteMode
--     forM_ (plotPoints params) $ \x -> do
-- --     forM_ (map (/10) [-50..50]) $ \x -> do
--         hPutStrLn datah $ show (x::Double) ++ " " ++ show (pdf dist x::Double)
--     hClose datah
--     
--     -- Create gnuplot file
--     putStrLn "Plotting data"
--     gnuh <- openFile (gnuFile params) WriteMode
--     hPutStrLn gnuh $ "set terminal postscript \"Times-Roman\" 25"
--     hPutStrLn gnuh $ "set output \"" ++ (picFile params) ++ "\""
--     hPutStrLn gnuh $ "unset xtics; unset ytics; unset key"
--     hPutStrLn gnuh $ "set border 0; set xzeroaxis lt 1; set yzeroaxis lt 1"
--     hPutStrLn gnuh $ "plot '"++(dataFile params)++"' using 1:2 lt 1 lw 4 lc rgb '#ccccff' with filledcurves, \\"
--     hPutStrLn gnuh $ "     '"++(dataFile params)++"' using 1:2 lt 1 lw 4 lc rgb '#0000ff' with lines"
--     hClose gnuh
--     
--     -- Run gnuplot, creating picture
--     system $ "gnuplot "++(gnuFile params)
--     putStrLn "done."
--     return ()
