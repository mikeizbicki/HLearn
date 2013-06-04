{-# LANGUAGE OverlappingInstances #-}
-- {-# LANGUAGE IncoherentInstances #-}

-- | This module contains the functions for plotting distributions using Gnuplot.

module HLearn.Models.Distributions.Visualization.Gnuplot
    (
    -- * Main interface
    plotDistribution
    
    -- ** Plot parameters
    , PlotParams(..)
    , PicType(..)
    , plotFile
    , genPlotParams

    -- * Internal
    , PlottableDistribution(..)
    , Plottable(..)
    , PlotType(..)
        
    -- * Utilities
    , samplesFromMinMax
    ) 
    where

import HLearn.Algebra
import HLearn.Models.Distributions.Common

import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as VU

import Control.Monad
import Data.List
import System.IO
import System.Process

-------------------------------------------------------------------------------
-- data types

data PlotType = Bar | Points | Continuous

data PicType = PNG { pngWidth::Int, pngHeight::Int} | EPS

getExtension :: PicType -> String
getExtension (PNG _ _) = ".png"
getExtension EPS = ".ps"

data PlotParams = PlotParams
    { dataFile :: FilePath
    , gnuFile :: FilePath
    , picFile :: FilePath
    , picType :: PicType
    }

plotFile :: String -> PicType -> PlotParams
plotFile str ext = PlotParams
    { dataFile = str++".dat"
    , gnuFile  = str++".gnu"
    , picFile  = str++(getExtension ext)
    , picType  = ext
    }

-- | provided due to backwards compatibility with the nuclear weapons blog post.
genPlotParams :: String -> a -> PlotParams
genPlotParams str a = plotFile str EPS

-------------------------------------------------------------------------------
-- plotting classes

class (Show t, Ord t) => Plottable t
instance (Show t, Ord t) => Plottable t

-- | In order to plot a distribution, it must be an instance of this class.  You shouldn't need to know the details.
class 
    ( Plottable (Datapoint dist)
    , Plottable (Probability dist), Num (Probability dist)
    , PDF dist
    , MaybeShow (Datapoint dist)
    ) => PlottableDistribution dist where

    samplePoints :: dist -> [Datapoint dist]
    plotType :: dist -> PlotType
    
    pdfL :: dist -> [Probability dist]
    pdfL dist = map (pdf dist) $ samplePoints dist

    plotdata :: dist -> String
    plotdata dist = mconcat [maybeshow (x::Datapoint dist) ++ " " ++ show (pdf dist x::Probability dist) ++"\n" | x <- plotPoints]
        where
            plotPoints = samplePoints dist

class MaybeShow a where
    maybeshow :: a -> String
    
instance (Show a) => MaybeShow (Maybe a) where
    maybeshow (Just a) = show a
    maybeshow Nothing  = "Nothing"

instance (Show a) => MaybeShow a where
    maybeshow = show

-------------------------------------------------------------------------------
-- plotting functions

-- | Call this function to plot your distribution.  You can create the PlotParams manually, or you can use default parameter creating function below.

plotDistribution :: (PlottableDistribution dist) => PlotParams -> dist -> IO ()
plotDistribution params dist = do
    -- Create data file
    putStrLn "Creating data file..."
    datah <- openFile (dataFile params) WriteMode
    hPutStrLn datah $ plotdata dist
    hClose datah
    
    -- Create gnuplot file
    putStrLn "Plotting data"
    gnuh <- openFile (gnuFile params) WriteMode
    hPutStrLn gnuh $ gnuplotHeader params
    hPutStrLn gnuh $ gnuplot params dist
    hClose gnuh
    
    -- Run gnuplot, creating picture
    system $ "gnuplot "++(gnuFile params)++" &"
    putStrLn "done."
    return ()

gnuplotHeader :: PlotParams -> String
gnuplotHeader params
    =  terminal
    ++ "set output \"" ++ (picFile params) ++ "\" \n"
    ++ "unset xtics; unset ytics; unset key \n"
--     ++ "set border 0; set xzeroaxis lt 1; set yzeroaxis lt 1 \n"
    ++ "zero(x)=0\n"
    ++ "set border 2; set style fill solid 1\n"
    where 
        terminal = case picType params of
            EPS -> "set terminal postscript \"Times-Roman\" 25 \n"
                ++ "set size 0.81, 1\n"
--             PNG _ _ -> "png"
            PNG w h -> "set terminal pngcairo size "++show w++","++show h++" enhanced font 'Times-Roman,10' \n"
        
-- class PlotHList t where
--     plotargs :: t -> [(String,String)] -> [String]
-- instance PlotHList (HList '[]) where
--     plotargs HNil _ = []
-- instance (PlotHList (HList xs)) => PlotHList (HList (x ': xs)) where
--     plotargs (x:::xs) (linecolor,bgcolor):colorL = 
--         ["'"++(dataFile params)++"' using 1:"++show i++" lt 1 lw 4 lc rgb '"++linecolor++"' with lines"]
--         :(plotargs xs colorL)

gnuplot :: (PlottableDistribution dist) => PlotParams -> dist -> String
gnuplot params dist
    =  yrange
    ++ plotcmd
    where
        plotcmd = case (plotType dist) of
            Bar -> "set tics scale 0; set xtics nomirror; unset ytics; unset key\n"
                ++ "set xzeroaxis lt 1 lc rgb '#000000'\n"
--                 ++ "set ylabel \"Probability\"\n"
                ++ "set style data histogram; set style histogram cluster gap 1\n"
                ++ "plot '"++(dataFile params)++"' using 2:xticlabels(1) lw 4 linecolor rgb '#0000ff' fs solid 1\n"
            Continuous -> "plot '"++(dataFile params)++"' using 1:2:(zero($2)) lt 1 lw 4 lc rgb '#ccccff' with filledcurves, "
--             Continuous -> "plot '"++(dataFile params)++"' using 1:2 lt 1 lw 4 lc rgb '#ccccff' with filledcurves, "
                        ++ "     '"++(dataFile params)++"' using 1:2 lt 1 lw 4 lc rgb '#0000ff' with lines"
            Points -> "plot '"++(dataFile params)++"' using 1:2 lt 1 lw 4 lc rgb '#0000ff' with points "
        positiveSamples = or $ map (>0) $ pdfL dist
        negativeSamples = or $ map (<0) $ pdfL dist
        yrange = if positiveSamples && negativeSamples
            then ""
            else if positiveSamples
                then "set yrange [0:]\n"
                else "set yrange [:0]\n"


--     plotDistributionL :: PlotParams -> [dist] -> IO ()
--     plotDistributionL params distL = do
--         -- Create data file
--         putStrLn "Creating data file..."
--         datah <- openFile (dataFile params) WriteMode
--         hPutStrLn datah $ plotdataL distL
--         hClose datah
--         
--         -- Create gnuplot file
--         putStrLn "Plotting data"
--         gnuh <- openFile (gnuFile params) WriteMode
--         hPutStrLn gnuh $ gnuplotL params distL
--         hClose gnuh
--         
--         -- Run gnuplot, creating picture
--         system $ "gnuplot "++(gnuFile params)
--         putStrLn "done."
--         return ()
{-
    gnuplotL  :: PlotParams -> [dist] -> String
    gnuplotL params distL
        =  "set terminal postscript \"Times-Roman\" 25 \n"
        ++ "set output \"" ++ (picFile params) ++ "\" \n"
        ++ "unset xtics; unset ytics; unset key \n"
        ++ "set style fill transparent solid 0.5 noborder\n"
        ++ "set border 0; set xzeroaxis lt 1; set yzeroaxis lt 1 \n"
        ++ "plot "
            -- ++(dataFile params)++"' using 1:2 lt 1 lw 4 lc rgb '#ccccff' with filledcurves, "
        ++ (mconcat . intersperse ", " $ 
            [ -- "     '"++(dataFile params)++"' using 1:"++show i++" fs solid 1.0 lc rgb '"++bgcolor++"'" -- "' with filledcurves,"
            "     '"++(dataFile params)++"' using 1:"++show i++" lt 1 lw 4 lc rgb '"++linecolor++"' with lines"
            | (i,linecolor,bgcolor,_dist) <- zip4 [2..] linecolorL bgcolorL distL
            ])
        where 
            linecolorL = ["#0000ff", "#00ff00", "#ff0000"]
            bgcolorL   = ["#ccccff", "#ccffcc", "#ffcccc"]-}
--     plotdataL :: [dist] -> String
--     plotdataL distL = mconcat 
--                             [show (x::Datapoint dist) ++ (mconcat
--                                 [ " " ++ show (pdf dist x::Probability dist) 
--                                 | dist <- distL
--                                 ])
--                                 ++"\n" 
--                             | x <- plotPoints
--                             ]
--         where
--             plotPoints = mconcat $ fmap samplePoints distL




{-instance PlottableDistribution (KDE Double) where
    plotdata dist = mconcat [show (x::Double) ++ " " ++ show (pdf dist x::Double) | x <- plotPoints]
        where
            plotPoints = VU.toList $ samplePoints $ getparams dist

    gnuplot params dist
        =  "set terminal postscript \"Times-Roman\" 25"
        ++ "set output \"" ++ (picFile params) ++ "\""
        ++ "unset xtics; unset ytics; unset key"
        ++ "set border 0; set xzeroaxis lt 1; set yzeroaxis lt 1"
        ++ "plot '"++(dataFile params)++"' using 1:2 lt 1 lw 4 lc rgb '#ccccff' with filledcurves, \\"
        ++ "     '"++(dataFile params)++"' using 1:2 lt 1 lw 4 lc rgb '#0000ff' with lines"-}
        
{-instance PlottableDistribution (Gaussian Double) where
    
    minx dist = (meanG dist)-5*(sqrt $ varG dist)
    maxx dist = (meanG dist)+5*(sqrt $ varG dist)-}
    
        
-- instance (Show label, Show prob, Num prob, Ord prob) => PlottableDistribution (Categorical label prob) label where
--     plotdata dist = concat $ do
--         (label,prob) <- dist2list dist
--         return $ show label++" "++show prob++"\n"
--         
--     gnuplot params dist
--         = "set terminal postscript \"Times-Roman\" 25; set output \""++ (picFile params) ++ "\"\n"
--         ++"set tics scale 0; set xtics nomirror; unset ytics; unset key\n"
--         ++"set border 2; set xzeroaxis lt 1\n"
--         ++"set ylabel \"Probability\"\n"
--         ++yrange
--         ++"set style fill border -1\n"
--         ++"set style data histogram; set style histogram cluster gap 1\n"
--         ++"plot '"++(dataFile params)++"' using 2:xticlabels(1)\n"
--         where
--             positiveSamples = or $ map (\(k,v) -> v>0) $ dist2list dist
--             negativeSamples = or $ map (\(k,v) -> v<0) $ dist2list dist
--             yrange = if positiveSamples && negativeSamples
--                 then ""
--                 else if positiveSamples
--                     then "set yrange [0:]\n"
--                     else "set yrange [:0]\n"

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

samplesFromMinMax min max = fmap (\x -> x/numsamples*(max-min)+min) [0..numsamples]
    where 
        numsamples = 1000