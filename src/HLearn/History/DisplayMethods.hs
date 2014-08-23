{-# LANGUAGE DataKinds #-}
module HLearn.History.DisplayMethods
    where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Monad.Trans
import Data.Dynamic
import Data.List
import Data.Monoid
import Data.Typeable
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import Numeric
import System.Console.ANSI
import System.CPUTime
import System.Directory
import System.IO

import Control.Lens
import Pipes
import Pipes.Core

import HLearn.Algebra hiding (elem)
import HLearn.History
import HLearn.Algebra.LinearAlgebra as LA
import HLearn.Optimization.Common
import HLearn.Optimization.LineMinimization
import HLearn.Optimization.NewtonRaphson

-------------------------------------------------------------------------------
-- display functions

showIndent :: Int -> String
showIndent i = concat $ replicate i " - "

showableReport :: Report -> Bool
showableReport report = case fromDynamic (dyn report) :: Maybe StartCollection of
    Just _ -> False
    Nothing -> True

showCPUTime :: CPUTime -> Int -> String
showCPUTime time len = showEFloat (Just $ len-4-4) (fromIntegral time * 1e-12 :: Double) "" ++ " sec"

padString :: String -> Int -> String
padString a i = take i $ a ++ repeat ' '

pad :: Show a => a -> Int -> String
pad a i = take i $ show a ++ repeat ' '

-------------------

concatDF :: [DisplayFunction] -> DisplayFunction
concatDF fs x = concat $ intersperse "; " $ filter (/="") $ map ($x) fs

displayCPUTime :: DisplayFunction
displayCPUTime x = showEFloat (Just 6) (fromIntegral (cpuTime x) * 1e-12 :: Double) "" ++ " sec"

displayCPUTimeDiff :: DisplayFunction
displayCPUTimeDiff x = showEFloat (Just 6) (fromIntegral (cpuTimeDiff x) * 1e-12 :: Double) "" ++ " sec"

displayNumReports :: DisplayFunction
displayNumReports = show . displayNumReports 

displayReportLevel :: DisplayFunction
displayReportLevel = show . displayReportLevel 

displayStartCollection :: DisplayFunction
displayStartCollection = mkDisplayFunction (show :: StartCollection -> String)

displayType :: DisplayFunction
displayType = head . words . show . dynTypeRep . dyn

displayIndent :: DisplayFunction
displayIndent x = concat $ replicate (reportLevel x) " - "

-------------------

display_fx1 :: DisplayFunction
display_fx1 x = "fx1="++concatDF
    [ display_fx1_NewtonRaphson
    , display_fx1_Brent
    ] x

display_fx1_NewtonRaphson :: DisplayFunction
display_fx1_NewtonRaphson = mkDisplayFunction (show.view _fx1 :: NewtonRaphson (LA.Vector Double) -> String)

display_fx1_Brent :: DisplayFunction
display_fx1_Brent = mkDisplayFunction (show._fw :: Brent Double -> String)

-------------------------------------------------------------------------------

infixr 7 |||
(|||) :: DisplayFilter' s1 -> DisplayMethod' s2 -> DisplayMethod' (s1,s2)
(f1 ||| f2) rep = do
    (s1,s2) <- get 
    (passFilter,s1') <- liftIO $ runStateT (f1 rep) s1
    ((),s2') <- if passFilter || globalPass
        then liftIO $ runStateT (f2 rep) s2
        else return ((),s2)
    put (s1',s2')
    where
        globalPass = dynTypeRep (dyn rep) == typeOf StartHistory
                  || dynTypeRep (dyn rep) == typeOf EndHistory

infixl 6 ===
(===) :: DisplayMethod' s1 -> DisplayMethod' s2 -> DisplayMethod' (s1,s2)
(f1 === f2) rep = do
    (s1,s2) <- get
    ((),s1') <- liftIO $ runStateT (f1 rep) s1
    ((),s2') <- liftIO $ runStateT (f2 rep) s2
    put (s1',s2')

-------------------

firstCall :: Monad m => Report -> m () -> m ()
firstCall rep f = forEach StartHistory rep $ \_ -> f

lastCall :: Monad m => Report -> m () -> m ()
lastCall rep f = forEach EndHistory rep $ \_ -> f 

forEach :: forall a m. (Typeable a, Monad m) => a -> Report -> (a -> m ()) -> m ()
forEach a rep f = case fromDynamic $ dyn rep :: Maybe a of
    Nothing -> return ()
    Just x -> f x
    

-------------------------------------------------------------------------------

removeLineMin :: DisplayFilter
removeLineMin rep = case head $ words $ show $ dynTypeRep $ dyn rep of
    "Brent" -> return False
    "LineBracket" -> return False
    otherwise -> return True

-------------------------------------------------------------------------------

data SummaryStatistics = SummaryStatistics
    { numOccurances :: Int 
    , totalCPUTime  :: CPUTime
    }
    deriving (Show,Typeable)

summaryStatistics :: DisplayMethod' (Map.Map TypeRep SummaryStatistics)
summaryStatistics rep = do
    m <- get
    let nextType = dynTypeRep $ dyn rep
        newStats = case Map.lookup nextType m of
            Nothing -> SummaryStatistics
                { numOccurances = 1
                , totalCPUTime = cpuTimeDiff rep
                }
            Just oldStats -> oldStats
                { numOccurances = numOccurances oldStats+1
                , totalCPUTime = totalCPUTime oldStats + cpuTimeDiff rep
                }
    put $ Map.insert nextType newStats m 
    lastCall rep $ do
        liftIO $ putStrLn "======================================================="
        liftIO $ putStrLn "| type                 | numOccurances | totalCPUTime | "
        liftIO $ putStrLn "======================================================="
        forM (Map.toList m) $ \(k,v) -> do
            liftIO $ putStr $ "| "
            liftIO $ putStr $ padString (head $ words $ show k) 20
            liftIO $ putStr $ " | "
            liftIO $ putStr $ pad (numOccurances v) 13
            liftIO $ putStr $ " | "
            liftIO $ putStr $ showCPUTime (totalCPUTime v) 12
            liftIO $ putStrLn " |"
        liftIO $ putStrLn "======================================================="

-------------------------------------------------------------------------------

defaultTrace :: DisplayFunction
defaultTrace = concatDF
    [ displayIndent
    , displayType
    , display_fx1
    , displayCPUTime
    , displayCPUTimeDiff
    ]

linearTrace :: DisplayMethod
linearTrace = linearTrace' defaultTrace

linearTrace' :: DisplayFunction -> DisplayMethod' ()
linearTrace' f rep = do 
    when (showableReport rep) $ liftIO $ putStrLn $ f rep

compactTrace :: DisplayMethod' Int
compactTrace = compactTrace' defaultTrace

compactTrace' :: DisplayFunction -> DisplayMethod' Int
compactTrace' f rep = do
    prevLevel <- get
    liftIO $ do
        when (prevLevel == reportLevel rep) $ do
            cursorUpLine 1
            clearFromCursorToScreenEnd
        when (prevLevel >  reportLevel rep) $ do
            cursorUpLine 1
            clearFromCursorToScreenEnd
        when (prevLevel <  reportLevel rep) $ do
--                     putStrLn ""
            clearFromCursorToScreenEnd
    put $ reportLevel rep

-------------------------------------------------------------------------------

data PassCount 
    = InPass Int Int
    | NotInPass Int
    
instance Monoid PassCount where
    mempty = NotInPass 0

allowPasses :: forall a. Typeable a => a -> [Int] -> DisplayFilter' PassCount
allowPasses a xs rep = case fromDynamic $ dyn rep :: Maybe a of
    Nothing -> do
        pc <- get
        case pc of 
            InPass i lvl -> when (reportLevel rep < lvl) $ put $ NotInPass i
            otherwise -> return ()
        return False
    Just _ -> do
        pc <- get
        case pc of
            InPass i lvl -> return $ (reportLevel rep == lvl) && i `elem` xs
            NotInPass i -> do
                put $ InPass (i+1) $ reportLevel rep
                return $ False

allowFirstPass :: forall a. Typeable a => a -> DisplayFilter' PassCount
allowFirstPass a = allowPasses a [1]

-------------------

type OptimizationPlot opt = [V.Vector (Scalar opt)]
    

optDP :: forall opt v v' a. 
    ( Has_fx1 opt v
    , Has_x1 opt v
    , Typeable (opt v)
    , v' a ~ Tensor 1 v
    , Scalar v ~ Scalar (opt v)
    , a ~ Scalar v
    , VG.Vector v' a
    ) => opt v -> Report -> V.Vector (Scalar (opt v))
optDP _ rep = VG.fromList $ (opt^.fx1) : (VG.toList $ opt ^. x1)
    where 
        opt = case fromDynamic $ dyn rep :: Maybe (opt v) of
            Just x -> x
            Nothing -> error "optDP: dynamic and static type mismatch"

mkOptimizationPlot :: forall opt v v' a. 
    ( Has_fx1 opt v
    , Has_x1 opt v
    , Has_f opt v
    , Show (Tensor 0 v)
    , Show (Tensor 1 v)
    , Typeable (opt v)
    , Show a
    , v' a ~ Tensor 1 v
    , Scalar v ~ Scalar (opt v)
    , a ~ Scalar v
    , VG.Vector v' a
    , Ord a
    , InnerProduct (Vector a)
    , Num (Scalar (Vector a))
    , Module (Vector a)
    , a ~ Tensor 0 a
    ) => opt v -> FilePath -> DisplayMethod' [opt v]
mkOptimizationPlot opt path rep = do
    xs <- get



    forEach (undefined::opt v) rep $ \opt -> do
--         let x = optDP opt rep
--         put $ x:xs
        put $ opt:xs

    lastCall rep $ liftIO $ do
        tmpdir <- mkTmpFileName path
        createDirectory tmpdir
        hopt <- openFile (tmpdir++"/opt.dat") WriteMode
        forM_ (reverse xs) $ \x -> do
            hPutStr   hopt $ show $ x^.fx1
            hPutStr   hopt " "
            hPutStrLn hopt $ map (\x -> if x==',' then ' ' else x) $ init $ tail $ show (VG.toList $ x^.x1) 
        hClose hopt

        let minx = (head xs)^.x1
            numdim = VG.length minx
            miny = minimum $ map (^.fx1) xs
            maxy = maximum $ map (^.fx1) xs
            yrange = maxy-miny

        let basis = V.fromList $ 
                [ VG.generate numdim (\j -> if i==j then 1 else 0) :: v' a
                | i <- [0..numdim-1]
                ] 

        let f x = (head xs)^.flens $ x

        forM_ [0..length xs-1] $ \itr -> do
            forM_ [0..numdim-1] $ \i -> do
                let basisvec = basis VG.! i
                    itrx = reverse xs !! itr
                hfunc <- openFile (tmpdir++"/func-itr"++show itr++"-basis"++show i++".dat") WriteMode
                let col = map (\x -> inner (x^.x1) basisvec) xs
                    biggest = maximum col
                    smallest = minimum col
                let start = smallest - (biggest-smallest)*0.2
                    end = biggest + (biggest-smallest)*0.2
                    numstep = 100::Int
                    step = (end-start)/(fromIntegral numstep)
                forM [0..numstep-1] $ \j -> do
                    let xpos = start+step*(fromIntegral j)
                    hPutStr   hfunc $ show xpos
                    hPutStr   hfunc " "
                    hPutStrLn hfunc $ show $ f $ ((itrx^.x1) <> ((negate $ inner (itrx^.x1) basisvec) .* basisvec)) <> (xpos .* basisvec)
                hClose hfunc

        -- output gnuplot file
        hgnuplot <- openFile (tmpdir++"/mkplots.gnu") WriteMode
        hPutStrLn hgnuplot $
            "#!/usr/bin/gnuplot \n\ 
            \ set terminal postscript 'Times-Roman' 14 \n\
            \ set output 'plots.ps' \n\
            \ unset xtics \n\
            \ unset ytics \n\
            \ set yrange ["++show (miny-0.1*yrange)++":"++show (maxy+0.1*yrange)++"]\n\
            \ unset key \n\
            \ set xzeroaxis lt 1 \n\
            \ set yzeroaxis lt 1 \n\
            \ set tmargin 0.25 \n\
            \ set lmargin 1 \n\
            \ set rmargin 1 \n\
            \ set bmargin 1 \n\
            \ set title offset 0,-1,0 \n\
            \ "
        forM [0..length xs-1] $ \itr -> do
            hPutStrLn hgnuplot $
                " ################################################# \n\
                \ \n\
                \ set multiplot layout "++numImage2layout numdim++" title 'iteration "++show itr++"'\n"
            forM [0..VG.length basis-1] $ \i -> do
                hPutStrLn hgnuplot $ 
                    " set title 'basis "++show i++"' \n\
                    \ plot 'func-itr"++show itr++"-basis"++show i++".dat' using 1:2 lt 1 lw 4 lc rgb '#000000' with lines,\\\n\
                    \      'opt.dat' using "++show (i+2)++":1 lt 1 lw 1 lc rgb '#ff0000' with lines \n\
                    \ \n"
        hClose hgnuplot

numImage2layout :: Int -> String
numImage2layout i = show rows++","++show cols 
    where
        rows = i `div` cols + if i`mod`cols == 0 then 0 else 1
        cols = ceiling $ sqrt (fromIntegral i :: Double) :: Int

-- mkPlotBasis :: Double -> Double -> opt -> v -> FilePath -> IO ()
-- mkPlotBasis biggest smallest opt basisvec filepath = do
--     hfunc <- openFile filepath WriteMode
--     let start = smallest - (biggest-smallest)*0.2
--         end = biggest + (biggest-smallest)*0.2
--         numstep = 100::Int
--         step = (end-start)/(fromIntegral numstep)
--     forM [0..numstep-1] $ \j -> do
--         let xpos = start+step*(fromIntegral j)
--         hPutStr   hfunc $ show xpos
--         hPutStr   hfunc " "
--                 hPutStrLn hfunc $ show $ f $ (minx <> ((negate $ inner minx basisvec) .* basisvec)) <> (xpos .* basisvec)
--             hClose hfunc
         
-- mkOptimizationPlot _ path rep = case fromDynamic $ dyn rep :: Maybe (opt v) of
--     Nothing ->  return ()
--     Just opt -> liftIO $ do
--         hout <- openFile path AppendMode
--         hPutStrLn hout (optDP opt rep)
--         firstCall rep $ do
--             hPutStrLn hout "first"
--         lastCall rep $ do
--             hPutStrLn hout "last"
--         hClose hout
                
mkTmpFileName :: FilePath -> IO FilePath
mkTmpFileName path = go 0
    where
        go i = do
            let numstr = show i
                path' = path ++ "." ++ (replicate (4-length numstr) '0') ++ numstr
            b1 <- doesFileExist path' 
            b2 <- doesDirectoryExist path'
            if b1 || b2
                then go $ i+1
                else return path'
                 
    

-------------------------------------------------------------------------------

-- historyToFile :: FilePath -> DisplayMethod
-- historyToFile filename = do
--     hout <- liftIO $ openFile filename ReadWriteMode
-- 
--     let go = do
--             next <- await
--             case next of
--                 Nothing -> return ()
--                 Just x -> do 
--                     liftIO $ hPrint hout next
--                     go
--     go
-- 
--     liftIO $ hClose hout

-------------------------------------------------------------------------------


