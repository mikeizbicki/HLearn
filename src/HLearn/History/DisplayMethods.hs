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
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import Numeric
import System.Console.ANSI
import System.CPUTime
import System.IO

import Control.Lens
import Pipes
import Pipes.Core
import HLearn.History
import qualified HLearn.Algebra.LinearAlgebra as LA

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

(|||) :: DisplayFilter' s1 -> DisplayMethod' s2 -> DisplayMethod' (s1,s2)
(f1 ||| f2) rep = do
    (s1,s2) <- get 
    (pass,s1') <- liftIO $ runStateT (f1 rep) s1
    ((),s2') <- if pass
        then liftIO $ runStateT (f2 rep) s2
        else return ((),s2)
    put (s1',s2')

(===) :: DisplayMethod' s1 -> DisplayMethod' s2 -> DisplayMethod' (s1,s2)
(f1 === f2) rep = do
    (s1,s2) <- get
    ((),s1') <- liftIO $ runStateT (f1 rep) s1
    ((),s2') <- liftIO $ runStateT (f2 rep) s2
    put (s1',s2')

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
    when (nextType == typeOf EndHistory) $ do
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


