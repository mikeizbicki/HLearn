{-# LANGUAGE RankNTypes,ImpredicativeTypes,DataKinds #-}
module HLearn.Optimization.Trace
    where

import Control.DeepSeq
import Control.Lens
import qualified Data.DList as DList
import Data.Dynamic
import Data.List
import Data.Typeable
import Debug.Trace (trace)
import Numeric

import HLearn.Algebra
import HLearn.Algebra.LinearAlgebra 
import HLearn.History
import HLearn.Optimization.Common
import HLearn.Optimization.LineMinimization
import HLearn.Optimization.QuasiNewton
import HLearn.Optimization.NewtonRaphson
-- import HLearn.Optimization.GradientDescent

-------------------------------------------------------------------------------
-- main functions

type ReportInfo = Report -> [String]

-- ioHistory :: [ReportInfo] -> History b -> IO b
-- ioHistory fs m = do
--     (a,log) <- runHistory m
--     mapM_ putStrLn $ traceDynamic 0 log
--     return a
--     where
--         traceDynamic :: Int -> [Report] -> [String]
--         traceDynamic i xs = concatMap (traceReport fs i) xs

-- traceReport :: [Report -> [String]] -> Int -> Report -> [String]
-- traceReport fs i x = {-# SCC traceReport #-} if i>=maxdepth+1
--     then []
--     else case fromDynamic (dyn x) :: Maybe (DList.DList Report) of
--         Nothing -> map (traceSpacer i++) $ concatMap ($x) fs
--         Just xs -> if i>=maxdepth
--             then []
--             else [traceSpacer (i+1)]++concatMap (traceReport fs (i+2)) (DList.toList xs)
--     where
--         maxdepth=6

traceSpacer :: Int -> String
traceSpacer 0 = ""
traceSpacer 1 = " . "
traceSpacer i = traceSpacer (i-1)++" . "

-------------------------------------------------------------------------------
-- printing specific types

-- traceDList :: Report -> String
-- traceDList opt = case fromDynamic (dyn opt) :: Maybe (DList.DList Report) of
--     Nothing -> ""
--     Just x -> show (dyn opt) -- ++" -- "++show (length $ DList.toList x)++" -- "++show (dyn $ DList.head x)

---------------------------------------

traceBracket :: Report -> [String]
traceBracket opt = case fromDynamic (dyn opt) :: Maybe (LineBracket Double) of
    Nothing -> []
    Just x -> 
        [ show (dyn opt)
        ++"; fa="++showDouble (_fa x)
        ++"; fb="++showDouble (_fb x)
        ++"; fc="++showDouble (_fc x)
--         ++"; ax="++showDouble (_ax x)
--         ++"; bx="++showDouble (_bx x)
--         ++"; cx="++showDouble (_cx x)
        ]

traceBrent :: Report -> [String]
traceBrent opt = case fromDynamic (dyn opt) :: Maybe (Brent Double) of
    Nothing -> []
    Just x -> 
        [ trace_itr undefined opt++"; "++show (dyn opt)
        ++"; fv="++showDouble (_fv x)
        ++"; fx="++showDouble (_fx x)
        ++"; fw="++showDouble (_fw x)
--         ++"; v="++showDouble (_v x)
--         ++"; x="++showDouble (_x x)
--         ++"; w="++showDouble (_w x)
        ]

traceBacktracking :: forall v. (Tensor 0 v ~ Double, Typeable v) => v -> Report -> [String]
traceBacktracking _ opt = case fromDynamic (dyn opt) :: Maybe (Backtracking v) of
    Nothing -> []
    Just x -> 
        [ trace_itr undefined opt++"; "++show (dyn opt)
        ++"; x="++showDouble (_bt_x x)
        ++"; fx="++showDouble (_bt_fx x)
--         ++"; fv="++showDouble (_fv x)
--         ++"; fx="++showDouble (_fx x)
--         ++"; fw="++showDouble (_fw x)
--         ++"; v="++showDouble (_v x)
--         ++"; x="++showDouble (_x x)
--         ++"; w="++showDouble (_w x)
        ]

-- traceGSS :: Report -> [String]
-- traceGSS opt = case fromDynamic (dyn opt) :: Maybe (GoldenSectionSearch Double) of
--     Nothing -> []
--     Just x -> 
--         [ trace_itr undefined opt++"; "++show (dyn opt)++"; fx1="++showDoubleLong (x^.fx1)++"; x1="++showDoubleLong (x^.x1)]

---------------------------------------

traceBFGS :: Report -> [String]
traceBFGS = traceFunk (undefined :: BFGS (Vector Double))

traceNewtonRaphson :: Report -> [String]
traceNewtonRaphson = traceFunk (undefined :: NewtonRaphson (Vector Double))

traceFunk :: forall v a. 
    ( Typeable v
    , Typeable a
    , Show (Scalar a)
    , Floating (Scalar a)
    , InnerProduct a
    , Scalar a ~ Double
    , Has_fx1 v a
    , Has_f'x1 v a
    , Has_stepSize v a
    ) => v a -> Report -> [String]
traceFunk _ opt = case fromDynamic (dyn opt) :: Maybe (v a) of
    Nothing -> []
    Just x -> [concat $ intersperse "; " $ map (\f -> f x opt) fs]
    where
        fs = [trace_itr,trace_eventType,trace_fx1,trace_f'x1,trace_stepSize,trace_sec]


-------------------------------------------------------------------------------

traceType :: forall a. Typeable a => Proxy a -> [TraceResult a] -> Report -> [String]
traceType _ fs e = case fromDynamic (dyn e) :: Maybe a of
    Nothing -> []
    Just opt -> [concat $ intersperse "; " $ map (\f -> disp $ f opt e) fs]
    where
        disp (name,val) = if longForm name
            then name++"="++val
            else val

        longForm "shortName" = False
        longForm "longName" = False
        longForm _ = True

-------------------

type TraceResult a = a -> Report -> (String,String)

numItr :: TraceResult a
numItr _ e = ("itr",show (numReports e))

shortName :: TraceResult a
shortName _ e = ("shortName",head $ words $ drop 2 $ show $ dyn e)

longName :: TraceResult a
longName _ e = ("longName", init $ init $ drop 2 $ show $ dyn e)

showSeconds :: TraceResult a
showSeconds _ e = ("sec",showEFloat (Just 4) ((fromIntegral $ cpuTimeDiff e)*1e-12 :: Double) "")

show_fx1 :: (Has_fx1 opt a, RealFloat (Scalar a)) => TraceResult (opt a)
show_fx1 a _ = ("fx1",showEFloat (Just 12) (a^.fx1) "")

show_f'x1 :: (Has_f'x1 opt a, RealFloat (Scalar a)) => TraceResult (opt a)
show_f'x1 a _ = ("|f'x1|",showEFloat (Just 4) (innerProductNorm $ a^.f'x1) "")

-------------------

trace_itr :: a -> Report -> String
trace_itr _ e = "itr="++show (numReports e)

trace_sec :: a -> Report -> String
trace_sec _ e = "sec="++showEFloat (Just 4) ((fromIntegral $ cpuTimeDiff e)*1e-12 :: Double) ""

-- trace_eventType :: a -> Report -> String
-- trace_eventType _ e = take 10 $ head $ words $ drop 2 $ show $ dyn e

trace_eventType :: a -> Report -> String
trace_eventType _ e = take 10 $ head $ words $ drop 2 $ show $ dyn e

trace_fx1 :: (RealFloat (Scalar a), Has_fx1 opt a) => opt a -> Report -> String
trace_fx1 a _ = "fx1="++showEFloat (Just 12) (a^.fx1) ""

trace_f'x1 :: (RealFloat (Scalar a), ValidTensor1 a, Has_f'x1 opt a) => opt a -> Report -> String
-- trace_f'x1 :: (RealFloat (Scalar a), InnerProduct (Tensor 1 a), Has_f'x1 opt a) => opt a -> Report -> String
trace_f'x1 a _ = "|f'x1|="++showEFloat (Just 4) (innerProductNorm $ a^.f'x1) ""

trace_stepSize :: (RealFloat (Scalar a), Has_stepSize opt a) => opt a -> Report -> String
trace_stepSize a _ = "step="++showEFloat (Just 4) (a^.stepSize) ""

-------------------------------------------------------------------------------
-- pretty printing

showDouble :: Double -> String
showDouble x = showEFloat (Just 4) x ""

showDoubleLong :: Double -> String
showDoubleLong x = showEFloat (Just 12) x ""

red :: String -> String
red str = "\x1b[31m"++str++"\x1b[39;49m"

