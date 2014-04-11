{-# LANGUAGE RankNTypes #-}
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
import HLearn.Optimization.Common
import HLearn.Optimization.LineMinimization
import HLearn.Optimization.QuasiNewton
import HLearn.Optimization.NewtonRaphson

-------------------------------------------------------------------------------
-- main functions

traceOptimization m = deepseq (map (flip trace ()) $ traceDynamic 0 log) a
    where
        (a,log) = unsafeRunOptimization m

        traceDynamic :: Int -> [Event] -> [String]
        traceDynamic i xs = concatMap (traceEvent [traceBFGS,traceNewtonRaphson,traceBracket,traceBrent] i) xs

traceHistory fs m = deepseq (map (flip trace ()) $ traceDynamic 0 log) a
    where
        (a,log) = unsafeRunHistory m

        traceDynamic :: Int -> [Event] -> [String]
        traceDynamic i xs = concatMap (traceEvent fs i) xs

traceEvent :: [Event -> [String]] -> Int -> Event -> [String]
traceEvent fs i x = if i>=maxdepth+1
    then []
    else case fromDynamic (dyn x) :: Maybe (DList.DList Event) of
        Nothing -> map (traceSpacer i++) $ concatMap ($x) fs
        Just xs -> if i>=maxdepth
            then []
            else [traceSpacer (i+1) ++ traceDList x]++concatMap (traceEvent fs (i+2)) (DList.toList xs)
    where
        maxdepth=6

traceSpacer :: Int -> String
traceSpacer 0 = ""
traceSpacer 1 = " . "
traceSpacer i = traceSpacer (i-1)++" . "

---------------------------------------

printHistory :: [Event->[String]] -> [Event] -> IO ()
printHistory fs es = mapM_ putStrLn $ history2strings fs es

history2strings :: [Event->[String]] -> [Event] -> [String]
history2strings = go 0
    where

        go i fs [] = []
        go i fs (e:es) = str++go i fs es
            where
                str = case fromDynamic (dyn e) :: Maybe (DList.DList Event) of
                    Nothing -> map (traceSpacer i++) $ concatMap ($e) fs
                    Just x -> {-traceDList e:-}go (i+1) fs (DList.toList x)

-------------------------------------------------------------------------------
-- printing specific types

traceDList :: Event -> String
traceDList opt = case fromDynamic (dyn opt) :: Maybe (DList.DList Event) of
    Nothing -> ""
    Just x -> show (dyn opt) -- ++" -- "++show (length $ DList.toList x)++" -- "++show (dyn $ DList.head x)

---------------------------------------

traceBracket :: Event -> [String]
traceBracket opt = case fromDynamic (dyn opt) :: Maybe (LineBracket Double) of
    Nothing -> []
    Just x -> [show (dyn opt)++"; fa="++showDouble (_fa x)++"; fb="++showDouble (_fb x)++"; fc="++showDouble (_fc x)]

traceBrent :: Event -> [String]
traceBrent opt = case fromDynamic (dyn opt) :: Maybe (Brent Double) of
    Nothing -> []
    Just x -> [trace_itr undefined opt++"; "++show (dyn opt)++"; fv="++showDoubleLong (_fv x)++"; fx="++showDoubleLong (_fx x)++"; fw="++showDoubleLong (_fw x)]

-- traceGSS :: Event -> [String]
-- traceGSS opt = case fromDynamic (dyn opt) :: Maybe (GoldenSectionSearch Double) of
--     Nothing -> []
--     Just x -> 
--         [ trace_itr undefined opt++"; "++show (dyn opt)++"; fx1="++showDoubleLong (x^.fx1)++"; x1="++showDoubleLong (x^.x1)]

---------------------------------------

traceBFGS :: Event -> [String]
traceBFGS = traceFunk (undefined :: BFGS (Vector Double))

traceNewtonRaphson :: Event -> [String]
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
    ) => v a -> Event -> [String]
traceFunk _ opt = case fromDynamic (dyn opt) :: Maybe (v a) of
    Nothing -> []
    Just x -> [concat $ intersperse "; " $ map (\f -> f x opt) fs]
    where
        fs = [trace_itr,trace_eventType,trace_fx1,trace_f'x1,trace_stepSize,trace_sec]


-------------------------------------------------------------------------------

trace_itr :: a -> Event -> String
trace_itr _ e = "itr="++show (count e)

trace_sec :: a -> Event -> String
trace_sec _ e = "sec="++showEFloat (Just 4) ((fromIntegral $ runtime e)*1e-12) ""

trace_eventType :: a -> Event -> String
trace_eventType _ e = head $ words $ drop 2 $ show $ dyn e

trace_fx1 :: (RealFloat (Scalar a), Has_fx1 opt a) => opt a -> Event -> String
trace_fx1 a _ = "fx1="++showEFloat (Just 12) (a^.fx1) ""

trace_f'x1 :: (RealFloat (Scalar a), ValidTensor a, Has_f'x1 opt a) => opt a -> Event -> String
-- trace_f'x1 :: (RealFloat (Scalar a), InnerProduct (Tensor 1 a), Has_f'x1 opt a) => opt a -> Event -> String
trace_f'x1 a _ = "|f'x1|="++showEFloat (Just 4) (innerProductNorm $ a^.f'x1) ""

trace_stepSize :: (RealFloat (Scalar a), Has_stepSize opt a) => opt a -> Event -> String
trace_stepSize a _ = "step="++showEFloat (Just 4) (a^.stepSize) ""

-------------------------------------------------------------------------------
-- pretty printing

showDouble :: Double -> String
showDouble x = showEFloat (Just 4) x ""

showDoubleLong :: Double -> String
showDoubleLong x = showEFloat (Just 12) x ""

red :: String -> String
red str = "\x1b[31m"++str++"\x1b[39;49m"

