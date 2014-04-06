module HLearn.Optimization.Trace
    where

import Control.DeepSeq
import qualified Data.DList as DList
import Data.Dynamic
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
        traceDynamic i xs = concatMap (traceEvent i) xs


traceEvent :: Int -> Event -> [String]
traceEvent i x = if i>=maxdepth+1
    then []
    else case fromDynamic (dyn x) :: Maybe (DList.DList Event) of
        Nothing -> map (traceSpacer i++) 
            $ traceBFGS x
            ++traceNewtonRaphson x
            ++traceBracket x
            ++traceBrent x
        Just xs -> if i>=maxdepth
            then []
            else [traceSpacer (i+1) ++ traceDList x]++concatMap (traceEvent (i+2)) (DList.toList xs)
    where
        maxdepth=2

traceSpacer :: Int -> String
traceSpacer 0 = ""
traceSpacer 1 = " . "
traceSpacer i = traceSpacer (i-1)++" . "

-------------------------------------------------------------------------------
-- printing specific types

traceDList :: Event -> String
traceDList opt = case fromDynamic (dyn opt) :: Maybe (DList.DList Event) of
    Nothing -> ""
    Just x -> show (dyn opt) -- ++" -- "++show (length $ DList.toList x)++" -- "++show (dyn $ DList.head x)
--     Just x -> show (dyn opt)++" -- "++show (length $ DList.toList x)++" -- "++show (dyn $ DList.head x)

---------------------------------------

traceBracket :: Event -> [String]
traceBracket opt = case fromDynamic (dyn opt) :: Maybe (LineBracket Double) of
    Nothing -> []
    Just x -> [show (dyn opt)++"; fa="++showDouble (_fa x)++"; fb="++showDouble (_fb x)++"; fc="++showDouble (_fc x)]

traceBrent :: Event -> [String]
traceBrent opt = case fromDynamic (dyn opt) :: Maybe (Brent Double) of
    Nothing -> []
    Just x -> [show (dyn opt)++"; fv="++showDoubleLong (_fv x)++"; fx="++showDoubleLong (_fx x)++"; fw="++showDoubleLong (_fw x)]

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
    Just x -> 
        [ "itr="++show (count opt)
        ++"; "++show (dyn opt)
        ++"; fx1="++showDoubleLong (fx1 x)
        ++"; |f'x1|="++showDouble (innerProductNorm $ f'x1 x)
        ++"; step="++showDouble (stepSize x)
--         ++"; step="++showDouble (stepSize x)
--         ++"; sec="++showDouble ((fromIntegral $ stoptime opt)*1e-12)
        ++"; sec="++showDouble ((fromIntegral $ runtime opt)*1e-12)
        ]

-------------------------------------------------------------------------------
-- pretty printing

showDouble :: Double -> String
showDouble x = showEFloat (Just 4) x ""

showDoubleLong :: Double -> String
showDoubleLong x = showEFloat (Just 12) x ""

red :: String -> String
red str = "\x1b[31m"++str++"\x1b[39;49m"

