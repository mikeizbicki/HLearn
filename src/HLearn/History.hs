module HLearn.History
    where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Monad.Trans
import Data.Dynamic
import Data.Monoid
import Data.Typeable
import Numeric
import System.Console.ANSI
import System.CPUTime
import System.IO

import Pipes

-------------------------------------------------------------------------------
-- data types

type CPUTime = Integer

data Report = Report 
    { cpuTime       :: {-#NOUNPACK#-}!CPUTime 
    , cpuTimeDiff   :: {-#NOUNPACK#-}!CPUTime 
    , dyn           :: {-#UNPACK#-}!Dynamic
    , numReports    :: {-#UNPACK#-}!Int
    , reportLevel   :: {-#UNPACK#-}!Int
    }
    deriving (Show,Typeable)

data StartCollection = StartCollection
    deriving (Show,Typeable)

data EndHistory = EndHistory
    deriving (Show,Typeable)

type BaseMonad = StateT [Report] IO

type DisplayFilter = DisplayFilter' ()
type DisplayMethod = DisplayMethod' ()

type DisplayFilter' s = Report -> StateT s IO Bool
type DisplayMethod' s = Report -> StateT s IO ()

type History = Producer Report BaseMonad

-------------------------------------------------------------------------------

type DisplayFunction = Report -> String

mkDisplayFunction :: forall a. Typeable a => (a -> String) -> DisplayFunction
mkDisplayFunction f x = case fromDynamic $ dyn x :: Maybe a of
    Nothing -> ""
    Just y -> f y

-------------------------------------------------------------------------------
-- functions

runHistory :: Monoid s => DisplayMethod' s -> History a -> IO a
runHistory = runHistoryWithState mempty

runHistoryWithState :: forall s a. s -> DisplayMethod' s -> History a -> IO a
runHistoryWithState s f hist = do
    time <- getCPUTime
    let startReport = 
            Report
                { cpuTime       = time
                , cpuTimeDiff   = 0
                , dyn           = toDyn StartCollection
                , numReports    = 0
                , reportLevel   = 0
                }
    let hist' = do
            a <- hist
            report EndHistory
            return a
    evalStateT (runEffect $ hist' >-> go s) [startReport]

    where
        go s = do
            next <- await
            ((),s') <- liftIO $ runStateT (f next) s
            go s'

idDisplayFilter :: DisplayFilter
idDisplayFilter _ = return True

idDisplayMethod :: DisplayMethod
idDisplayMethod _ = return ()

---------------------------------------

report :: Typeable a => a -> History a
report a = do
    time <- liftIO getCPUTime
    prevReport:xs <- get
    let newReport = Report 
            { cpuTime       = time
            , cpuTimeDiff   = time - cpuTime prevReport
            , dyn           = toDyn a
            , numReports    = numReports prevReport+1
            , reportLevel   = reportLevel $ prevReport
            }
    yield newReport
    put $ newReport:xs
    return a

collectReports :: History a -> History a
collectReports hist = do
    mkLevel
    a <- hist
    rmLevel
    return a
    where
        mkLevel = do
            prevReport:xs <- get
            time <- liftIO getCPUTime
            let newReport = Report
                    { cpuTime       = time
                    , cpuTimeDiff   = 0
                    , dyn           = toDyn StartCollection
                    , numReports    = -1
                    , reportLevel   = reportLevel prevReport+1
                    }
            put $ newReport:prevReport:xs
                
        rmLevel = do
            newReport:xs <- get
            put xs

prevReport :: History Report
prevReport = do
    x:_ <- get
    return x
