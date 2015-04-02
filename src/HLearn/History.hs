module HLearn.History
    (
    -- * The History Monad
    History
    , runHistory
    , evalHistory
    , traceHistory
    , beginFunction

    , report
    , getNumReports

    -- * Display Functions
    , Optimizable
    , DisplayFunction
    , nodisplay
    , dhist

    , module Control.Lens -- FIXME: remove this as a dependency
    )
    where

import qualified Prelude as P

import Control.Lens
import Control.Monad.Identity     hiding (Functor (..), Monad(..), join)
import Control.Monad.Reader       hiding (Functor (..), Monad(..), join)
import Control.Monad.State.Strict hiding (Functor (..), Monad(..), join)
import Control.Monad.Trans        hiding (Functor (..), Monad(..))
import Data.Dynamic
import Data.Typeable
import System.CPUTime
import System.IO
import System.IO.Unsafe

import SubHask
import SubHask.Monad

-------------------------------------------------------------------------------

type Optimizable a = (Typeable a, Show a)
type DisplayFunction = forall a. Optimizable a => Report -> a -> IO ()

nodisplay :: DisplayFunction
nodisplay _ _ = return ()

dhist :: DisplayFunction
dhist r a = do
    putStrLn $ (concat $ P.replicate (reportLevel r) " - ") ++ show a

instance Semigroup a => Semigroup (IO a) where
    ioa1 + ioa2 = do
        a1 <- ioa1
        a2 <- ioa2
        return $ a1+a2

-------------------------------------------------------------------------------

type CPUTime = Integer

data Report = Report
    { cpuTimeStart  :: !CPUTime
    , cpuTimeDiff   :: !CPUTime
    , numReports    :: {-#UNPACK#-}!Int
    , reportLevel   :: {-#UNPACK#-}!Int
    }
    deriving Show

-------------------------------------------------

newtype History a = History (ReaderT DisplayFunction (StateT [Report] IO) a)

{-# INLINABLE evalHistory #-}
evalHistory :: History a -> a
evalHistory = unsafePerformIO . runHistory nodisplay

{-# INLINABLE traceHistory #-}
traceHistory :: History a -> a
traceHistory = unsafePerformIO . runHistory dhist

{-# INLINABLE runHistory #-}
runHistory :: forall a. DisplayFunction -> History a -> IO a
runHistory f (History hist) = {-# SCC runHistory #-} do
    time <- getCPUTime
    let startReport = Report
            { cpuTimeStart  = time
            , cpuTimeDiff   = 0
            , numReports    = 0
            , reportLevel   = 0
            }

    -- the nasty type signature below is needed for -XImpredicativeTypes
    evalStateT
        ( (runReaderT :: forall m. ReaderT DisplayFunction m a -> DisplayFunction -> m a )
            hist
            f
        ) [startReport]

{-# INLINABLE beginFunction #-}
beginFunction :: String -> History a -> History a
beginFunction b ha = collectReports $ do
    report b
    collectReports ha


{-# INLINABLE report #-}
report :: forall disp a. Optimizable a => a -> History a
report a = {-# SCC report #-} History $ do
    time <- liftIO getCPUTime

    prevReport:xs <- get
    let newReport = Report
            { cpuTimeStart  = time
            , cpuTimeDiff   = time - cpuTimeStart prevReport
            , numReports    = numReports prevReport+1
            , reportLevel   = reportLevel prevReport
            }
    put $ newReport:xs

    -- get our DisplayFunction and call it
    -- the cumbersome type annotation is required for -XImpredicativeTypes
    (f::DisplayFunction) <- (ask :: ReaderT DisplayFunction (StateT [Report] IO) DisplayFunction)
    liftIO $ f newReport a

    return a


{-# INLINABLE collectReports #-}
collectReports :: History a -> History a
collectReports (History hist) = {-# SCC collectReports #-} History $ do
    mkLevel
    a <- hist
    rmLevel
    return a
    where
        mkLevel = do
            prevReport:xs <- get
            time <- liftIO getCPUTime
            let newReport = Report
                    { cpuTimeStart  = time
                    , cpuTimeDiff   = 0
                    , numReports    = -1
                    , reportLevel   = reportLevel prevReport+1
                    }
            put $ newReport:prevReport:xs

        rmLevel = do
            newReport:xs <- get
            put xs

{-# INLINABLE getNumReports #-}
getNumReports :: History Int
getNumReports = History $ do
    x:_ <- get
    return $ numReports x

---------------------------------------
-- monad hierarchy

instance Functor Hask History where
    fmap f (History s) = History (fmap f s)

instance Then History where
    (>>) = haskThen

instance Monad Hask History where
    return_ a = History $ return_ a
    join (History s) = History $ join (fmap (\(History s)->s) s)

---------------------------------------
-- algebra hierarchy

type instance Scalar (History a) = Scalar a

instance Semigroup a => Semigroup (History a) where
    ha1 + ha2 = do
        a1 <- ha1
        a2 <- ha2
        return $ a1+a2

instance Cancellative a => Cancellative (History a) where
    ha1 - ha2 = do
        a1 <- ha1
        a2 <- ha2
        return $ a1-a2

instance Monoid a => Monoid (History a) where
    zero = return zero

instance Group a => Group (History a) where
    negate ha = do
        a <- ha
        return $ negate a

---------------------------------------
-- comparison hierarchy

type instance Logic (History a) = History (Logic a)

instance Eq_ a => Eq_ (History a) where
    a==b = do
        a' <- a
        b' <- b
        return $ a'==b'

instance POrd_ a => POrd_ (History a) where
    {-# INLINABLE inf #-}
    inf a b = do
        a' <- a
        b' <- b
        return $ inf a' b'

instance Lattice_ a => Lattice_ (History a) where
    {-# INLINABLE sup #-}
    sup a b = do
        a' <- a
        b' <- b
        return $ sup a' b'

instance MinBound_ a => MinBound_ (History a) where
    minBound = return $ minBound

instance Bounded a => Bounded (History a) where
    maxBound = return $ maxBound

instance Heyting a => Heyting (History a) where
    (==>) a b = do
        a' <- a
        b' <- b
        return $ a' ==> b'

instance Complemented a => Complemented (History a) where
    not a = do
        a' <- a
        return $ not a'

instance Boolean a => Boolean (History a)

