module HLearn.History
    (
    -- * The History Monad
    History
    , runHistory
    , evalHistory
    , traceHistory
    , traceAllHistory

    -- ** Reporting tools
    , Report (..)
    , beginFunction
    , report
    , iterate

    -- *** stop conditions
    , StopCondition
    , StopCondition_

    , maxIterations
    , stopBelow
    , fx1grows
    , mulTolerance

    -- * Display Functions
    , Optimizable
    , DisplayFunction
    , dhist
    , dispIteration
    , displayFilter
    , maxReportLevel
    , counthist

    -- * data membership classes
    , Has_x1 (..)
    , Has_fx1 (..)

    )
    where

import qualified Prelude as P

import Control.Monad.Identity     hiding (Functor (..), Monad(..), join, forM_)
import Control.Monad.Reader       hiding (Functor (..), Monad(..), join, forM_)
import Control.Monad.State.Strict hiding (Functor (..), Monad(..), join, forM_)
import Control.Monad.Trans        hiding (Functor (..), Monad(..))
import Numeric
import System.CPUTime
import System.IO
import System.IO.Unsafe

import SubHask
import SubHask.Compatibility.Containers

import Debug.Trace

-------------------------------------------------------------------------------

type Optimizable a = (Typeable a, Show a)

data DisplayFunction s = DisplayFunction
    { startDisplayFunction :: IO ()
    , stepDisplayFunction  :: forall a. Optimizable a => Report -> s -> a -> (s, IO ())
    , stopDisplayFunction  :: s -> IO ()
    }

instance Semigroup s => Semigroup (DisplayFunction s) where
    df1+df2 = DisplayFunction
        (startDisplayFunction df1+startDisplayFunction df2)
        (stepDisplayFunction df1 +stepDisplayFunction df2 )
        (stopDisplayFunction df1 +stopDisplayFunction df2 )

instance Monoid s => Monoid (DisplayFunction s) where
    zero = DisplayFunction zero zero zero

type DisplayFilter = forall a. Optimizable a => Report -> a -> Bool
type DisplayInfo = forall a. Optimizable a => Report -> a -> String

counthist :: DisplayFunction (Map' TypeRep Int)
counthist = DisplayFunction zero step stop
    where
        -- type signature needed for -XImpredicativeTypes
        step :: forall a. Optimizable a => Report -> Map' TypeRep Int -> a -> (Map' TypeRep Int, IO ())
        step _ s _ = (insertAt t x s, return ())
            where
                x = case lookup t s of
                    Nothing -> 1
                    Just x  -> x+1

                t = typeRep (Proxy::Proxy a)

        stop m = forM_ (toIxList m) $ \(t,i) -> do
            putStrLn $ "  " ++ show t ++ ": " ++ show i

displayFilter :: Monoid s => DisplayFilter -> DisplayFunction s -> DisplayFunction s
displayFilter f df = df
    { stepDisplayFunction = \r s a -> if f r a
        then stepDisplayFunction df r s a
        else (zero, return ())
    }

maxReportLevel :: Int -> DisplayFilter
maxReportLevel n r _ = reportLevel r <= n

dispIteration :: forall s. Monoid s => DisplayInfo -> DisplayFunction s
dispIteration f = DisplayFunction zero g zero
    where
        -- type signature needed for -XImpredicativeTypes
        g :: forall a. Optimizable a => Report -> s -> a -> (s, IO () )
        g r s a = (zero, putStrLn $ (concat $ P.replicate (reportLevel r) " - ") ++ f r a)

dhist :: Monoid s => DisplayFunction s
dhist = dispIteration (infoItr + infoType + infoDiffTime)

infoDiffTime :: DisplayInfo
infoDiffTime r _ = "; " ++ showEFloat (Just $ len-4-4) (fromIntegral (cpuTimeDiff r) * 1e-12 :: Double) "" ++ " sec"
    where
        len=16

infoString :: String -> DisplayInfo
infoString = const . const

infoType :: DisplayInfo
infoType _ a = "; "++if typeRep [a] == typeRep [""]
    then P.init $ P.tail $ show a
    else P.head $ P.words $ show $ typeRep [a]

infoItr :: DisplayInfo
infoItr r _ = "; "++show (numReports r)

type instance Logic (IO a) = Logic a

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

type History a = forall s. History_ s a

newtype History_ s a = History
    ( ReaderT
        ( DisplayFunction s )
        ( StateT
            (s,[Report])
--             ( StateT
--                 s
            IO
--             )
        )
        a
    )

{-# INLINABLE evalHistory #-}
evalHistory :: History_ () a -> a
evalHistory = unsafePerformIO . runHistory zero

{-# INLINABLE traceHistory #-}
traceHistory :: History_ () a -> a
traceHistory = unsafePerformIO . runHistory (displayFilter (maxReportLevel 2) dhist)

{-# INLINABLE traceAllHistory #-}
traceAllHistory :: History_ () a -> a
traceAllHistory = unsafePerformIO . runHistory dhist

{-# INLINABLE runHistory #-}
runHistory :: forall s a. Monoid s => DisplayFunction s -> History_ s a -> IO a
runHistory df (History hist) = {-# SCC runHistory #-} do
    time <- getCPUTime
    let startReport = Report
            { cpuTimeStart  = time
            , cpuTimeDiff   = 0
            , numReports    = 0
            , reportLevel   = 0
            }

    startDisplayFunction df

    -- the nasty type signature below is needed for -XImpredicativeTypes
    (a, (s,_)) <- runStateT
        ( (runReaderT :: forall m. ReaderT (DisplayFunction s) m a -> DisplayFunction s -> m a )
            hist
            df
        )
        (zero, [startReport])

    stopDisplayFunction df s

    return a

{-# INLINABLE beginFunction #-}
beginFunction :: String -> History_ s a -> History_ s a
beginFunction b ha = collectReports $ do
    report b
    collectReports ha


-- maxIterations :: Int {- ^ max number of iterations -} -> StopCondition
-- maxIterations i _ _ = History $ do
--     (_ , x:_) <- get
--     return $ numReports x >= i

{-# INLINABLE report #-}
report :: forall s a. Optimizable a => a -> History_ s a
report a = {-# SCC report #-} History $ do
    time <- liftIO getCPUTime

    (s0, prevReport:xs) <- get
    let newReport = Report
            { cpuTimeStart  = time
            , cpuTimeDiff   = time - cpuTimeStart prevReport
            , numReports    = numReports prevReport+1
            , reportLevel   = reportLevel prevReport
            }

    -- get our DisplayFunction and call it
    -- the cumbersome type signature is required for -XImpredicativeTypes
    (f::DisplayFunction s) <- (ask :: ReaderT (DisplayFunction s) (StateT (s, [Report]) IO) (DisplayFunction s))
    let (s1,io) = stepDisplayFunction f newReport s0 a

    put $ (s1, newReport:xs)
    liftIO io
    return a


{-# INLINABLE collectReports #-}
collectReports :: History_ s a -> History_ s a
collectReports (History hist) = {-# SCC collectReports #-} History $ do
    mkLevel
    a <- hist
    rmLevel
    return a
    where
        mkLevel = do
            (s, prevReport:xs) <- get
            time <- liftIO getCPUTime
            let newReport = Report
                    { cpuTimeStart  = time
                    , cpuTimeDiff   = 0
                    , numReports    = -1
                    , reportLevel   = reportLevel prevReport+1
                    }
            put $ (s, newReport:prevReport:xs)

        rmLevel = do
            (s, newReport:xs) <- get
            put (s,xs)

---------------------------------------
-- monad hierarchy

instance Functor Hask (History_ s) where
    fmap f (History s) = History (fmap f s)

instance Then (History_ s) where
    (>>) = haskThen

instance Monad Hask (History_ s) where
    return_ a = History $ return_ a
    join (History s) = History $ join (fmap (\(History s)->s) s)

---------------------------------------
-- algebra hierarchy

type instance Scalar (History_ s a) = Scalar a

instance Semigroup a => Semigroup (History_ s a) where
    ha1 + ha2 = do
        a1 <- ha1
        a2 <- ha2
        return $ a1+a2

instance Cancellative a => Cancellative (History_ s a) where
    ha1 - ha2 = do
        a1 <- ha1
        a2 <- ha2
        return $ a1-a2

instance Monoid a => Monoid (History_ s a) where
    zero = return zero

instance Group a => Group (History_ s a) where
    negate ha = do
        a <- ha
        return $ negate a

---------------------------------------
-- comparison hierarchy

type instance Logic (History_ s a) = History_ s (Logic a)

instance Eq_ a => Eq_ (History_ s a) where
    a==b = do
        a' <- a
        b' <- b
        return $ a'==b'

instance POrd_ a => POrd_ (History_ s a) where
    {-# INLINABLE inf #-}
    inf a b = do
        a' <- a
        b' <- b
        return $ inf a' b'

instance Lattice_ a => Lattice_ (History_ s a) where
    {-# INLINABLE sup #-}
    sup a b = do
        a' <- a
        b' <- b
        return $ sup a' b'

instance MinBound_ a => MinBound_ (History_ s a) where
    minBound = return $ minBound

instance Bounded a => Bounded (History_ s a) where
    maxBound = return $ maxBound

instance Heyting a => Heyting (History_ s a) where
    (==>) a b = do
        a' <- a
        b' <- b
        return $ a' ==> b'

instance Complemented a => Complemented (History_ s a) where
    not a = do
        a' <- a
        return $ not a'

instance Boolean a => Boolean (History_ s a)

-------------------------------------------------------------------------------

class Has_x1  opt v where x1  :: opt v -> v
class Has_fx1 opt v where fx1 :: opt v -> Scalar v

---------------------------------------

-- | A (sometimes) more convenient version of "StopCondition_".
type StopCondition = forall a. StopCondition_ a

-- | Functions of this type determine whether the "iterate" function should keep looping or stop.
type StopCondition_ a = a -> a -> History Bool

-- | This function is similar in spirit to the @while@ loop of imperative languages like @C@.
-- The advantage is the "DisplayFunction"s from the "History" monad get automatically (and efficiently!) threaded throughout our computation.
-- "iterate" is particularly useful for implementing iterative optimization algorithms.
{-# INLINABLE iterate #-}
iterate :: forall a. Optimizable a
    => (a -> History a)         -- ^ step function
    -> a                        -- ^ start parameters
    -> StopCondition_ a         -- ^ stop conditions
    -> History a
iterate step opt0 stop = {-# SCC iterate #-} do
    report opt0
    opt1 <- step opt0
    go opt0 opt1
    where
        go prevopt curopt = {-# SCC iterate_go #-} do
            report curopt
            done <- stop prevopt curopt
            if done
                then return curopt
                else do
                    opt' <- step curopt
                    go curopt opt'

-- | Stop iterating after the specified number of iterations.
-- This number is typically set fairly high (at least one hundred, possibly in the millions).
-- It should probably be used on all optimizations to prevent poorly converging optimizations taking forever.
maxIterations :: Int {- ^ max number of iterations -} -> StopCondition
maxIterations i _ _ = History $ do
    (_ , x:_) <- get
    return $ numReports x >= i

-- | Stop the optimization as soon as our function is below the given threshold.
stopBelow ::
    ( Has_fx1 opt v
    , Ord (Scalar v)
    ) => Scalar v
      -> StopCondition_ (opt v)
stopBelow threshold _ opt = return $ (fx1 opt) < threshold

-- | Stop the iteration when successive function evaluations are within a given distance of each other.
-- On "well behaved" problems, this means our optimization has converged.
mulTolerance ::
    ( BoundedField (Scalar v)
    , Has_fx1 opt v
    ) => Scalar v -- ^ tolerance
      -> StopCondition_ (opt v)
mulTolerance tol prevopt curopt = {-# SCC multiplicativeTollerance #-} do
    return $ (fx1 prevopt) /= infinity && left < right
        where
            left = 2*abs (fx1 curopt - fx1 prevopt)
            right = tol*(abs (fx1 curopt) + abs (fx1 prevopt) + 1e-18)

-- | Stop the optimization if our function value has grown between iterations
fx1grows :: ( Has_fx1 opt v , Ord (Scalar v) ) => StopCondition_ (opt v)
fx1grows opt0 opt1 = {-# SCC fx1grows #-} return $ fx1 opt0 < fx1 opt1
