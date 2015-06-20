-- | All optimization algorithms get run within the "History" monad provided in this module.
-- This monad lets us thread user-defined debugging code throughout our optimization procedures.
-- Most optimization libraries don't include significant debugging features because of the runtime overhead.
-- That's not a problem for us, however.
-- When you run a "History" monad with no debugging information (e.g. by using "evalHistory"), then no runtime penalty is incurred.
-- GHC/LLVM is able to optimize everything into tight, efficient loops.
-- You only pay for the overhead that you actually use.
module HLearn.History
    (
    -- * The History Monad
    History
    , History_
--     , History__
    , runHistory
    , evalHistory
    , traceHistory
    , traceAllHistory

    , NoCxt
    , ValidCxt

    -- ** Reporting tools
    , Report (..)
    , beginFunction
    , report
    , withMsg
    , withMsgIO
    , iterate
    , currentItr

    -- *** stop conditions
    , StopCondition

    , maxIterations
    , stopBelow
    , mulTolerance
    , fx1grows
    , noProgress

    -- * Display Functions
    , DisplayFunction

    -- ** Display each iteration
    , dispIteration
    , dispIteration_
    , infoString
    , infoDiffTime
    , infoType
    , infoItr

    -- ** Display at the end
    , summaryTable

    -- ** Consider only some iterations
    , DisplayFilter
    , displayFilter
    , maxReportLevel

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
import Unsafe.Coerce

import SubHask
import SubHask.Algebra.Container
import SubHask.Compatibility.Containers

-------------------------------------------------------------------------------

-- FIXME:
-- This class is obsolete and should be deleted.
-- It should be hard to do, I'm just tired right now and don't want to do the refactoring.
class (Typeable a, Show a) => Optimizable a
instance (Typeable a, Show a) => Optimizable a

-------------------------------------------------------------------------------

-- |
--
-- FIXME: Is there a way to store times in "Int"s rather than "Integer"s for more efficiency?
type CPUTime = Integer

-- | This data type stores information about each step in our optimization routines.
--
-- FIXME:
-- Is there a better name for this?
data Report = Report
    { cpuTimeStart  :: !CPUTime
    , cpuTimeDiff   :: !CPUTime
    , numReports    :: {-#UNPACK#-}!Int
    , reportLevel   :: {-#UNPACK#-}!Int
    }
    deriving Show

mkMutable [t| Report |]

-------------------------------------------------------------------------------
-- display functions

-- | When running a "History" monad, there are three times we might need to perform IO actions: the beginning, middle, and end.
-- This type just wraps all three of those functions into a single type.
data DisplayFunction_ cxt s = DisplayFunction
    { startDisplayFunction :: IO ()
    , stepDisplayFunction  :: forall a. cxt a => Report -> s -> a -> (s, IO ())
    , stopDisplayFunction  :: s -> IO ()
    }

type DisplayFunction = DisplayFunction_ Optimizable

mkMutable [t| forall cxt s. DisplayFunction_ cxt s |]

instance Semigroup s => Semigroup (DisplayFunction_ cxt s) where
    df1+df2 = DisplayFunction
        (startDisplayFunction df1+startDisplayFunction df2)
        (stepDisplayFunction df1 +stepDisplayFunction df2 )
        (stopDisplayFunction df1 +stopDisplayFunction df2 )

instance Monoid s => Monoid (DisplayFunction_ cxt s) where
    zero = DisplayFunction zero zero zero

----------------------------------------
-- filtering

-- | Functions of this type are used to prevent the "stepDisplayFunction" from being called in certain situaations.
type DisplayFilter = forall a. Optimizable a => Report -> a -> Bool

displayFilter :: Monoid s => DisplayFilter -> DisplayFunction s -> DisplayFunction s
displayFilter f df = df
    { stepDisplayFunction = \r s a -> if f r a
        then stepDisplayFunction df r s a
        else (zero, return ())
    }

maxReportLevel :: Int -> DisplayFilter
maxReportLevel n r _ = reportLevel r <= n

----------------------------------------
-- summary table

-- | Functions of this type are used as parameters to the "dispIteration_" function.
type DisplayInfo = forall a. Optimizable a => Report -> a -> String

-- | After each step in the optimization completes, print a single line describing what happened.
dispIteration :: Monoid s => DisplayFunction s
dispIteration = dispIteration_ (infoItr + infoType + infoDiffTime)

-- | A more general version of "dispIteration" that let's you specify what information to display.
dispIteration_ :: forall s. Monoid s => DisplayInfo -> DisplayFunction s
dispIteration_ f = DisplayFunction zero g zero
    where
        -- type signature needed for -XImpredicativeTypes
        g :: forall a. Optimizable a => Report -> s -> a -> (s, IO () )
        g r s a = (zero, putStrLn $ (concat $ P.replicate (reportLevel r) " - ") ++ f r a)

-- | Pretty-print a "CPUTime".
showTime :: CPUTime -> String
showTime t = showEFloat (Just $ len-4-4) (fromIntegral t * 1e-12 :: Double) "" ++ " sec"
    where
        len=12

-- | Print a raw string.
infoString :: String -> DisplayInfo
infoString = const . const

-- | Print the time used to complete the step.
infoDiffTime :: DisplayInfo
infoDiffTime r _ = "; " ++ showTime (cpuTimeDiff r)

-- | Print the name of the optimization step.
infoType :: DisplayInfo
infoType _ a = "; "++if typeRep [a] == typeRep [""]
    then P.init $ P.tail $ show a
    else show a
--     else P.head $ P.words $ show $ typeRep [a]

-- | Print the current iteration of the optimization.
infoItr :: DisplayInfo
infoItr r _ = "; "++show (numReports r)

----------------------------------------
-- summary table

-- | Contains all the information that might get displayed by "summaryTable".
--
-- FIXME:
-- There's a lot more information that could be included.
-- We could make "summaryTable" take parameters describing which elements to actually calculate/display.
data CountInfo = CountInfo
    { numcalls :: Int
    , tottime  :: Integer
    }
    deriving Show

type instance Logic CountInfo = Bool

instance Eq_ CountInfo where
    ci1==ci2 = numcalls ci1 == numcalls ci2
            && tottime  ci1 == tottime  ci2

avetime :: CountInfo -> Integer
avetime = round (fromIntegral tottime / fromIntegral numcalls :: CountInfo -> Double)

-- | Call "runHistory" with this "DisplayFunction" to get a table summarizing the optimization.
-- This does not affect output during the optimization itself, only at the end.
summaryTable :: DisplayFunction (Map' (Lexical String) CountInfo)
summaryTable = DisplayFunction zero step stop
    where
        -- type signature needed for -XImpredicativeTypes
        step :: forall a. Optimizable a
            => Report
            -> Map' (Lexical String) CountInfo
            -> a
            -> ( Map' (Lexical String) CountInfo, IO () )
        step r s a = (insertAt k ci s, return ())
            where
                t = typeRep (Proxy::Proxy a)

                k = Lexical $ if t == typeRep (Proxy::Proxy String)
                    then unsafeCoerce a
                    else P.head $ P.words $ show t

                ci0 = case lookup k s of
                    Just x -> x
                    Nothing -> CountInfo
                        { numcalls = 0
                        , tottime  = 0
                        }

                ci = ci0
                    { numcalls = numcalls ci0+1
                    , tottime  = tottime  ci0+cpuTimeDiff r
                    }

        stop :: Map' (Lexical String) CountInfo -> IO ()
        stop m = do

            let hline = putStrLn $ " " ++ P.replicate (maxlen_name+maxlen_count+maxlen_time+10) '-'

            hline
            putStrLn $ " | " ++ padString title_name  maxlen_name
                    ++ " | " ++ padString title_count maxlen_count
                    ++ " | " ++ padString title_time  maxlen_time
                    ++ " | "
            hline
            forM_ (toIxList m) $ \(k,ci) -> do
                putStrLn $ " | " ++ padString (unLexical k           ) maxlen_name
                        ++ " | " ++ padString (show     $ numcalls ci) maxlen_count
                        ++ " | " ++ padString (showTime $ tottime  ci) maxlen_time
                        ++ " | "
            hline

            where
                title_name  = "report name"
                title_count = "number of calls"
                title_time  = "average time per call"
                maxlen_name  = maximum $ length title_name:(map (length .                       fst) $ toIxList m)
                maxlen_count = maximum $ length title_count:(map (length . show     . numcalls . snd) $ toIxList m)
                maxlen_time  = maximum $ length title_time: (map (length . showTime . tottime  . snd) $ toIxList m)

padString :: String -> Int -> String
padString a i = P.take i $ a ++ P.repeat ' '

-------------------------------------------------

-- | Every type is an instance of "NoCxt".
-- When running a "History" monad, we must always assign a value to the "cxt" variable.
-- Use "NoCxt" when you don't want to enforce any constraints.
class NoCxt a
instance NoCxt a

-- | Applies the cxt to construct the needed constraints.
type ValidCxt (cxt :: * -> Constraint) a =
    ( cxt String
    , cxt a
    , cxt (Scalar a)
    )

-- | A (sometimes) more convenient version of "History_"
type History cxt a = forall s. ValidCxt cxt a => History_ cxt s a

-- | This monad internally requires -XImpredicativeTypes to thread our "DisplayFunction" throughout the code.
newtype History_ cxt s a = History
    ( ReaderT
        ( DisplayFunction_ cxt s )
        ( StateT
            (s,[Report])
--             ( StateT
--                 s
            IO
--             )
        )
        a
    )

mkMutable [t| forall cxt s a. History_ cxt s a |]

-- | Run the "History" computation without any debugging information.
-- This is the most efficient way to run an optimization.
{-# INLINABLE evalHistory #-}
evalHistory :: History_ NoCxt () a -> a
evalHistory = unsafePerformIO . runHistory zero

-- | Run the "History" computation with a small amount of debugging information.
{-# INLINABLE traceHistory #-}
traceHistory :: Optimizable a => History_ Optimizable () a -> a
traceHistory = unsafePerformIO . runHistory (displayFilter (maxReportLevel 2) dispIteration)

-- | Run the "History" computation with a lot of debugging information.
{-# INLINABLE traceAllHistory #-}
traceAllHistory :: Optimizable a => History_ Optimizable () a -> a
traceAllHistory = unsafePerformIO . runHistory dispIteration

-- | Specify the amount of debugging information to run the "History" computation with.
{-# INLINABLE runHistory #-}
runHistory :: forall cxt s a. (cxt a, Monoid s) => DisplayFunction_ cxt s -> History_ cxt s a -> IO a
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
        ( (runReaderT :: forall m. ReaderT (DisplayFunction_ cxt s) m a -> DisplayFunction_ cxt s -> m a )
            hist
            df
        )
        (zero, [startReport])

    stopDisplayFunction df s

    return a

-- | You should call this function everytime your "History" computation enters a new phase.
-- For iterative algorithms, you should probably call this function once per loop.
--
-- This is a convenient wrapper around the "report" and "collectReports" functions.
{-# INLINABLE beginFunction #-}
beginFunction :: cxt String => String -> History_ cxt s a -> History_ cxt s a
beginFunction b ha = collectReports $ do
    report b
    collectReports ha

-- | Register the parameter of type @a@ as being important for debugging information.
-- This creates a new "Report" and automatically runs the appropriate "stepDisplayFunction".
{-# INLINABLE report #-}
report :: forall cxt s a. cxt a => a -> History_ cxt s a
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
    (f::DisplayFunction_ cxt s) <-
        (ask :: ReaderT (DisplayFunction_ cxt s) (StateT (s, [Report]) IO) (DisplayFunction_ cxt s))
    let (s1,io) = stepDisplayFunction f newReport s0 a

    put $ (s1, newReport:xs)
    liftIO io
    return a

-- | Group all of the "Reports" that happen in the given computation together.
-- You probably don't need to call this function directly, and instead should call "beginFunction".
{-# INLINABLE collectReports #-}
collectReports :: History_ cxt s a -> History_ cxt s a
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


{-# INLINABLE currentItr #-}
currentItr :: History cxt Int
currentItr = History $ do
    (_ , x:_) <- get
    return $ numReports x

---------------------------------------

withMsg :: (cxt String, NFData a) => String -> a -> History_ cxt s a
withMsg msg a = withMsgIO msg (return a)

withMsgIO :: (cxt String, NFData a) => String -> IO a -> History_ cxt s a
withMsgIO msg ioa = do
    a <- History $ liftIO ioa
    report $ deepseq a $ msg
    return a

-------------------------------------------------------------------------------
-- algebra

---------------------------------------
-- monad hierarchy

instance Functor Hask (History_ cxt s) where
    fmap f (History s) = History (fmap f s)

instance Then (History_ cxt s) where
    (>>) = haskThen

instance Monad Hask (History_ cxt s) where
    return_ a = History $ return_ a
    join (History s) = History $ join (fmap (\(History s)->s) s)

---------------------------------------
-- math hierarchy

type instance Scalar (History_ cxt s a) = Scalar a

instance Semigroup a => Semigroup (History_ cxt s a) where
    ha1 + ha2 = do
        a1 <- ha1
        a2 <- ha2
        return $ a1+a2

instance Cancellative a => Cancellative (History_ cxt s a) where
    ha1 - ha2 = do
        a1 <- ha1
        a2 <- ha2
        return $ a1-a2

instance Monoid a => Monoid (History_ cxt s a) where
    zero = return zero

instance Group a => Group (History_ cxt s a) where
    negate ha = do
        a <- ha
        return $ negate a

---------------------------------------
-- comparison hierarchy

type instance Logic (History_ cxt s a) = History_ cxt s (Logic a)

instance Eq_ a => Eq_ (History_ cxt s a) where
    a==b = do
        a' <- a
        b' <- b
        return $ a'==b'

instance POrd_ a => POrd_ (History_ cxt s a) where
    {-# INLINABLE inf #-}
    inf a b = do
        a' <- a
        b' <- b
        return $ inf a' b'

instance Lattice_ a => Lattice_ (History_ cxt s a) where
    {-# INLINABLE sup #-}
    sup a b = do
        a' <- a
        b' <- b
        return $ sup a' b'

instance MinBound_ a => MinBound_ (History_ cxt s a) where
    minBound = return $ minBound

instance Bounded a => Bounded (History_ cxt s a) where
    maxBound = return $ maxBound

instance Heyting a => Heyting (History_ cxt s a) where
    (==>) a b = do
        a' <- a
        b' <- b
        return $ a' ==> b'

instance Complemented a => Complemented (History_ cxt s a) where
    not a = do
        a' <- a
        return $ not a'

instance Boolean a => Boolean (History_ cxt s a)

-------------------------------------------------------------------------------
-- iteration

-- | This function is similar in spirit to the @while@ loop of imperative languages like @C@.
-- The advantage is the "DisplayFunction"s from the "History" monad get automatically (and efficiently!) threaded throughout our computation.
-- "iterate" is particularly useful for implementing iterative optimization algorithms.
{-# INLINABLE iterate #-}
iterate :: forall cxt a. cxt a
    => (a -> History cxt a)                 -- ^ step function
    -> StopCondition a                      -- ^ stop conditions
    -> a                                    -- ^ start parameters
    -> History cxt a                        -- ^ result
iterate step stop opt0 = {-# SCC iterate #-} do
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

---------------------------------------

class Has_x1        opt v where x1      :: opt v -> v
class Has_fx1       opt v where fx1     :: opt v -> Scalar v

---------------------------------------
-- stop conditions

-- | Functions of this type determine whether the "iterate" function should keep looping or stop.
type StopCondition a = forall cxt. cxt a => a -> a -> forall s. History_ cxt s Bool

-- | Stop iterating after the specified number of iterations.
-- This number is typically set fairly high (at least one hundred, possibly in the millions).
-- It should probably be used on all optimizations to prevent poorly converging optimizations taking forever.
maxIterations :: Int {- ^ max number of iterations -} -> StopCondition a
maxIterations i _ _ = History $ do
    (_ , x:_) <- get
    return $ numReports x >= i

-- | Stop the optimization as soon as our function is below the given threshold.
stopBelow ::
    ( Has_fx1 opt v
    , Ord (Scalar v)
    ) => Scalar v
      -> StopCondition (opt v)
stopBelow threshold _ opt = return $ (fx1 opt) < threshold

-- | Stop the iteration when successive function evaluations are within a given distance of each other.
-- On "well behaved" problems, this means our optimization has converged.
mulTolerance ::
    ( BoundedField (Scalar v)
    , Has_fx1 opt v
    ) => Scalar v -- ^ tolerance
      -> StopCondition (opt v)
mulTolerance tol prevopt curopt = {-# SCC multiplicativeTollerance #-} do
    return $ (fx1 prevopt) /= infinity && left < right
        where
            left = 2*abs (fx1 curopt - fx1 prevopt)
            right = tol*(abs (fx1 curopt) + abs (fx1 prevopt) + 1e-18)

-- | Stop the optimization if our function value has grown between iterations
fx1grows :: ( Has_fx1 opt v , Ord (Scalar v) ) => StopCondition (opt v)
fx1grows opt0 opt1 = return $ fx1 opt0 < fx1 opt1

-- | Stop the optimization if our function value has stopped decreasing
noProgress :: Eq (opt v) => StopCondition (opt v)
noProgress opt0 opt1 = return $ opt0 == opt1
