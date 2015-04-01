{-# LANGUAGE OverlappingInstances #-}
module HLearn.History
    (
    History
--     , History_DW
--     , History'
    , History_
    , runHistory
    , beginFunction

    , DisplayWrapper(..)

    , report
    , collectReports
--     , prevReport
    , currentItr

    , module Control.Lens
    , module SubHask.Monad
    )
    where

import qualified Prelude as P

import Control.Lens
import Control.Monad.Identity     hiding (Functor (..), Monad(..), join)
import Control.Monad.State.Strict hiding (Functor (..), Monad(..), join)
import Control.Monad.Trans        hiding (Functor (..), Monad(..))
import Data.Dynamic
import Data.Typeable
import System.CPUTime
import System.IO

import Pipes

import SubHask
import SubHask.Monad
import SubHask.Category.Trans.Constrained

-------------------------------------------------------------------------------

{-
class (P.Monad m, P.Functor m, Functor Hask m, Monad Hask m, Boolean (m Bool)) => HistoryMonad m where
    type Reportable m a :: Constraint
    type Reportable m a = ()

    data Report m

    report :: Reportable m a => a -> m a
    collectReports :: m a -> m a
    prevReport :: m (Report m)
    currentItr :: Ring r => m r

data StartCollection = StartCollection
    deriving (Show,Typeable)

data StartHistory = StartHistory
    deriving (Show,Typeable)

data EndHistory = EndHistory
    deriving (Show,Typeable)
-}

-------------------------------------------------------------------------------

class DisplayWrapper disp a where
    display :: Report -> proxy disp -> a -> IO ()

-- type History a = forall (disp :: *). DisplayWrapper disp a => History_ disp a
-- type History a = Show a => forall (disp :: *). History_ disp a
type History a = Show a => History' a
type History' a = forall disp. History_ disp a

-- type History' disp a =
--     ( DisplayWrapper (disp :: *) a
--     , DisplayWrapper disp [Char]
--     , DisplayWrapper disp Bool
--     ) => History_ disp a
-- type History_DW a = Show a => forall (disp :: *).
--     ( DisplayWrapper disp a
--     , DisplayWrapper disp [Char]
--     , DisplayWrapper disp Bool
-- --     , DisplayWrapper disp b
--     )=> History_ disp a

newtype History_ (disp :: *) a = History_ (StateT [Report] IO a)

-- runHistory :: forall a disp. DisplayWrapper disp a => History_ (disp :: *) a -> IO a
runHistory :: forall a disp. History_ disp a -> IO a
runHistory (History_ hist) = do
    time <- getCPUTime
    let startReport = Report
            { cpuTime       = time
            , cpuTimeDiff   = 0
            , numReports    = 0
            , reportLevel   = 0
            }

    evalStateT hist [startReport]

beginFunction :: (Show a, Show b) => b -> History_ disp a -> History_ disp a
-- beginFunction :: (DisplayWrapper disp a, DisplayWrapper disp b) => b -> History_ disp a -> History_ disp a
beginFunction b ha = collectReports $ do
    report b
    collectReports ha

type CPUTime = Integer

data Report = Report
    { cpuTime       :: {-#UNPACK#-}!CPUTime
    , cpuTimeDiff   :: {-#UNPACK#-}!CPUTime
    , numReports    :: {-#UNPACK#-}!Int
    , reportLevel   :: {-#UNPACK#-}!Int
    }
    deriving Show


{-# INLINABLE report #-}
-- report :: DisplayWrapper disp a => a -> History_ disp a
report :: a -> History_ disp a
report a = {-# SCC report #-} History_ $ do
    time <- liftIO getCPUTime
    prevReport:xs <- get
    let newReport = Report
            { cpuTime       = time
            , cpuTimeDiff   = time - cpuTime prevReport
            , numReports    = numReports prevReport+1
            , reportLevel   = reportLevel $ prevReport
            }
    put $ newReport:xs
--     liftIO $ do
--         putStrLn $ (concat $ P.replicate (reportLevel newReport) " - ") ++ show a
    return a

{-# INLINABLE collectReports #-}
collectReports :: History_ disp a -> History_ disp a
collectReports (History_ hist) = {-# SCC collectReports #-} History_ $ do
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
                    , numReports    = -1
                    , reportLevel   = reportLevel prevReport+1
                    }
            put $ newReport:prevReport:xs

        rmLevel = do
            newReport:xs <- get
            put xs

-- {-# INLINABLE prevReport #-}
-- prevReport = {-# SCC prevReport #-} History_ $ do
--     x:_ <- get
--     return x
--
{-# INLINABLE currentItr #-}
currentItr :: History_ disp Int
currentItr = {-# SCC currentItr #-} History_ $ do
    x:_ <- get
    return {-. fromInteger . fromIntegral -}$ numReports x

---------------------------------------
-- monad hierarchy

instance Functor Hask (History_ disp) where
    fmap f (History_ s) = History_ (fmap f s)

instance Then (History_ disp) where
    (>>) = haskThen

instance Monad Hask (History_ disp) where
    return_ a = History_ $ return_ a
    join (History_ s) = History_ $ join (fmap (\(History_ s)->s) s)

---------------------------------------
-- comparison hierarchy

type instance Logic (History_ disp a) = History_ disp (Logic a)

instance Eq_ a => Eq_ (History_ disp a) where
    a==b = do
        a' <- a
        b' <- b
        return $ a'==b'

instance POrd_ a => POrd_ (History_ disp a) where
    {-# INLINABLE inf #-}
    inf a b = do
        a' <- a
        b' <- b
        return $ inf a' b'

instance Lattice_ a => Lattice_ (History_ disp a) where
    {-# INLINABLE sup #-}
    sup a b = do
        a' <- a
        b' <- b
        return $ sup a' b'

instance MinBound_ a => MinBound_ (History_ disp a) where
    minBound = return $ minBound

instance Bounded a => Bounded (History_ disp a) where
    maxBound = return $ maxBound

instance Heyting a => Heyting (History_ disp a) where
    (==>) a b = do
        a' <- a
        b' <- b
        return $ a' ==> b'

instance Complemented a => Complemented (History_ disp a) where
    not a = do
        a' <- a
        return $ not a'

instance Boolean a => Boolean (History_ disp a)

