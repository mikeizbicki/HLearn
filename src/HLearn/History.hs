module HLearn.History
    (
    DynamicHistory
    , runDynamicHistory
    , runDynamicHistoryWithState
--     , report
--     , collectReports
--     , prevReport
--     , currentItr

    , SimpleHistory
    , runSimpleHistory

    , HistoryMonad (..)
    , Reportable (..)
    , Report (..)
    , CPUTime

    , mkDisplayFunction
    , idDisplayFilter
    , idDisplayMethod
    , DisplayFunction
    , DisplayFilter
    , DisplayFilter'
    , DisplayMethod
    , DisplayMethod'
    , StartCollection (..)
    , StartHistory (..)
    , EndHistory (..)

    , module Control.Applicative
    , module Control.DeepSeq
    , module Control.Lens
    , module Control.Monad
    , module Data.Typeable
    , module Data.Dynamic
    )
    where

import Control.Applicative
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Monad.Trans
import Data.Dynamic
import Data.Typeable
import System.CPUTime
import System.IO

import Pipes

import SubHask hiding (Functor(..), Applicative(..), Monad(..), Then(..), fail, return)

-------------------------------------------------------------------------------
-- data types

class (Functor m, Monad m, Boolean (m Bool)) => HistoryMonad m where
    type Reportable m a :: Constraint
    type Reportable m a = ()

    data Report m

    report :: Reportable m a => a -> m a
    collectReports :: m a -> m a
    prevReport :: m (Report m)
    currentItr :: Ring r => m r

type CPUTime = Integer

data StartCollection = StartCollection
    deriving (Show,Typeable)

data StartHistory = StartHistory
    deriving (Show,Typeable)

data EndHistory = EndHistory
    deriving (Show,Typeable)

type DisplayFilter = DisplayFilter' ()
type DisplayMethod = DisplayMethod' ()

type DisplayFilter' s = Report DynamicHistory -> StateT s IO Bool
type DisplayMethod' s = Report DynamicHistory -> StateT s IO ()


-------------------------------------------------------------------------------

type DisplayFunction = Report DynamicHistory -> String

mkDisplayFunction :: forall a. Typeable a => (a -> String) -> DisplayFunction
mkDisplayFunction f x = case fromDynamic $ dyn x :: Maybe a of
    Nothing -> ""
    Just y -> f y

{-# INLINE idDisplayFilter #-}
idDisplayFilter :: DisplayFilter
idDisplayFilter _ = return True

{-# INLINE idDisplayMethod #-}
idDisplayMethod :: DisplayMethod
idDisplayMethod _ = return ()

-------------------------------------------------------------------------------
-- SimpleHistory

newtype SimpleHistory a = SimpleHistory (State [Int] a)
    deriving (Functor,Applicative,Monad)

type instance Logic (SimpleHistory a) = SimpleHistory (Logic a)

runSimpleHistory :: SimpleHistory a -> a
runSimpleHistory (SimpleHistory m) = (fst.runState m) [0]

instance Eq_ a => Eq_ (SimpleHistory a) where
    a==b = do
        a' <- a
        b' <- b
        return $ a'==b'

instance POrd_ a => POrd_ (SimpleHistory a) where
    {-# INLINABLE inf #-}
    inf a b = do
        a' <- a
        b' <- b
        return $ inf a' b'

instance Lattice_ a => Lattice_ (SimpleHistory a) where
    {-# INLINABLE sup #-}
    sup a b = do
        a' <- a
        b' <- b
        return $ sup a' b'

instance MinBound_ a => MinBound_ (SimpleHistory a) where
    minBound = return $ minBound

instance Bounded a => Bounded (SimpleHistory a) where
    maxBound = return $ maxBound

instance Heyting a => Heyting (SimpleHistory a) where
    (==>) a b = do
        a' <- a
        b' <- b
        return $ a' ==> b'

instance Complemented a => Complemented (SimpleHistory a) where
    not a = do
        a' <- a
        return $ not a'

instance Boolean a => Boolean (SimpleHistory a)

instance HistoryMonad SimpleHistory where
    type Reportable SimpleHistory a = ()
    newtype Report SimpleHistory = Report_SimpleHistory Int

    {-# INLINE report #-}
    report a = SimpleHistory $ do
        x:xs <- get
        put $ (x+1):xs
        return a

    {-# INLINE collectReports #-}
    collectReports (SimpleHistory m) = SimpleHistory $ do
        x:xs <- get
        put $ 0:x:xs
        a <- m
        put $ x:xs
        return a

    {-# INLINE prevReport #-}
    prevReport = SimpleHistory $ do
        x:_ <- get
        return $ Report_SimpleHistory x

    {-# INLINE currentItr #-}
    currentItr = SimpleHistory $ do
        x:_ <- get
        return $ fromIntegral x
--     type Reportable m a :: Constraint
--     type Reportable m a = ()
--
--     data Report m
--
--     report :: Reportable m a => a -> m a
--     collectReports :: m a -> m a
--     prevReport :: m (Report m)
--     currentItr :: Ring r => m r


-------------------------------------------------------------------------------
-- DynamicHistory

newtype DynamicHistory a = DynamicHistory (Producer (Report DynamicHistory) (StateT [Report DynamicHistory] IO) a)
    deriving (Functor,Applicative,Monad)

type instance Logic (DynamicHistory a) = DynamicHistory (Logic a)

instance Eq_ a => Eq_ (DynamicHistory a) where
    a==b = do
        a' <- a
        b' <- b
        return $ a'==b'

runDynamicHistory :: Monoid s => DisplayMethod' s -> DynamicHistory a -> IO a
runDynamicHistory = runDynamicHistoryWithState zero

runDynamicHistoryWithState :: forall s a. s -> DisplayMethod' s -> DynamicHistory a -> IO a
runDynamicHistoryWithState s f hist = do
    time <- getCPUTime
    let startReport =
            Report
                { cpuTime       = time
                , cpuTimeDiff   = 0
                , dyn           = toDyn StartCollection
                , numReports    = 0
                , reportLevel   = 0
                }
    let (DynamicHistory hist') = do
            report StartHistory
            a <- hist
            report EndHistory
            return a
    evalStateT (runEffect $ hist' >-> go s) [startReport]

    where
        go s = do
            next <- await
            ((),s') <- liftIO $ runStateT (f next) s
            go s'

instance POrd_ a => POrd_ (DynamicHistory a) where
    {-# INLINABLE inf #-}
    inf a b = do
        a' <- a
        b' <- b
        return $ inf a' b'

instance Lattice_ a => Lattice_ (DynamicHistory a) where
    {-# INLINABLE sup #-}
    sup a b = do
        a' <- a
        b' <- b
        return $ sup a' b'

instance MinBound_ a => MinBound_ (DynamicHistory a) where
    minBound = return $ minBound

instance Bounded a => Bounded (DynamicHistory a) where
    maxBound = return $ maxBound

instance Heyting a => Heyting (DynamicHistory a) where
    (==>) a b = do
        a' <- a
        b' <- b
        return $ a' ==> b'

instance Complemented a => Complemented (DynamicHistory a) where
    not a = do
        a' <- a
        return $ not a'

instance Boolean a => Boolean (DynamicHistory a)

instance HistoryMonad DynamicHistory where

    type Reportable DynamicHistory a = Typeable a

    data Report DynamicHistory = Report
        { cpuTime       :: {-#NOUNPACK#-}!CPUTime
        , cpuTimeDiff   :: {-#NOUNPACK#-}!CPUTime
        , dyn           :: {-#UNPACK#-}!Dynamic
        , numReports    :: {-#UNPACK#-}!Int
        , reportLevel   :: {-#UNPACK#-}!Int
        }
        deriving Show


    {-# INLINE report #-}
    report a = {-# SCC report #-} DynamicHistory $ do
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

    {-# INLINE collectReports #-}
    collectReports (DynamicHistory hist) = {-# SCC collectReports #-} DynamicHistory $ do
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

    {-# INLINE prevReport #-}
    prevReport = {-# SCC prevReport #-} DynamicHistory $ do
        x:_ <- get
        return x

    {-# INLINE currentItr #-}
    currentItr = {-# SCC currentItr #-} DynamicHistory $ do
        x:_ <- get
        return . fromInteger . fromIntegral $ numReports x
