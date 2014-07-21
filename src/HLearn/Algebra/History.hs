module HLearn.Algebra.History
    where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Writer
import Control.DeepSeq
import qualified Data.DList as DList
import qualified Data.Sequence as Seq
import Data.Dynamic
import Data.Maybe
import Data.Typeable
import Debug.Trace

import System.CPUTime
import System.IO.Unsafe

import qualified Control.ConstraintKinds as CK
import HLearn.Algebra

-------------------------------------------------------------------------------
-- simple stack

newtype StateStack = StateStack [[Event]]

emptyStack :: StateStack 
emptyStack = StateStack []

addEvent :: Event -> StateStack -> StateStack
addEvent e (StateStack []) = StateStack [[e]]
addEvent e (StateStack (x:xs)) = StateStack $ (e:x):xs

pop :: StateStack -> Maybe ([Event],StateStack)
pop (StateStack []) = Nothing
pop (StateStack (x:xs)) = Just (x,StateStack xs)

push :: [Event] -> StateStack -> StateStack
push x (StateStack xs) = StateStack (x:xs)

-------------------------------------------------------------------------------
-- History monad

-- newtype History a = History (WriterT (DList.DList Event) (StateT StateStack IO) a)
-- newtype History a = History (WriterT (DList.DList Event) (StateT StateStack Identity) a)
newtype History a = History 
    (WriterT (DList.DList Event) 
        (StateT StateStack 
             (RandT StdGen
                 Identity
--                  IO
--                  LazyIO
             )
        ) 
    a )
    deriving (Functor,Applicative,Monad,MonadRandom)

deriving instance Typeable DList.DList 

-------------------

newtype LazyIO a = LazyIO { strictIO :: IO a }

instance Functor LazyIO where
    fmap f (LazyIO a) = LazyIO $ fmap f a

-- instance Applicative LazyIO where
    
instance Monad LazyIO where
    return a = LazyIO $ return a
    (LazyIO a) >>= f = LazyIO $ a >>= unsafeInterleaveIO . strictIO . f 
--     (LazyIO a) >>= f = LazyIO $ a >>= unsafeGetIO . f 

---------

-- unsafeIO2History :: IO a -> History a
-- unsafeIO2History a = History $ lift $ lift $ lift $ LazyIO a

-------------------
data Event = Event
    { dyn :: Dynamic
    , stoptime :: Integer
    , runtime :: Integer
    , count :: Int
    }
    deriving (Show,Typeable)

data StartHistory = StartHistory

runHistory :: History a -> IO (a,[Event])
runHistory (History w) = do
    let ((a,xs),_) = runIdentity $ flip evalRandT (mkStdGen 0) $ runStateT (runWriterT w) $ push [] emptyStack
    return (a,DList.toList xs)
-- runHistory (History w) = do
--     ((a,xs),_) <- flip evalRandT (mkStdGen 0) $ runStateT (runWriterT w) $ push [] emptyStack
--     return (a,DList.toList xs)
-- runHistory (History w) = do
--     ((a,xs),_) <- strictIO $ flip evalRandT (mkStdGen 0) $ runStateT (runWriterT w) emptyStack
--     return (a,DList.toList xs)

unsafeRunHistory :: History a -> (a,[Event])
-- unsafeRunHistory (History w) = 
--     let ((a,xs),_) = flip evalRand (mkStdGen 0) $ runStateT (runWriterT w) emptyStack
--     in (a,DList.toList xs)
unsafeRunHistory h = unsafePerformIO $ runHistory h

execHistory :: History a -> a
execHistory h = fst $ unsafeRunHistory h

-------------------------------------------------------------------------------
-- helper functions

latestEvents :: History [Event]
latestEvents = History $ do
    stack <- get
    return $ case pop stack of
        Just (xs,_) -> xs
        Nothing -> error "latestEvents: stack empty"

prevEvent :: History (Maybe Event)
prevEvent = do
    xs <- latestEvents
    return $ case xs of
        [] -> Nothing
        (x:_) -> Just x

prevEventOfType :: TypeRep -> History (Maybe Event)
prevEventOfType t = do
    xs <- latestEvents
    return $ go xs
    where
        go [] = Nothing
        go (x:xs) = if eventType x==t
            then Just x
            else go xs

prevValueOfType :: forall a. Typeable a => a -> History (Maybe a)
prevValueOfType t = do
    me <- prevEventOfType (typeOf t)
    return $ case me of
        Nothing -> Nothing
        Just e -> case fromDynamic (dyn e) :: Maybe a of
            Just v -> Just v
            Nothing -> error "prevValueOfType: this case should never happen"

countEvents :: History Int
countEvents = do
    event0 <- prevEvent
    return $ case event0 of
        Nothing -> 0
        Just x -> count x

eventType :: Event -> TypeRep
eventType = dynTypeRep . dyn

event :: Typeable a => a -> History ()
event !a = do
    numEvents <- countEvents
    event0 <- prevEvent
--     time <- unsafeIO2History $ getCPUTime
    History $ do
        let dyn = toDyn a
--             time = unsafePerformIO $ getCPUTime
        time <- return $ unsafePerformIO $ getCPUTime

--         time <- liftIO getCPUTime
        let runtime = case event0 of
                Nothing -> 0
                Just x -> time-stoptime x
        let event = Event dyn time runtime (numEvents+1)
        modify (addEvent event)
        seq time $ tell $ DList.singleton event

report :: Typeable a => a -> History a
report a = {-# SCC report #-} event a >> return a

collectEvents :: History a -> History a
-- collectEvents = id
collectEvents (History m) = {-# SCC collectEvents #-} History $ do
    modify $ push []
    a <- censor (\ws -> DList.singleton $ Event (toDyn ws) 0 0 0) m
    modify $ snd . fromJust . pop
    return a
