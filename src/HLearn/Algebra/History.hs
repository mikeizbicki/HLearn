module HLearn.Algebra.History
    where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
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

newtype History a = History (WriterT (DList.DList Event) (StateT StateStack IO) a)
-- newtype History a = History (WriterT (DList.DList Event) (StateT StateStack Identity) a)
    deriving (Functor,Applicative,Monad)

deriving instance Typeable DList.DList 

data Event = Event
    { dyn :: Dynamic
    , stoptime :: Integer
    , runtime :: Integer
    , count :: Int
    }
    deriving (Show,Typeable)


runHistory :: History a -> IO (a,[Event])
-- runHistory (History w) = do
--     let ((a,xs),_) = runState (runWriterT w) emptyStack
--     return (a,DList.toList xs)
runHistory (History w) = do
    ((a,xs),_) <- runStateT (runWriterT w) emptyStack
    return (a,DList.toList xs)

unsafeRunHistory :: History a -> (a,[Event])
-- unsafeRunHistory (History w) = 
--     let ((a,xs),_) = runState (runWriterT w) emptyStack
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
    History $ do
        let dyn = toDyn a
--             time = unsafePerformIO $ getCPUTime
        time <- liftIO getCPUTime
        let runtime = case event0 of
                Nothing -> 0
                Just x -> time-stoptime x
        let event = Event dyn time runtime (numEvents+1)
        modify (addEvent event)
        seq time $ tell $ DList.singleton event

report :: Typeable a => a -> History a
report a = event a >> return a

collectEvents :: History a -> History a
collectEvents (History m) = History $ do
    modify $ push []
    a <- censor (\ws -> DList.singleton $ Event (toDyn ws) 0 0 0) m
    modify $ snd . fromJust . pop
    return a
