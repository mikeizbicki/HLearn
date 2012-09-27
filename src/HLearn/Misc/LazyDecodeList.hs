module HLearn.Misc.LazyDecodeList
    where
          
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Int
import Data.List
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map as Map
import Debug.Trace
import System.IO.Unsafe

import Data.Binary
          
-------------------------------------------------------------------------------
-- lazy binary streaming to/from list

-- taken from http://stackoverflow.com/questions/6205294/binary-serialization-for-lists-of-undefined-length-in-haskell
newtype Stream a = Stream { unstream :: [a] }
    deriving (Show,Read)

instance Binary a => Binary (Stream a) where
    put (Stream [])     = putWord8 0
    put (Stream (x:xs)) = putWord8 1 >> put x >> put (Stream xs)

    get = do
        t <- getWord8
        case t of
            0 -> return (Stream [])
            1 -> do x         <- get
                    Stream xs <- get
                    return (Stream (x:xs))

-- lazy decoding
-- taken from http://stackoverflow.com/questions/11695373/lazy-decoding-of-a-list-with-data-binary

getMaybe :: Binary a => Get (Maybe a)
getMaybe = do
    t <- getWord8
    case t of
      0 -> return Nothing
      _ -> fmap Just get
      
step :: Binary a => (BSL.ByteString,Int64) -> Maybe (a,(BSL.ByteString,Int64))
step (xs,offset) = case runGetState getMaybe xs offset of
                     (Just v, ys, newOffset) -> Just (v,(ys,newOffset))
                     _                       -> Nothing

tmp  = encode $ Stream [1 .. 10 :: Integer]
tmp' = encode $ Stream [1 .. 100000000 :: Integer]

lazyDecodeList :: Binary a => BSL.ByteString -> [a]
lazyDecodeList xs = unfoldr step ({-BSL.take 101 -}xs,0)

lazyDecodeStream :: Binary a => BSL.ByteString -> Stream a
lazyDecodeStream = Stream . lazyDecodeList

decodeStream :: BSL.ByteString -> [Integer]
decodeStream bsl = case BSL.head bsl of
                        '\0' -> []
                        '\1' -> ret
    where
        ret = case (BSL.readInteger $ BSL.tail bsl) of
                 Nothing -> error "decodeStream barf"
                 Just (x,bsl') -> trace (show x) $ x:(decodeStream bsl')

lazyDecodeFile :: (Binary a) => FilePath -> IO [a]
lazyDecodeFile file = liftM lazyDecodeList $ BSL.readFile file
