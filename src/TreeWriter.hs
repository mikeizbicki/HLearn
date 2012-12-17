{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TreeWriter
    where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer hiding ((<>))
import Data.Functor.Identity
import qualified Data.Foldable as F
import Data.Semigroup

-------------------------------------------------------------------------------
-- Depth

newtype DepthLogger l a = DepthLogger { extract :: StateT Int (Writer [(Int,l)]) a }
-- newtype DepthLogger l a = DepthLogger { extract :: (Writer l a) }

instance (Semigroup a) => Semigroup (DepthLogger l a) where
    (<>) (DepthLogger m1) (DepthLogger m2) = DepthLogger $ do
        depth <- get
        let ((a1,d1),xs1) = runWriter $ runStateT m1 depth
        let ((a2,d2),xs2) = runWriter $ runStateT m2 depth
        if d1/=d2
            then error "DepthLogger.(<>): d1 /= d2"
--             else return $ writer (a1<>a2,xs1<>xs2)
            else return $ a1<>a2

reduce :: (Semigroup sg, F.Foldable f) => f sg -> sg
reduce = F.foldl1 (<>)

instance (Monoid l) => Monad (DepthLogger l) where
    (>>=) (DepthLogger m) f = DepthLogger $ m >>= (extract . f)
    return x = DepthLogger $ return x
    
runDepthLogger :: DepthLogger l a -> (a,[(Int,l)])
runDepthLogger (DepthLogger m) = (a,xs)
    where
--         (wm,d) = undefined -- execStateT m 0
        ((a,d),xs) = runWriter $ runStateT m 0

-- down1 :: DepthLogger l a -> DepthLogger l a
-- down1 

descend :: DepthLogger l a -> DepthLogger l a
descend action = DepthLogger $ do
    depth <- get
    put (depth+1)
    ret <- extract $ action
    put depth
    return ret

write :: l -> DepthLogger l ()
write l = DepthLogger $ do 
    depth <- get
    tell [(depth,l)]

testMonad = runDepthLogger $ do
    write "funky"
    write $ show . runDepthLogger $ reduce $ map (write . show) [1,2,3,4,5,6,7,8,9,10::Int]
    descend $ do
        write "poop"
        write "pee"
        descend $ do
            write "finally"
            write "free"
    write "monkey"
    
--     (>>=) (DepthLog a) f = f a
--         where
--             (DepthLog a') 
--     return a = DepthLog $ return a

-- data Depth a = Depth { runDepth :: (Int,a) }
--     deriving Show
-- 
-- instance Monad Depth where
--     (>>=) (Depth (d,a)) f = Depth ((d+d'),a')
--         where Depth (d',a') = f a
--     return a = Depth (0,a)
--     
-- descend :: Depth a -> Depth a
-- descend (Depth (d,a)) = do
--     Depth (d+1,a)
-- 
-- testDepth :: Depth ()
-- testDepth = do
--     descend $ do
--         descend $ do
--             return ()
--         