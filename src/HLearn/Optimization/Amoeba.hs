module HLearn.Optimization.Amoeba
    where

import Prelude
import Control.Monad
import Control.Monad.ST
import Data.List
import Debug.Trace
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Algorithms.Intro as Intro
import Numeric.LinearAlgebra hiding ((<>))
import qualified Numeric.LinearAlgebra as LA

findMinAmoeba f x0 = runST $ do

    -- initialize simplex
    vec <- VM.new (VG.length x0+1)
    VGM.write vec 0 (f x0,x0)
    forM [1..VGM.length vec-1] $ \i -> do
        e_i <- VGM.replicate (VG.length x0) 0
        VGM.write e_i (i-1) 1
        e_i' <- VG.freeze e_i
        let x_i = x0 `LA.add` e_i'
        VGM.write vec i (f x_i,x_i)

    -- iterate
    vec' <- itrM 1000 (stepAmoeba f) vec

    -- return
    (_,ret) <- VGM.read vec 0
    return ret

stepAmoeba f vec = stepAmoebaRaw 1 2 (-1/2) (1/2) f vec

stepAmoebaRaw ::
    ( Fractional b
    , Ord b
    , VGM.MVector vec (b,a)
--     , a ~ LA.Matrix b
    , a ~ LA.Vector b
    , Field b
    , vec ~ VM.MVector
    , b ~ Double
    ) => b
      -> b
      -> b
      -> b
      -> (a -> b)
      -> vec s (b,a)
      -> ST s (vec s (b,a))
stepAmoebaRaw alpha gamma ro sigma f vec = do

    Intro.sortBy (\a b -> compare (fst a) (fst b)) vec

    (f_1,x_1) <- VGM.read vec 0
    (f_2,x_2) <- VGM.read vec 1
    (f_n1,x_n1) <- VGM.read vec $ VGM.length vec -1

    x_0 <- liftM ( scale (1/fromIntegral (VGM.length vec-1))
                 . foldl1' (LA.add)
                 . init
                 . map snd
                 . V.toList
                 ) $ VG.unsafeFreeze vec

    let x_r = x_0 `LA.add` (scale alpha $ x_0 `LA.sub` x_n1)
        f_r = f x_r

        x_e = x_0 `LA.add` (scale gamma $ x_0 `LA.sub` x_n1)
        f_e = f x_e

        x_c = x_0 `LA.add` (scale ro $ x_0 `LA.sub` x_n1)
        f_c = f x_c

    -- check reflection
    if f_1 <= f_r && f_r < f_1
        then VGM.write vec (VGM.length vec-1) (f_r,x_r)

        -- check expansion
        else if f_r < f_1
            then if f_e < f_r
                then VGM.write vec (VGM.length vec-1) (f_e,x_e)
                else VGM.write vec (VGM.length vec-1) (f_r,x_r)

            -- check contraction
            else if f_c < f_n1
                then VGM.write vec (VGM.length vec-1) (f_c,x_c)

                -- reduction
                else forM_ [1..VGM.length vec-1] $ \i -> do
                    (f_i,x_i) <- VGM.read vec i
                    let x_i' = x_1 `LA.add` (scale sigma $ x_i `LA.sub` x_1)
                        f_i' = f x_i'
                    VGM.write vec i (f_i',x_i')

    return vec
--     refMinVal <- newSTRef (-infinity)
--     refMinIndex <- newSTRef 0
--     forM [0..VGM.length vec-1] $ \i -> do
--         ival <- VGM.read vec i
--         minVal <- readSTRef refMinVal
--         if minVal < fst ival
--             then return ()
--             else do
--                 writeSTRef refMinVal $ fst ival
--                 writeSTRef refMinIndex i
--     undefined

itrM :: Monad m => Int -> (a -> m a) -> a -> m a
itrM 0 f a = return a
itrM i f a = do
    a' <- f a
--     if a' == a
--         then trace ("no movement\n   a="++show a++"\n   a'="++show a') $ return ()
--         else trace ("itrM i="++show i++"; f a ="++show a'++"; a="++show a) $ return ()
    itrM (i-1) f a'
