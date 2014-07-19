{-# LANGUAGE ScopedTypeVariables,TemplateHaskell,DeriveDataTypeable,DataKinds,FlexibleInstances,TypeFamilies,RankNTypes,BangPatterns,FlexibleContexts,StandaloneDeriving,GeneralizedNewtypeDeriving,TypeOperators,MultiParamTypeClasses #-}

module LoadData
    where

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.Csv
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Vector.Algorithms.Intro as Intro
import System.Console.CmdArgs.Implicit
import System.Mem
import System.IO
import qualified Numeric.LinearAlgebra as LA

import Test.QuickCheck hiding (verbose,sample)
import Control.Parallel.Strategies

import HLearn.Algebra
-- import HLearn.DataStructures.StrictVector
-- import HLearn.DataStructures.CoverTree
-- import HLearn.DataStructures.SpaceTree
-- import HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor
-- import HLearn.DataStructures.SpaceTree.Algorithms.RangeSearch
-- import HLearn.DataStructures.SpaceTree.DualTreeMonoids
-- import qualified HLearn.DataStructures.StrictList as Strict
-- import qualified HLearn.DataStructures.StrictVector as Strict
import HLearn.Metrics.Lebesgue
import HLearn.Metrics.Mahalanobis
import HLearn.Metrics.Mahalanobis.Normal
-- import HLearn.Models.Distributions

import Timing
import HLearn.UnsafeVector

import Debug.Trace

-------------------------------------------------------------------------------

data DataParams = DataParams
    { datafile :: String
    , pca      :: Bool
    , varshift :: Bool
    }
    
{-# INLINABLE loaddata #-}
loaddata :: 
    ( VG.Vector v Float 
    , NFData (v Float)
    , FromRecord (v Float)
    ) => DataParams -> IO (V.Vector (v Float))
loaddata params = do
    rse :: Either String (V.Vector (v Float)) 
        <- timeIO ("loading ["++datafile params++"] ") 
         $ fmap (decode NoHeader) 
         $ BS.readFile (datafile params)
    rs <- case rse of 
        Right rs -> return rs
        Left str -> error $ "failed to parse CSV file " ++ datafile params ++ ": " ++ take 1000 str

    setptsize $ VG.length $ VG.head rs

    putStrLn "  dataset info:"
    putStrLn $ "    num dp:  " ++ show (VG.length rs)
    putStrLn $ "    num dim: " ++ show (VG.length $ rs V.! 0)
    putStrLn ""

    rs' <- if pca params
        then time "calculating PCA" $ VG.convert $ rotatePCA rs
        else return rs
    
    rs'' <- if varshift params
        then time "varshifting data" $ 
            VG.convert $ VG.map (shuffleVec $ VU.map fst $ mkShuffleMap rs') rs'
        else return rs'

    return rs''

{-# INLINABLE mkShuffleMap #-}
-- | calculate the variance of each column, then sort so that the highest variance is first
mkShuffleMap :: forall v a.
    ( VG.Vector v a
    , Floating a
    , Ord a
    , VU.Unbox a
--     ) => V.Vector (v a) -> VU.Vector Int
    ) => V.Vector (v a) -> VU.Vector (Int,a)
mkShuffleMap v = runST $ do
    let numdim = VG.length (v V.! 0)
        numdpf = fromIntegral $ VG.length v

    let m1V = VG.foldl1 (VG.zipWith (+)) v
        m2V = VG.foldl1 (VG.zipWith (+)) $ VG.map (VG.map (\x -> x*x)) v
        varV = VG.zipWith (\m1 m2 -> m2/numdpf-m1/numdpf*m1/numdpf) m1V m2V

        sortV = VG.zip (VG.fromList [0..numdim-1::Int]) $ VG.convert varV :: VU.Vector (Int, a)

--     meanV :: VUM.MVector s a <- VGM.new numdim
--     varV  :: VUM.MVector s a <- VGM.new numdim
--     let go (-1) = return ()
--         go i = do
--             let go_inner (-1) = return ()
--                 go_inner j = do
--                     let tmp = v VG.! i VG.! j
--                     meanVtmp <- (+tmp    ) `liftM` VGM.read meanV j
--                     varVtmp  <- (+tmp*tmp) `liftM` VGM.read varV  j
--                     VUM.write meanV
-- 
--             let xs   = fmap (VG.! i) v
--                 dist = train xs :: Normal a a
--                 var  = variance dist
--             VGM.write varV i (i,var)
--             go (i-1)

--     msortV :: VUM.MVector s (Int, a) <- VGM.new numdim 
--     
--     let go (-1) = return ()
--         go i = do
-- --             let !xs   = fmap (VG.! i) v
--             let !xs   = fmap (`VG.unsafeIndex` i) v
--                 !dist = train xs :: Normal a a
--                 !var  = variance dist
--             VGM.write msortV i (i,var)
--             go (i-1)
--     go (numdim-1)

--     forM [0..numdim-1] $ \i -> do
--         let xs   = fmap (VG.! i) v
--             dist = train xs :: Normal a a
--             var  = variance dist
--         VGM.write varV i (i,var)

    msortV <- VG.unsafeThaw sortV
    Intro.sortBy (\(_,v2) (_,v1) -> compare v1 v2) msortV
    sortV' <- VG.unsafeFreeze msortV

    return sortV'

{-# INLINABLE shuffleVec #-}
-- | apply the shufflemap to the data set to get a better ordering of the data
shuffleVec :: VG.Vector v a => VU.Vector Int -> v a -> v a
shuffleVec vmap v = VG.generate (VG.length vmap) $ \i -> v VG.! (vmap VG.! i)

-- shuffleVec vmap v = runST $ do
--     ret <- VGM.new (VG.length v)
-- 
--     let go (-1) = return ()
--         go i = do
--             VGM.write ret i $ v VG.! (vmap VG.! i)
--             go (i-1)
--     go (VG.length v-1)
-- 
-- --     forM [0..VG.length v-1] $ \i -> do
-- --         VGM.write ret i $ v VG.! (vmap VG.! i)
--     VG.freeze ret

{-# INLINABLE meanCenter #-}
-- | translate a dataset so the mean is zero
meanCenter :: 
    ( VG.Vector v1 (v2 a)
    , VG.Vector v2 a
    , Floating a
    ) => v1 (v2 a) -> v1 (v2 a)
meanCenter dps = VG.map (\v -> VG.zipWith (-) v meanV) dps
    where   
        meanV = VG.map (/ fromIntegral (VG.length dps)) $ VG.foldl1' (VG.zipWith (+)) dps 

{-# INLINABLE rotatePCA #-}
-- | rotates the data using the PCA transform
rotatePCA :: 
    ( VG.Vector container dp
    , VG.Vector container [Float]
    , VG.Vector v a
    , dp ~ v a
    , Show a
    , a ~ Float
    ) => container dp -> container dp
rotatePCA dps' = {-# SCC rotatePCA #-} VG.map rotate dps
    where
--         rotate dp = VG.convert $ LA.single $ eigm LA.<> LA.double (VG.convert dp :: VS.Vector Float)
        rotate dp = VG.convert $ LA.single $ (LA.trans eigm) LA.<> LA.double (VG.convert dp :: VS.Vector Float)
        dps = meanCenter dps'

        (eigv,eigm) = {-# SCC eigSH #-} LA.eigSH $ LA.double gramMatrix

        gramMatrix = {-# SCC gramMatrix #-} LA.trans tmpm LA.<> tmpm
            where
                tmpm = LA.fromLists (VG.toList $ VG.map VG.toList dps)

--         gramMatrix = {-# SCC gramMatrix #-} foldl1' (+) 
--             [ let dp' = VG.convert dp in LA.asColumn dp' LA.<> LA.asRow dp' | dp <- VG.toList dps ]

-------------------------------------------------------------------------------
-- tests

v1 = VS.fromList [1,2,3]
v2 = VS.fromList [1,3,4]
v3 = VS.fromList [1,5,6]
v4 = VS.fromList [0,0,1]
vs = V.fromList [v1,v2,v3,v4] :: V.Vector (VS.Vector Float)

dist a b = distance (L2 a) (L2 b)

