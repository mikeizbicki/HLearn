module HLearn.Data.LoadData
    where

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.Maybe
import qualified Data.List as L
import qualified Data.Set as Set
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
-- import System.Mem
import System.IO
import System.Directory
-- import System.FilePath.Posix
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Devel as LA

import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Univariate.Normal
-- import Test.QuickCheck hiding (verbose,sample,label)
-- import Control.Parallel.Strategies

import SubHask hiding (Functor(..), Applicative(..), Monad(..), Then(..), fail, return)
import SubHask.Algebra.Container
import SubHask.Algebra.Parallel
import SubHask.Compatibility.ByteString
import SubHask.Compatibility.Cassava
import SubHask.Compatibility.Containers
import SubHask.Compatibility.Vector.Lebesgue
import SubHask.TemplateHaskell.Deriving

import HLearn.Data.UnsafeVector
import HLearn.History.Timing
import HLearn.Models.Classifiers.Common

import Debug.Trace
import Prelude (asTypeOf,unzip)
import qualified Prelude as P

--------------------------------------------------------------------------------

-- | This loads files in the format used by the BagOfWords UCI dataset.
-- See: https://archive.ics.uci.edu/ml/machine-learning-databases/bag-of-words/readme.txt
loadBagOfWords :: FilePath -> IO (Array (Map' Int Float))
loadBagOfWords filepath = do
    hin <- openFile filepath ReadMode
    numdp :: Int <- liftM read $ hGetLine hin
    numdim :: Int <- liftM read $ hGetLine hin
    numlines :: Int <- liftM read $ hGetLine hin

    ret <- VGM.replicate numdp zero
    forM [0..numlines-1] $ \i -> do
        line <- hGetLine hin
        let [dp,dim,val] :: [Int] = map read $ L.words line
        curdp <- VGM.read ret (dp-1)
        VGM.write ret (dp-1) $ insert (dim,fromIntegral val) curdp

    hClose hin
    VG.unsafeFreeze ret

-- | Loads a dataset of strings in the unix words file format (i.e. one word per line).
-- This format is also used by the UCI Bag Of Words dataset.
-- See: https://archive.ics.uci.edu/ml/machine-learning-databases/bag-of-words/readme.txt
loadWords :: (Elem dp~Char, Eq dp, Unfoldable dp) => FilePath -> IO (Array dp)
loadWords filepath = do
    hin <- openFile filepath ReadMode
    contents <- hGetContents hin
    return $ fromList $ map fromList $ L.lines contents


--------------------------------------------------------------------------------

-- | Returns all files in a subdirectory (and all descendant directories).
-- Unlike "getDirectoryContents", this function prepends the directory's path to each filename.
-- This is important so that we can tell where in the hierarchy the file is located.
--
-- FIXME:
-- This is relatively untested.
-- It probably has bugs related to weird symbolic links.
getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive = fmap toList . go
    where
        go :: FilePath -> IO (Seq FilePath)
        go dirpath = do
            files <- getDirectoryContents dirpath
            fmap concat $ forM files $ \file -> case file of
                '.':_ -> return empty
                _ -> do
                    let file' = dirpath++"/"++file
                    isdir <- doesDirectoryExist file'
                    contents <- if isdir
                        then go file'
                        else return empty
                    return $ file' `cons` contents

-- | A generic method for loading data points.
-- Each file in a directory hierarchy corresponds to a single data point.
--
-- The label assigned to the data point is simply the name of the file.
-- This means each data point will have a distinct label.
-- For typical supervised learning tasks, you will want to prune the
loadDirectory ::
    ( Eq a
    , NFData a
    ) => Maybe Int            -- ^ maximum number of datapoints to load; Nothing for unlimitted
      -> (FilePath -> IO a)   -- ^ function to load an individual file
      -> (FilePath -> Bool)   -- ^ function to filter out invalid filenames
      -> (a -> Bool)          -- ^ function to filter out malformed results
      -> FilePath             -- ^ directory to load data from
      -> IO (Array (Labeled' a FilePath))         -- ^
loadDirectory numdp loadFile validFilepath validResult dirpath = {-# SCC loadDirectory #-} do

    files <- timeIO "getDirectoryContentsRecursive" $ do
        xs <- getDirectoryContentsRecursive dirpath

        let takedp = case numdp of
                Nothing -> id
                Just n -> fmap (L.take n)

        return $ takedp $ L.filter validFilepath xs

    results <- timeIO "loadDirectory" $ do
        xs <- forM files $ \filepath -> do
            res <- loadFile filepath
            return $ Labeled' res filepath
        return $ L.filter (validResult . xLabeled') xs

    putStrLn $ "  numdp: " ++ show (length files)
--     when debug $ do
--         forM files $ \file -> do
--             putStrLn file

    return $ fromList results

-------------------


--------------------------------------------------------------------------------

data DataParams = DataParams
    { datafile :: String
    , labelcol :: Maybe Int
    , pca      :: Bool
    , varshift :: Bool
    }

head x = case unCons x of
    Nothing -> error "head on empty"
    Just (x,_) -> x

{-# INLINABLE loadCSV #-}
loadCSV ::
    ( NFData a
    , FromRecord a
    , Eq a
    , Show (Scalar a)
    ) => FilePath -> IO (Array a)
loadCSV filepath = do

    bs <- timeIO ("loading ["++filepath++"]") $ readFileByteString filepath

    let rse = decode NoHeader bs
    time "parsing csv file" rse

    rs <- case rse of
        Right rs -> return rs
        Left str -> error $ "failed to parse CSV file " ++ filepath ++ ": " ++ L.take 1000 str

    putStrLn "  dataset info:"
    putStrLn $ "    num dp:  " ++ show (size rs)
    putStrLn ""

    return rs

{-# INLINABLE loaddata #-}
loaddata ::
    ( VG.Vector v f
    , NFData (v f)
    , NFData  f
    , FromRecord (v f)
    , Eq (v f)
    , Ord f
    , POrd_ (v f)
--     , Normed (v f)
    , Show (Scalar (v f))
    , Floating f
    , f~Float
    , VUM.Unbox f
    ) => DataParams -> IO (Array (v f))
loaddata params = do
    rs <- loadCSV $ datafile params

    -- These algorithms are implemented sequentially,
    -- so they run faster if we temporarily disable the RTS multithreading
    disableMultithreading $ do

        rs' <- if pca params
            then time "calculating PCA" $ VG.convert $ rotatePCA rs
            else return rs

        let shuffleMap = mkShuffleMap $ VG.map ArrayT rs'
        time "mkShuffleMap" shuffleMap

        rs'' <- if varshift params
            then time "varshifting data" $ VG.map (shuffleVec shuffleMap) rs'
            else return rs'

        return $ rs''


-------------------------------------------------------------------------------
-- data preprocessing

-- | calculate the variance of each column, then sort so that the highest variance is first
{-# INLINABLE mkShuffleMap #-}
mkShuffleMap :: forall v a s.
    ( VG.Vector v a
    , Elem (v a) ~ a
    , Foldable (v a)
    , Floating a
    , Ord a
    , VU.Unbox a
    , Eq_ (v a)
    , ClassicalLogic (v a)
    , NFData a
    , a~Float
    ) => Array (v a) -> UnboxedArray Int
mkShuffleMap v = if VG.length v == 0
    then empty
    else runST ( do
        let numdim = VG.length (v VG.! 0)

        -- Use an efficient 1-pass algorithm for the variance.
        -- This is much faster (due to cache issues) than 2-pass algorithms on large datasets.
        -- Since this is only used as a speed heuristic, perfect numerical stability doesn't matter.
        -- See http://www.cs.berkeley.edu/~mhoemmen/cs194/Tutorials/variance.pdf
        let varV = VG.map ((\(_,_,q) -> q) . VG.foldl' go (1,0,0)) v
            go (k,mk,qk) x = (k+1,mk',qk')
                where
                    mk'=mk+(x-mk)/k
                    qk'=qk+(x-mk)*(x-mk)

            sortV = VG.zip (VG.fromList [0..numdim-1::Int])
                  $ VG.convert varV :: UnboxedArray (Int, a)

        msortV <- VG.unsafeThaw sortV
        Intro.sortBy (\(_,v2) (_,v1) -> compare v1 v2) msortV
        sortV' <- VG.unsafeFreeze msortV

        return $ VG.map fst sortV'
        )


{-# INLINABLE shuffleVec #-}
-- | apply the shufflemap to the data set to get a better ordering of the data
shuffleVec :: VG.Vector v a => UnboxedArray Int -> v a -> v a
shuffleVec vmap v = VG.generate (VG.length vmap) $ \i -> v `VG.unsafeIndex` (vmap `VG.unsafeIndex` i)

{-# INLINABLE meanCenter #-}
-- | translate a dataset so the mean is zero
meanCenter ::
    ( VG.Vector v1 (v2 a)
    , VG.Vector v2 a
    , Floating a
    ) => v1 (v2 a) -> v1 (v2 a)
meanCenter dps = {-# SCC meanCenter #-} VG.map (\v -> VG.zipWith (-) v meanV) dps
    where
        meanV = {-# SCC meanV #-} VG.map (/ fromIntegral (VG.length dps)) $ VG.foldl1' (VG.zipWith (+)) dps

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
        rotate dp = {-# SCC convert #-} VG.convert $ LA.single $ (LA.trans eigm) LA.<> LA.double (VG.convert dp :: VS.Vector Float)
        dps =  meanCenter dps'

        (eigv,eigm) = {-# SCC eigSH #-} LA.eigSH $ LA.double gramMatrix

        gramMatrix = {-# SCC gramMatrix #-} foldl1' (P.+)
            [ let dp' = VG.convert dp in LA.asColumn dp' LA.<> LA.asRow dp' | dp <- VG.toList dps ]

gramMatrix_ :: (Ring a, Storable a) => [Vector a] -> LA.Matrix a
gramMatrix_ xs = runST ( do
    let dim = VG.length (head xs)
    m <- LA.newMatrix 0 dim dim

    forM_ xs $ \x -> do
        forM_ [0..dim-1] $ \i -> do
            forM_ [0..dim-1] $ \j -> do
                mij <- LA.unsafeReadMatrix m i j
                LA.unsafeWriteMatrix m i j $ mij + (x `VG.unsafeIndex` i)*(x `VG.unsafeIndex` j)

    LA.unsafeFreezeMatrix m
    )


{-# INLINABLE rotatePCADouble #-}
-- | rotates the data using the PCA transform
rotatePCADouble ::
    ( VG.Vector container (v Double)
    , VG.Vector container [Double]
    , VG.Vector v Double
    ) => container (v Double) -> container (v Double)
rotatePCADouble dps' =  VG.map rotate dps
    where
        rotate dp = VG.convert $ (LA.trans eigm) LA.<> (VG.convert dp :: VS.Vector Double)
        dps = meanCenter dps'

        (eigv,eigm) =  LA.eigSH gramMatrix

        gramMatrix =  LA.trans tmpm LA.<> tmpm
            where
                tmpm = LA.fromLists (VG.toList $ VG.map VG.toList dps)

-------------------------------------------------------------------------------
-- tests

v1 = VS.fromList [1,2,3]
v2 = VS.fromList [1,3,4]
v3 = VS.fromList [1,5,6]
v4 = VS.fromList [0,0,1]
vs = V.fromList [v1,v2,v3,v4] :: V.Vector (VS.Vector Float)

dist a b = distance (L2 a) (L2 b)

