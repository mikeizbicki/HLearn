module HLearn.Data.LoadData
    where

import Control.DeepSeq
-- import Control.Monad
import Control.Monad.ST
import Data.Maybe
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Algorithms.Intro as Intro
-- import System.Mem
import System.IO
import System.Directory
-- import System.FilePath.Posix
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Devel as LA

import HLearn.Models.Distributions

import Data.List (take,drop,zipWith)

import SubHask
import SubHask.Algebra.Container
import SubHask.Algebra.Parallel
import SubHask.Compatibility.ByteString
import SubHask.Compatibility.Cassava
import SubHask.Compatibility.Containers
import SubHask.Compatibility.Vector
import SubHask.Compatibility.Vector.Lebesgue
import SubHask.TemplateHaskell.Deriving

import HLearn.Data.UnsafeVector
import HLearn.History.Timing
import HLearn.Models.Classifiers.Common

import Debug.Trace
import Prelude (asTypeOf,unzip,head)
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
        VGM.write ret (dp-1) $ insertAt dim (fromIntegral val) curdp

    hClose hin
    VG.unsafeFreeze ret

-- | Loads a dataset of strings in the unix words file format (i.e. one word per line).
-- This format is also used by the UCI Bag Of Words dataset.
-- See: https://archive.ics.uci.edu/ml/machine-learning-databases/bag-of-words/readme.txt
loadWords :: (Monoid dp, Elem dp~Char, Eq dp, Constructible dp) => FilePath -> IO (Array dp)
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

-- | Load a CSV file containing numeric attributes.
{-# INLINABLE loadCSV #-}
loadCSV ::
    ( NFData a
    , FromRecord a
    , FiniteModule a
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
    putStrLn $ "    num dp:  " ++ show ( size rs )
    putStrLn $ "    numdim:  " ++ show ( dim $ rs!0 )
    putStrLn ""

    return rs

-------------------------------------------------------------------------------
-- data preprocessing

-- | This exists only for the varshift right now and doesn't actually work
--
-- FIXME: move
newtype Componentwise v = Componentwise { unComponentwise :: v }

type instance Scalar (Componentwise v) = Scalar v
type instance Logic (Componentwise v) = Logic v
type instance Elem (Componentwise v) = Scalar v

instance IsMutable (Componentwise v)

instance Eq_ v => Eq_ (Componentwise v) where
    (Componentwise v1)==(Componentwise v2) = v1==v2

instance Semigroup v => Semigroup (Componentwise v) where
    (Componentwise v1)+(Componentwise v2) = Componentwise $ v1+v2

instance Monoid v => Monoid (Componentwise v) where
    zero = Componentwise zero

instance Abelian v => Abelian (Componentwise v)

instance Cancellative v => Cancellative (Componentwise v) where
    (Componentwise v1)-(Componentwise v2) = Componentwise $ v1-v2

instance Group v => Group (Componentwise v) where
    negate (Componentwise v) = Componentwise $ negate v

instance FreeModule v => Rg (Componentwise v) where
    (Componentwise v1)*(Componentwise v2) = Componentwise $ v1.*.v2

instance FiniteModule v => Rig (Componentwise v) where
    one = Componentwise $ ones

instance FiniteModule v => Ring (Componentwise v)

instance (FiniteModule v, VectorSpace v) => Field (Componentwise v) where
    (Componentwise v1)/(Componentwise v2) = Componentwise $ v1./.v2

instance (ValidLogic v, FiniteModule v) => IxContainer (Componentwise v) where
    values (Componentwise v) = values v

-- | Uses an efficient 1-pass algorithm to calculate the mean variance.
-- This is much faster than the 2-pass algorithms on large datasets,
-- but has (slightly) worse numeric stability.
--
-- See http://www.cs.berkeley.edu/~mhoemmen/cs194/Tutorials/variance.pdf for details.
--
-- FIXME: Find a better location for this
{-# INLINE meanAndVarianceInOnePass  #-}
meanAndVarianceInOnePass :: (Foldable xs, Field (Elem xs)) => xs -> (Elem xs, Elem xs)
meanAndVarianceInOnePass ys = case uncons ys of
    Nothing -> error "meanAndVarianceInOnePass on empty container"
    Just (x,xs) -> (\(k,m,v) -> (m,v/(k-1))) $ foldl' go (2,x,0) xs
    where
        go (k,mk,qk) x = (k+1,mk',qk')
            where
                mk'=mk+(x-mk)/k
                qk'=qk+(k-1)*(x-mk)*(x-mk)/k

-- | A wrapper around "meanAndVarianceInOnePass"
--
-- FIXME: Find a better location for this
{-# INLINE varianceInOnePass  #-}
varianceInOnePass :: (Foldable xs, Field (Elem xs)) => xs -> Elem xs
varianceInOnePass = snd . meanAndVarianceInOnePass

-- FIXME: hack due to lack of forall'd constraints
type ValidElem v =
    ( Elem (SetElem v (Elem v)) ~ Elem v
    , Elem (SetElem v (Scalar (Elem v))) ~ Scalar (Elem v)
    , IxContainer (SetElem v (Elem v))
    )

-- | calculate the variance of each column, then sort so that the highest variance is first
--
-- NOTE: the git history has a lot of versions of this function with different levels of efficiency.
{-# INLINABLE mkShuffleMap #-}
mkShuffleMap :: forall v.
    ( FiniteModule v
    , VectorSpace v
    , Eq v
    , ValidElem v
    ) => Array v -> UnboxedArray Int
mkShuffleMap vs = if size vs==0
    then error "mkShuffleMap: called on empty array"
    else runST ( do
--         let variances = fromList
--                 $ values
--                 $ varianceInOnePass
--                 $ VG.map Componentwise vs
--                 :: Array (Scalar v)

        let variances
                = fromList
                $ imap (\i _ -> varianceInOnePass $ (VG.map (!i) vs))
                $ values
                $ VG.head vs
                :: Array (Scalar v)

        msortV <- VG.unsafeThaw $ imap (,) $ variances
        Intro.sortBy (\(_,v1) (_,v2) -> compare v1 v2) msortV
        sortV' :: Array (Int,Scalar v) <- VG.unsafeFreeze msortV

        return $ VG.convert $ VG.map fst sortV'
        )

-- | apply the shufflemap to the data set to get a better ordering of the data
{-# INLINABLE apShuffleMap #-}
apShuffleMap :: FiniteModule v => UnboxedArray Int -> v -> v
apShuffleMap vmap v = unsafeToModule $ V.toList $ V.generate (size vmap) $ \i -> v!(vmap!i)

{-# INLINABLE meanCenter #-}
-- | translate a dataset so the mean is zero
meanCenter ::
    ( VG.Vector v1 (v2 a)
    , VG.Vector v2 a
    , Real a
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
--         rotate dp = VG.convert $ LA.single $ eigm LA.<> LA.double (VG.convert dp :: VS.Vector Float)
        rotate dp = {-# SCC convert #-} VG.convert $ LA.single $ (LA.trans eigm) LA.<> LA.double (VG.convert dp :: VS.Vector Float)
        dps =  meanCenter dps'

        (eigv,eigm) = {-# SCC eigSH #-} LA.eigSH $ LA.double gramMatrix

--         gramMatrix = {-# SCC gramMatrix #-} gramMatrix_ $ map VG.convert $ VG.toList dps
--         gramMatrix = {-# SCC gramMatrix #-} LA.trans tmpm LA.<> tmpm
--             where
--                 tmpm = LA.fromLists (VG.toList $ VG.map VG.toList dps)

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
