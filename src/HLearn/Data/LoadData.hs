-- | This module handles loading data from disk.
module HLearn.Data.LoadData
    where

import SubHask
import SubHask.Algebra.Array
import SubHask.Algebra.Container
import SubHask.Algebra.Parallel
import SubHask.Compatibility.ByteString
import SubHask.Compatibility.Cassava
import SubHask.Compatibility.Containers
import SubHask.TemplateHaskell.Deriving

import HLearn.History.Timing
import HLearn.Models.Distributions

import qualified Prelude as P
import Prelude (asTypeOf,unzip,head,take,drop,zipWith)
import Control.Monad.ST
import qualified Data.List as L
import Data.Maybe
import System.Directory
import System.IO

--------------------------------------------------------------------------------

{-
FIXME:
This code was written a long time ago to assist with the Cover Tree ICML paper.
It needs to be updated to use the new subhask interface.
This should be an easy project.

-- | This loads files in the format used by the BagOfWords UCI dataset.
-- See: https://archive.ics.uci.edu/ml/machine-learning-databases/bag-of-words/readme.txt
loadBagOfWords :: FilePath -> IO (BArray (Map' Int Float))
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
-}

-- | Loads a dataset of strings in the unix words file format (i.e. one word per line).
-- This format is also used by the UCI Bag Of Words dataset.
-- See: https://archive.ics.uci.edu/ml/machine-learning-databases/bag-of-words/readme.txt
loadWords :: (Monoid dp, Elem dp~Char, Eq dp, Constructible dp) => FilePath -> IO (BArray dp)
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
      -> IO (BArray (Labeled' a FilePath))         -- ^
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

    return $ fromList results

-- | Load a CSV file containing numeric attributes.
{-# INLINABLE loadCSV #-}
loadCSV ::
    ( NFData a
    , FromRecord a
    , FiniteModule a
    , Eq a
    , Show (Scalar a)
    ) => FilePath -> IO (BArray a)
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

-- | FIXME: this should be combined with the CSV function above
loadCSVLabeled' ::
    ( NFData x
    , FromRecord x
    , FiniteModule x
    , Eq x
    , Show (Scalar x)
    , Read (Scalar x)
    ) => Int                -- ^ column of csv file containing the label
      -> FilePath           -- ^ path to csv file
      -> IO (BArray (Labeled' x (Lexical String)))
loadCSVLabeled' col filepath = do

    bs <- timeIO ("loading ["++filepath++"]") $ readFileByteString filepath

    let rse = decode NoHeader bs
    time "parsing csv file" rse

    rs :: BArray (BArray String) <- case rse of
        Right rs -> return rs
        Left str -> error $ "failed to parse CSV file " ++ filepath ++ ": " ++ L.take 1000 str

    let ret = fromList $ map go $ toList rs

    putStrLn "  dataset info:"
    putStrLn $ "    num dp:  " ++ show ( size ret )
    putStrLn $ "    numdim:  " ++ show ( dim $ xLabeled' $ ret!0 )
    putStrLn ""

    return ret

    where
        go arr = Labeled' x y
            where
                y = Lexical $ arr!col
                x = unsafeToModule $ map read $ take (col) arrlist ++ drop (col+1) arrlist

                arrlist = toList arr

-------------------------------------------------------------------------------
-- data preprocessing
--
-- FIXME:
-- Find a better location for all this code.

-- | Uses an efficient 1-pass algorithm to calculate the mean variance.
-- This is much faster than the 2-pass algorithms on large datasets,
-- but has (slightly) worse numeric stability.
--
-- See http://www.cs.berkeley.edu/~mhoemmen/cs194/Tutorials/variance.pdf for details.
{-# INLINE meanAndVarianceInOnePass  #-}
meanAndVarianceInOnePass :: (Foldable xs, Field (Elem xs)) => xs -> (Elem xs, Elem xs)
meanAndVarianceInOnePass ys =
    {-# SCC meanAndVarianceInOnePass #-}
    case uncons ys of
        Nothing -> error "meanAndVarianceInOnePass on empty container"
        Just (x,xs) -> (\(k,m,v) -> (m,v/(k-1))) $ foldl' go (2,x,0) xs
        where
            go (k,mk,qk) x = (k+1,mk',qk')
                where
                    mk'=mk+(x-mk)/k
                    qk'=qk+(k-1)*(x-mk)*(x-mk)/k

-- | A wrapper around "meanAndVarianceInOnePass"
{-# INLINE varianceInOnePass  #-}
varianceInOnePass :: (Foldable xs, Field (Elem xs)) => xs -> Elem xs
varianceInOnePass = snd . meanAndVarianceInOnePass

-- | Calculate the variance of each column, then sort so that the highest variance is first.
-- This can be useful for preprocessing data.
--
-- NOTE:
-- The git history has a lot of versions of this function with different levels of efficiency.
-- I need to write a blog post about how all the subtle haskellisms effect the runtime.
{-# INLINABLE mkShuffleMap #-}
mkShuffleMap :: forall v.
    ( FiniteModule v
    , VectorSpace v
    , Unboxable v
    , Unboxable (Scalar v)
    , Eq v
    , Elem (SetElem v (Elem v)) ~ Elem v
    , Elem (SetElem v (Scalar (Elem v))) ~ Scalar (Elem v)
    , IxContainer (SetElem v (Elem v))
    ) => BArray v -> UArray Int
mkShuffleMap vs = {-# SCC mkShuffleMap #-} if size vs==0
    then error "mkShuffleMap: called on empty array"
    else runST ( do
        -- FIXME:
        -- @smalldata@ should be a random subsample of the data.
        -- The size should also depend on the dimension.
        let smalldata = P.take 1000 $ toList vs

--         let variances = fromList
--                 $ values
--                 $ varianceInOnePass
--                 $ VG.map Componentwise vs
--                 :: BArray (Scalar v)

        let variances
                = imap (\i _ -> varianceInOnePass $ (imap (\_ -> (!i)) smalldata))
                $ values
                $ vs!0
                :: [Scalar v]

        return
            $ fromList
            $ map fst
            $ L.sortBy (\(_,v1) (_,v2) -> compare v2 v1)
            $ imap (,)
            $ variances
        )

-- | apply the shufflemap to the data set to get a better ordering of the data
{-# INLINABLE apShuffleMap #-}
apShuffleMap :: forall v. FiniteModule v => UArray Int -> v -> v
apShuffleMap vmap v = unsafeToModule xs
    where
        xs :: [Scalar v]
        xs = generate1 (size vmap) $ \i -> v!(vmap!i)

{-# INLINABLE generate1 #-}
generate1 :: (Monoid v, Constructible v) => Int -> (Int -> Elem v) -> v
generate1 n f = if n <= 0
    then zero
    else fromList1N n (f 0) (map f [1..n-1])

{-
FIXME:
All this needs to be reimplemented using the subhask interface.
This requires fixing some of the features of subhask's linear algebra system.

-- | translate a dataset so the mean is zero
{-# INLINABLE meanCenter #-}
meanCenter ::
    ( VG.Vector v1 (v2 a)
    , VG.Vector v2 a
    , Real a
    ) => v1 (v2 a) -> v1 (v2 a)
meanCenter dps = {-# SCC meanCenter #-} VG.map (\v -> VG.zipWith (-) v meanV) dps
    where
        meanV = {-# SCC meanV #-} VG.map (/ fromIntegral (VG.length dps)) $ VG.foldl1' (VG.zipWith (+)) dps

-- | rotates the data using the PCA transform
{-# INLINABLE rotatePCA #-}
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

gramMatrix_ :: (Ring a, Storable a) => [VS.Vector a] -> LA.Matrix a
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
                -}
