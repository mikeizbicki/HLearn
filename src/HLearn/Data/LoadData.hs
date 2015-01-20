module HLearn.Data.LoadData
    where

import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.List hiding (insert,length,concat,partition,head)
import Data.Maybe
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

-- import Test.QuickCheck hiding (verbose,sample,label)
-- import Control.Parallel.Strategies

import SubHask hiding (Functor(..), Applicative(..), Monad(..), Then(..), fail, return)
import SubHask.Algebra.Container
import SubHask.Compatibility.ByteString
import SubHask.Compatibility.Cassava
import SubHask.Compatibility.Containers
import SubHask.Compatibility.Vector.Lebesgue
import SubHask.TemplateHaskell.Deriving

import HLearn.Data.UnsafeVector
import HLearn.History.Timing
import HLearn.Models.Classifiers.Common
import HLearn.Metrics.EMD

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
        let [dp,dim,val] :: [Int] = map read $ words line
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
    return $ fromList $ map fromList $ lines contents


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

-- | A generic method for loading unlabeled data points
-- where each file in a directory hierarchy corresponds to a single data point.
loadDirectory ::
    ( Eq a
    , NFData a
    ) => Maybe Int            -- ^ maximum number of datapoints to load; Nothing for unlimitted
      -> FilePath             -- ^ directory to load data from
      -> (FilePath -> IO a)   -- ^ function to load an individual file
      -> (FilePath -> Bool)   -- ^ function to filter out invalid filenames
      -> (a -> Bool)          -- ^ function to filter out malformed results
      -> IO (Array a)         -- ^
loadDirectory numdp dirpath loadFile validFilepath validResult = {-# SCC loadDirectory #-} do

    let takedp = case numdp of
            Nothing -> id
            Just n -> fmap (take n)

    files <-  timeIO "getDirectoryContentsRecursive"
        $ takedp
        $ fmap (filter validFilepath)
        $ getDirectoryContentsRecursive dirpath

    results <- timeIO "loadDirectory"
        $ fmap (filter validResult)
        $ mapM loadFile files

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
--     , Normed a
    , Show (Scalar a)
    ) => FilePath -> IO (Array a)
loadCSV filepath = do

    bs <- timeIO ("loading ["++filepath++"]") $ readFileByteString filepath

    let rse = decode NoHeader bs
    time "parsing csv file" rse

    rs <- case rse of
        Right rs -> return rs
        Left str -> error $ "failed to parse CSV file " ++ filepath ++ ": " ++ take 1000 str

    putStrLn "  dataset info:"
    putStrLn $ "    num dp:  " ++ show (size rs)
--     putStrLn $ "    size dp: " ++ show (size $ head rs)
    putStrLn ""

    return rs

{-# INLINABLE loaddata #-}
loaddata ::
    ( VG.Vector v f
    , NFData (v f)
    , FromRecord (v f)
    , Eq (v f)
    , Ord f
--     , Normed (v f)
    , Show (Scalar (v f))
    , Floating f
    , VUM.Unbox f
    ) => DataParams -> IO (Array (v f))
loaddata params = do
    (ArrayT rs) <- loadCSV $ datafile params

    setptsize $ VG.length $ VG.head rs

    rs' <- if pca params
        then error "pca disabled"
--         then time "calculating PCA" $ VG.convert $ rotatePCA rs
        else return rs

    rs'' <- if varshift params
        then time "varshifting data" $
            VG.convert $ VG.map (shuffleVec $ VU.map fst $ mkShuffleMap rs') rs'
        else return rs'

    return $ ArrayT rs''

{-
{-# INLINABLE loadLabeledNumericData #-}
loadLabeledNumericData :: forall v f.
    ( VG.Vector v f
    , NFData (v f)
    , FromRecord (V.Vector f)
    , Read f
    , f ~ Double
    ) => DataParams -> IO (V.Vector (MaybeLabeled String (v f)))
loadLabeledNumericData params = do
    let filename = datafile params
        colindex = fromJust $ labelcol params

    xse :: Either String (V.Vector (V.Vector String))
        <- timeIO ("loading ["++datafile params++"] ")
         $ fmap (decode HasHeader)
         $ BS.readFile $ datafile params
    xs <- case xse of
        Right rs -> return rs
        Left str -> error $ "failed to parse CSV file " ++ datafile params ++ ": " ++ take 1000 str

    let numdp = VG.length xs
    let numdim = VG.length $ xs VG.! 0
    let numlabels = Set.size $ Set.fromList $ VG.toList $ VG.map (VG.! colindex) xs

    hPutStrLn stderr "  dataset info:"
    hPutStrLn stderr $ "    num dp:     " ++ show numdp
    hPutStrLn stderr $ "    num dim:    " ++ show numdim
    hPutStrLn stderr $ "    num labels: " ++ show numlabels

    let ys = VG.map (\x -> MaybeLabeled
            { label = Just $ x VG.! colindex
            , attr = VG.convert (
                VG.map read $ VG.fromList $ (:) "1" $ VG.toList $ VG.take (colindex) x VG.++ VG.drop (colindex+1) x
--                 VG.map read $ VG.fromList $ VG.toList $ VG.take label_index x <> VG.drop (label_index+1) x
                :: V.Vector f
                )
            })
            xs
--             :: V.Vector (MaybeLabeled String (LA.Vector Double))

    let rs=VG.map attr ys

    rs' <- if pca params
        then time "calculating PCA" $ VG.convert $ rotatePCADouble rs
        else return rs

    rs'' <- if varshift params
        then time "varshifting data" $ error "FIXME: varshift not implemented"
--             VG.convert $ VG.map (shuffleVec $ VU.map fst $ mkShuffleMap rs') rs'
        else return rs'

    let ys' = VG.zipWith (\y r -> MaybeLabeled (label y) r) ys rs''

    deepseq ys' $ return ys'
    -}

-------------------------------------------------------------------------------
-- data preprocessing

{-# INLINABLE mkShuffleMap #-}
-- | calculate the variance of each column, then sort so that the highest variance is first
mkShuffleMap :: forall v a s.
    ( VG.Vector v a
    , Floating a
    , Ord a
    , VU.Unbox a
    ) => V.Vector (v a) -> VU.Vector (Int,a)
mkShuffleMap v = runST ( do
    let numdim = VG.length (v V.! 0)
        numdpf = fromIntegral $ VG.length v

    let m1V = VG.foldl1 (VG.zipWith (+)) v
        m2V = VG.foldl1 (VG.zipWith (+)) $ VG.map (VG.map (\x -> x*x)) v
        varV = VG.zipWith (\m1 m2 -> m2/numdpf-m1/numdpf*m1/numdpf) m1V m2V

        sortV = VG.zip (VG.fromList [0..numdim-1::Int]) $ VG.convert varV :: VU.Vector (Int, a)

    msortV <- VG.unsafeThaw sortV
    Intro.sortBy (\(_,v2) (_,v1) -> compare v1 v2) msortV
    sortV' <- VG.unsafeFreeze msortV

    return sortV'
    )
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

        gramMatrix = {-# SCC gramMatrix #-} gramMatrix_ $ map VG.convert $ VG.toList dps
--         gramMatrix = {-# SCC gramMatrix #-} LA.trans tmpm LA.<> tmpm
--             where
--                 tmpm = LA.fromLists (VG.toList $ VG.map VG.toList dps)

--         gramMatrix = {-# SCC gramMatrix #-} foldl1' (+)
--             [ let dp' = VG.convert dp in LA.asColumn dp' LA.<> LA.asRow dp' | dp <- VG.toList dps ]

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

