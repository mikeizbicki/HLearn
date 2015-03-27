{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
module HLearn.History.DisplayMethods
    where

-- import Control.Monad.Identity
import Control.Monad.Random
import Control.Monad.State.Strict (get,put,runStateT)
-- import Control.Monad.Trans
import Data.List hiding (concat,elem,maximum,minimum,length,(!!))
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import Numeric
import System.Console.ANSI
import System.CPUTime
import System.Directory
import System.Exit
import System.IO
import System.Process

import Control.Lens hiding (cons)
import Pipes hiding (Foldable (..))
import Pipes.Core
-- import Text.InterpolatedString.QQ

import qualified Prelude as P
import Prelude (take,drop,map,filter,zip)
import SubHask
import SubHask.Compatibility.Containers
import SubHask.Compatibility.HMatrix
import SubHask.Monad

import HLearn.History hiding (cons)
import HLearn.Optimization.Common
import HLearn.Optimization.LineMinimization.Univariate
import HLearn.Optimization.LineMinimization.Multivariate
import HLearn.Optimization.GradientDescent

-------------------------------------------------------------------------------
-- display functions

dispFloat :: P.RealFloat a => a -> String
dispFloat a = showEFloat (Just 12) a ""

showIndent :: Int -> String
showIndent i = concat $ replicate i " - "

showableReport :: Report DynamicHistory -> Bool
showableReport report = case fromDynamic (dyn report) :: Maybe StartCollection of
    Just _ -> False
    Nothing -> True

showCPUTime :: CPUTime -> Int -> String
showCPUTime time len = showEFloat (Just $ len-4-4) (fromIntegral time * 1e-12 :: Double) "" ++ " sec"

padString :: String -> Int -> String
padString a i = take i $ a ++ repeat ' '

pad :: Show a => a -> Int -> String
pad a i = take i $ show a ++ repeat ' '

-------------------

concatDF :: [DisplayFunction] -> DisplayFunction
concatDF fs x = concat $ intersperse "; " $ filter (/="") $ map ($ x) fs

displayCPUTime :: DisplayFunction
displayCPUTime x = showEFloat (Just 6) (fromIntegral (cpuTime x) * 1e-12 :: Double) "" ++ " sec"

displayCPUTimeDiff :: DisplayFunction
displayCPUTimeDiff x = showEFloat (Just 6) (fromIntegral (cpuTimeDiff x) * 1e-12 :: Double) "" ++ " sec"

displayNumReports :: DisplayFunction
displayNumReports = show . displayNumReports

displayReportLevel :: DisplayFunction
displayReportLevel = show . displayReportLevel

displayStartCollection :: DisplayFunction
displayStartCollection = mkDisplayFunction (show :: StartCollection -> String)

displayType :: DisplayFunction
displayType = head . words . show . dynTypeRep . dyn

displayIndent :: DisplayFunction
displayIndent x = concat $ replicate (reportLevel x) " - "

displayItr :: DisplayFunction
displayItr x = "itr="++show (numReports x)

-------------------

display_fx1 :: DisplayFunction
display_fx1 x = "fx1="++concatDF
--     [ display_fx1_NewtonRaphson
    [ display_fx1_CGD
    , display_fx1_CGDM
    , display_fx1_Brent
    , display_fx1_Backtracking
    , display_fx1_BacktrackingM
    ] x

mkDisplayFunction2 :: forall a. Typeable a => (a -> String) -> DisplayFunction
mkDisplayFunction2 f x = case fromDynamic $ dyn x :: Maybe a of
    Nothing -> ""
    Just y -> f y

display_fx1_CGD :: DisplayFunction
display_fx1_CGD = mkDisplayFunction (dispFloat.view fx1 :: ConjugateGradientDescent (VS.Vector Double) -> String)

display_fx1_CGDM :: DisplayFunction
display_fx1_CGDM = mkDisplayFunction (dispFloat.view fx1 :: (Typeable Matrix, P.RealFloat(Scalar (Matrix Double))) =>  ConjugateGradientDescent (Matrix Double) -> String)

-- display_fx1_NewtonRaphson :: DisplayFunction
-- display_fx1_NewtonRaphson = undefined -- mkDisplayFunction (show.view _fx1 :: NewtonRaphson (LA.Vector Double) -> String)

display_fx1_Brent :: DisplayFunction
display_fx1_Brent = mkDisplayFunction (dispFloat._fw :: Brent Double -> String)

display_fx1_Backtracking :: DisplayFunction
display_fx1_Backtracking = mkDisplayFunction (dispFloat._bt_fx :: Backtracking (VS.Vector Double) -> String)

-- deriving instance Typeable Matrix
-- type instance Scalar (Matrix r) = r

display_fx1_BacktrackingM :: DisplayFunction
display_fx1_BacktrackingM x = case fromDynamic $ dyn x :: Maybe (Backtracking (SubHask.Compatibility.HMatrix.Matrix Double)) of
    Nothing -> ""
    Just y -> dispFloat (_bt_fx y)

-------------------

display_step :: DisplayFunction
display_step x = "step="++concatDF
    [
    ] x

-- display_step_SGD :: DisplayFunction
-- display_step_SGD = mkDisplayFunction (dispfloat.view step :: SGD (V.Vector

-------------------------------------------------------------------------------

infixr 7 |||
(|||) :: DisplayFilter' s1 -> DisplayMethod' s2 -> DisplayMethod' (s1,s2)
(f1 ||| f2) rep = do
    (s1,s2) <- get
    (passFilter,s1') <- liftIO $ runStateT (f1 rep) s1
    ((),s2') <- if passFilter || globalPass
        then liftIO $ runStateT (f2 rep) s2
        else return ((),s2)
    put (s1',s2')
    where
        globalPass = dynTypeRep (dyn rep) == typeOf StartHistory
                  || dynTypeRep (dyn rep) == typeOf EndHistory

infixl 6 ===
(===) :: DisplayMethod' s1 -> DisplayMethod' s2 -> DisplayMethod' (s1,s2)
(f1 === f2) rep = do
    (s1,s2) <- get
    ((),s1') <- liftIO $ runStateT (f1 rep) s1
    ((),s2') <- liftIO $ runStateT (f2 rep) s2
    put (s1',s2')

-------------------

firstCall :: Monad Hask m => Report DynamicHistory -> m () -> m ()
firstCall rep f = forEach StartHistory rep $ \_ -> f

lastCall :: Monad Hask m => Report DynamicHistory -> m () -> m ()
lastCall rep f = forEach EndHistory rep $ \_ -> f

forEach :: forall a m. (Typeable a, Monad Hask m) => a -> Report DynamicHistory -> (a -> m ()) -> m ()
forEach a rep f = case fromDynamic $ dyn rep :: Maybe a of
    Nothing -> return ()
    Just x -> f x


-------------------------------------------------------------------------------

removeLineMin :: DisplayFilter
removeLineMin rep = case head $ words $ show $ dynTypeRep $ dyn rep of
    "Brent" -> return False
    "LineBracket" -> return False
    "Backtracking" -> return False
    otherwise -> return True

sampleSGD :: Int -> DisplayFilter
sampleSGD n rep = do
    if take 3 ( show $ dynTypeRep $ dyn rep) /= "SGD"
        then return True
        else if numReports rep `mod` n == 0
            then return True
            else return False

-------------------------------------------------------------------------------

data SummaryStatistics = SummaryStatistics
    { numOccurances :: Int
    , totalCPUTime  :: CPUTime
    }
    deriving (Show,Typeable)

type instance Logic SummaryStatistics = Bool

instance Eq_ SummaryStatistics where
    s1==s2 = numOccurances s1==numOccurances s2
          && totalCPUTime s1==totalCPUTime s2

instance Semigroup SummaryStatistics where
    ss1 + ss2 = SummaryStatistics
        { numOccurances = numOccurances ss1 + numOccurances ss2
        , totalCPUTime  = totalCPUTime ss1  + totalCPUTime ss2
        }

instance Monoid SummaryStatistics where
    zero = SummaryStatistics 0 0

-- instance (Ord k, Semigroup g) => Semigroup (Map.Map k g) where
--     m1 + m2 = Map.unionWith (+) m1 m2
--
-- instance (Ord k, Semigroup g) => Monoid (Map.Map k g) where
--     zero = Map.empty

summaryStatistics :: DisplayMethod' (Map' TypeRep SummaryStatistics)
summaryStatistics rep = do
    m <- get
    let nextType = dynTypeRep $ dyn rep
        newStats = case m!?nextType of
            Nothing -> SummaryStatistics
                { numOccurances = 1
                , totalCPUTime = cpuTimeDiff rep
                }
            Just oldStats -> oldStats
                { numOccurances = numOccurances oldStats+1
                , totalCPUTime = totalCPUTime oldStats + cpuTimeDiff rep
                }
    put $ insertAt nextType newStats m
--     put $ (nextType,newStats) `cons` m
    lastCall rep $ do
        liftIO $ putStrLn "======================================================="
        liftIO $ putStrLn "| type                 | numOccurances | totalCPUTime | "
        liftIO $ putStrLn "======================================================="
--         forM (Map.toList m) $ \(k,v) -> do
        forM (toIxList m) $ \(k,v) -> do
            liftIO $ putStr $ "| "
            liftIO $ putStr $ padString (head $ words $ show k) 20
            liftIO $ putStr $ " | "
            liftIO $ putStr $ pad (numOccurances v) 13
            liftIO $ putStr $ " | "
            liftIO $ putStr $ showCPUTime (totalCPUTime v) 12
            liftIO $ putStrLn " |"
        liftIO $ putStrLn "======================================================="

-------------------------------------------------------------------------------

defaultTrace :: DisplayFunction
defaultTrace = concatDF
    [ displayIndent
    , displayItr
    , displayType
    , display_fx1
    , display_step
    , displayCPUTime
    , displayCPUTimeDiff
    ]

linearTrace :: DisplayMethod
linearTrace = linearTrace' defaultTrace

linearTrace' :: DisplayFunction -> DisplayMethod' ()
linearTrace' f rep = do
    when (showableReport rep) $ liftIO $ putStrLn $ f rep

compactTrace :: DisplayMethod' Int
compactTrace = compactTrace' defaultTrace

compactTrace' :: DisplayFunction -> DisplayMethod' Int
compactTrace' f rep = do
    prevLevel <- get
    liftIO $ do
        when (prevLevel == reportLevel rep) $ do
            cursorUpLine 1
            clearFromCursorToScreenEnd
        when (prevLevel >  reportLevel rep) $ do
            cursorUpLine 1
            clearFromCursorToScreenEnd
        when (prevLevel <  reportLevel rep) $ do
--                     putStrLn ""
            clearFromCursorToScreenEnd
    put $ reportLevel rep

-------------------------------------------------------------------------------

data PassCount
    = InPass Int Int
    | NotInPass Int

instance Semigroup PassCount where
    (+) = undefined

instance Monoid PassCount where
    zero = NotInPass 0

allowPasses :: forall a. Typeable a => a -> [Int] -> DisplayFilter' PassCount
allowPasses a xs rep = case fromDynamic $ dyn rep :: Maybe a of
    Nothing -> do
        pc <- get
        case pc of
            InPass i lvl -> when (reportLevel rep < lvl) $ put $ NotInPass i
            otherwise -> return ()
        return False
    Just _ -> do
        pc <- get
        case pc of
            InPass i lvl -> return $ (reportLevel rep == lvl) && i `elem` xs
            NotInPass i -> do
                put $ InPass (i+1) $ reportLevel rep
                return $ False

allowFirstPass :: forall a. Typeable a => a -> DisplayFilter' PassCount
allowFirstPass a = allowPasses a [1]

-------------------

type OptimizationPlot opt = [V.Vector (Scalar opt)]


optDP :: forall opt v.
    ( Has_fx1 opt v
    , Has_x1 opt v
    , Typeable (opt v)
    , Foldable v
    , Elem v ~ Scalar v
    ) => opt v -> Report DynamicHistory -> V.Vector (Scalar v)
optDP _ rep = VG.fromList $ (opt^.fx1) : (toList $ opt ^. x1)
    where
        opt = case fromDynamic $ dyn rep :: Maybe (opt v) of
            Just x -> x
            Nothing -> error "optDP: dynamic and static type mismatch"

mkOptimizationPlot :: forall opt v v'.
    ( Has_fx1 opt v
    , Has_x1 opt v
    , Has_f opt v
    , Random (Scalar v)
    , Show (Scalar v)
    , Show v
    , Eq (opt (v' (Scalar v)))
    , Typeable (opt v)
    , v ~ v' (Scalar v)
    , VG.Vector v' (Scalar v)
    , Ord (Scalar v)
    , Hilbert v
    , Bounded (Scalar v)
    ) => opt v -> FilePath -> DisplayMethod' [opt v]
mkOptimizationPlot opt path rep = do
    xs <- get

--         basis <- V.replicateM 9 $ VG.generateM numdim (\i -> getRandomR (-1,1))
--         basis <- V.replicateM 1 $ VG.generateM numdim (\i -> return 1)

    let numdim = VG.length $ (head xs)^.x1
        basis = V.fromList $ -- take 6 $
            [ VG.generate numdim (\j -> if i==j then 2 else 0) :: v
            | i <- [0..numdim-1]
            ]

    let f x = (head xs)^.flens $ x

    mkOptimization2d' basis f opt path rep

optpath2file :: forall opt v.
    ( Hilbert v
    , Show (Scalar v)
    , Has_fx1 opt v
    , Has_x1 opt v
    , Typeable (opt v)
    ) => [v] -> opt v -> DisplayMethod' Handle
optpath2file basis _ rep = do
    firstCall rep $ do
        hout <- liftIO $ openFile "optpath.dat" WriteMode
        put hout

    forEach (undefined::opt v) rep $ \opt -> do
        hout <- get
        liftIO $ do
            let replace a b = map (\x -> if x==a then b else x)
            hPutStr   hout $ show $ opt^.fx1
            hPutStr   hout " "
            hPutStrLn hout $ replace ',' ' ' $ init $ tail $ show $ map (<>(opt^.x1)) basis

    lastCall rep $ do
        hout <- get
        liftIO $ hClose hout

mkOptimizationPlot' :: forall opt v v'.
    ( Has_fx1 opt v
    , Has_x1 opt v
--     , Has_f opt v
    , Random (Scalar v)
    , Show (Scalar v)
    , Show v
    , Eq (opt (v' (Scalar v)))
    , Typeable (opt v)
    , v ~ v' (Scalar v)
    , VG.Vector v' (Scalar v)
    , Ord (Scalar v)
    , Bounded (Scalar v)
    , Hilbert v
    ) => V.Vector v -> (v -> Scalar v) -> opt v -> FilePath -> DisplayMethod' [opt v]
mkOptimizationPlot' basis f opt path rep = do
    xs <- get
    forEach (undefined::opt v) rep $ \opt -> do
        put $ opt:xs

    lastCall rep $ liftIO $ do

        let minx = (head xs)^.x1
            numdim = VG.length minx
            miny = minimum $ map (^.fx1) xs
            maxy = maximum $ map (^.fx1) xs
            yrange = maxy-miny

        putStr "generating data for plots... "
        tmpdir <- mkTmpFileName path
        createDirectory tmpdir

        -- output optimization path to file
        hopt <- openFile (tmpdir++"/opt.dat") WriteMode

        forM_ (reverse xs) $ \x -> do
            hPutStr   hopt $ show $ x^.fx1
            hPutStr   hopt " "
            hPutStrLn hopt $ map (\x -> if x==',' then ' ' else x) $ init $ tail $ show $ VG.toList $ VG.map (<>(x^.x1)) basis
        hClose hopt

        -- output 1d cross-sections
        forM_ [0..length xs-1] $ \itr -> do
            forM_ [0..VG.length basis-1] $ \i -> do
                let basisvec = basis VG.! i
--                     itrx = reverse xs ! itr
                    itrx = elemAt itr $ reverse xs
                    filename = tmpdir++"/crosssection1d-itr"++padInt itr++"-basis"++padInt i++".dat"
                hfunc <- openFile filename WriteMode
                let col = map (\x -> (x^.x1) <> basisvec) xs
                    biggest = if maximum col<0 then 0 else maximum col
                    smallest = if minimum col>0 then 0 else minimum col
                let start = smallest - (biggest-smallest)*0.1
                    end = biggest + (biggest-smallest)*0.1
                    numstep = 20::Integer
                    step = (end-start)/(fromInteger numstep)
                forM [0..numstep-1] $ \j -> do
                    let xpos = start+step*(fromInteger j)
                        ypos = f $ itrx^.x1
                                 - (itrx^.x1) <> basisvec *. basisvec
                                 + xpos*.basisvec
                    hPutStr   hfunc $ show xpos
                    hPutStr   hfunc " "
                    hPutStrLn hfunc $ show ypos
                hClose hfunc

        -- output gnuplot file
        hgnuplot <- openFile (tmpdir++"/mkplots.gnu") WriteMode
        hPutStrLn hgnuplot $
--             \ set terminal postscript 'Times-Roman' 14 \n\
--             \ set output 'plots.ps' \n\
            "#!/usr/bin/gnuplot \n\
            \ set terminal pngcairo enhanced color \n\
            \ unset xtics \n\
            \ unset ytics \n\
            \ set yrange ["++show (miny-0.1*yrange)++":"++show (maxy+0.1*yrange)++"]\n\
            \ unset key \n\
            \ set xzeroaxis lt 1 \n\
            \ set yzeroaxis lt 1 \n\
            \ set tmargin 0.25 \n\
            \ set lmargin 1 \n\
            \ set rmargin 1 \n\
            \ set bmargin 1 \n\
            \ set title offset 0,-1,0 \n\
            \ "
        forM [0..length xs-1] $ \itr -> do
            hPutStrLn hgnuplot $
                " ################################################# \n\
                \ \n\
                \ unset multiplot\n\
                \ set output 'plots"++padInt itr++".png'\n\
                \ set multiplot layout "++numImage2layout (VG.length basis)++
                " title 'iteration "++show itr++"'\n"

            forM [0..VG.length basis-1] $ \i -> do
                hPutStrLn hgnuplot $
--                     " set title 'basis "++show i++"' \n\
                    " plot 'crosssection1d-itr"++padInt itr++"-basis"++padInt i++".dat' using 1:2 lt 1 lw 4 lc rgb '#000000' with lines,\\\n\
                    \      'opt.dat' using "++show (i+2)++":1 lt 1 lw 1 lc rgb '#0000ff' with lines \n\
                    \ \n"
        hClose hgnuplot
        putStrLn "done."

        -- run the gnuplot commands
        putStr "generating plots..."
        exitcode <- system $ "cd "++tmpdir++" && gnuplot mkplots.gnu && convert $(for a in *.png; do printf -- \"-delay 20 %s \" $a; done; ) result.gif && konqueror result.gif &"
        case exitcode of
            ExitSuccess -> putStrLn "done."
            ExitFailure i -> putStrLn $ "ExitFailure"++show i

elemAt :: Int -> [a] -> a
elemAt 0 (x:xs) = x
elemAt i (x:xs) = elemAt (i-1) xs
elemAt _ []     = error "elemAt: empty list"

mkOptimization2d' :: forall opt v v'.
    ( Has_fx1 opt v
    , Has_x1 opt v
--     , Has_f opt v
    , Random (Scalar v)
    , Show (Scalar v)
    , Show v
    , Eq (opt (v' (Scalar v)))
    , Typeable (opt v)
    , v ~ v' (Scalar v)
    , VG.Vector v' (Scalar v)
    , Ord (Scalar v)
    , Bounded (Scalar v)
    , Hilbert v
    ) => V.Vector v -> (v -> Scalar v) -> opt v -> FilePath -> DisplayMethod' [opt v]
mkOptimization2d' basis f opt path rep = do
    xs <- get
    forEach (undefined::opt v) rep $ \opt -> do
        put $ opt:xs

    lastCall rep $ liftIO $ do

        let minx = (head xs)^.x1
            numdim = VG.length minx
            miny = minimum $ map (^.fx1) xs
            maxy = maximum $ map (^.fx1) xs
            yrange = maxy-miny

        putStr "generating data for plots... "
        tmpdir <- mkTmpFileName path
        createDirectory tmpdir

        -- output optimization path to file
        hopt <- openFile (tmpdir++"/opt.dat") WriteMode

        forM_ (reverse xs) $ \x -> do
            hPutStr   hopt $ show $ x^.fx1
            hPutStr   hopt " "
            hPutStrLn hopt $ map (\x -> if x==',' then ' ' else x) $ init $ tail $ show $ VG.toList $ VG.map (<>(x^.x1)) basis
        hClose hopt

        -- output 1d cross-sections
        forM_ [0..length xs-1] $ \itr -> do
            forM_ [0..VG.length basis-1] $ \i -> do
                let basisvec = basis VG.! i
                    itrx = elemAt itr $ reverse xs
                    filename = tmpdir++"/crosssection1d-itr"++padInt itr++"-basis"++padInt i++".dat"
                hfunc <- openFile filename WriteMode
                let col = map (\x -> (x^.x1) <> basisvec) xs
                    biggest = if maximum col<0 then 0 else maximum col
                    smallest = if minimum col>0 then 0 else minimum col
                let start = smallest - (biggest-smallest)*0.1
                    end = biggest + (biggest-smallest)*0.1
                    numstep = 50::Integer
                    step = (end-start)/(fromInteger numstep)
                forM [0..numstep-1] $ \j -> do
                    let xpos = start+step*(fromInteger j)
                        ypos = f $ itrx^.x1
                                 - (itrx^.x1) <> basisvec *. basisvec
                                 + xpos*.basisvec
                    hPutStr   hfunc $ show xpos
                    hPutStr   hfunc " "
                    hPutStrLn hfunc $ show ypos
                hClose hfunc

        -- output 2d cross-sections
        forM_ [0..length xs-1] $ \itr -> do
            let itrx = elemAt itr $ reverse xs
            forM_ [0..VG.length basis-1] $ \b1 -> do
                forM_ [b1+1..VG.length basis-1] $ \b2 -> do
                    let filename = tmpdir++"/crosssection2d-itr"++padInt itr++"-basis"
                                         ++padInt b1++"x"
                                         ++padInt b2++".dat"
                    hfunc <- openFile filename WriteMode

                    let numstep = 20::Integer
                        margin = 0.5
                        basisvec1 = basis VG.! b1
                        basisvec2 = basis VG.! b2

                    let col1 = map (\x -> (x^.x1) <> basisvec1) xs
                        biggest1 = if maximum col1<0 then 0 else maximum col1
                        smallest1 = if minimum col1>0 then 0 else minimum col1
                        start1 = smallest1 - (biggest1-smallest1)*margin
                        end1 = biggest1 + (biggest1-smallest1)*margin
                        step1 = (end1-start1)/(fromInteger numstep)

                    let col2 = map (\x -> (x^.x1) <> basisvec2) xs
                        biggest2 = if maximum col2<0 then 0 else maximum col2
                        smallest2 = if minimum col2>0 then 0 else minimum col2
                        start2 = smallest2 - (biggest2-smallest2)*margin
                        end2 = biggest2 + (biggest2-smallest2)*margin
                        step2 = (end2-start2)/(fromInteger numstep)

                    forM [0..numstep-1] $ \x -> do

                        forM [0..numstep-1] $ \y -> do
                            let xpos = start1+step1*(fromInteger x)
                                ypos = start2+step2*(fromInteger y)
                                zpos = f $ itrx^.x1
                                         - (itrx^.x1) <> basisvec1 *. basisvec1
                                         - (itrx^.x1) <> basisvec2 *. basisvec2
                                         + xpos*.basisvec1
                                         + ypos*.basisvec2
                            hPutStr   hfunc $ show xpos
                            hPutStr   hfunc " "
                            hPutStr   hfunc $ show ypos
                            hPutStr   hfunc " "
                            hPutStrLn hfunc $ show zpos

                        hPutStrLn hfunc ""

                    hClose hfunc

        -- output gnuplot file
        hgnuplot <- openFile (tmpdir++"/mkplots.gnu") WriteMode

        -- gnuplot header
        hPutStrLn hgnuplot $
--             \ set terminal postscript 'Times-Roman' 14 \n\
--             \ set output 'plots.ps' \n\
            "#!/usr/bin/gnuplot \n\
            \ set terminal pngcairo enhanced color \n\
            \ unset xtics \n\
            \ unset ytics \n\
            \ set yrange ["++show (miny-0.1*yrange)++":"++show (maxy+0.1*yrange)++"]\n\
            \ unset key \n\
            \ set xzeroaxis lt 1 \n\
            \ set yzeroaxis lt 1 \n\
            \ set tmargin 0.25 \n\
            \ set lmargin 1 \n\
            \ set rmargin 1 \n\
            \ set bmargin 1 \n\
            \ set title offset 0,-1,0 \n\
            \ set auto \n\
            \ set style data lines \n\
            \ unset colorbox\n\
            \ set contour base\n\
            \ set size 0.5,0.5\n\
            \ set view map\n\
            \ set pm3d\n\
            \ set palette gray\n\
            \ unset clabel\n\
            \ unset xlabel\n\
            \ unset ylabel\n\
            \ unset zlabel\n\
            \ unset ztics\n\
            \ set terminal postscript eps enhanced size 4,3\n"
        forM [0..length xs-1] $ \itr -> do
            hPutStrLn hgnuplot $
                " ################################################# \n\
                \ \n\
                \ "
--                 \ unset multiplot\n\
--                 \ set output 'plots"++padInt itr++".png'\n\
--                 \ set multiplot layout "++show (VG.length basis)++","++show (VG.length basis)++
--                 " title 'iteration "++show itr++"'\n"

            forM [0..VG.length basis-1] $ \i -> do

--                 -- plot empty panels
--                 forM [0..i-1] $ \j -> do
--                     hPutStrLn hgnuplot $
--                         "unset border; unset xzeroaxis; unset yzeroaxis; plot sin(x) lc rgb '#ffffff'"

                -- plot 1d cross-section
                hPutStrLn hgnuplot $
                    " set output 'crosssection1d-itr"++padInt itr++"-basis"++padInt i++".eps'\n"++
                    " set border 15\n\
                    \ set xzeroaxis lt 1 lc '#777777'\n\
                    \ set yzeroaxis lt 1 lc '#777777'\n\
                    \ set yrange ["++show (miny-0.1*yrange)++":"++show (maxy+0.1*yrange)++"]\n\
                    \ plot 'crosssection1d-itr"++padInt itr++"-basis"++padInt i++".dat' using 1:2 lt 1 lw 4 lc rgb '#000000' with lines,\\\n\
                    \      'opt.dat' using "++show (i+2)++":1 lt 1 lw 1 lc rgb '#ff0000' with lines,\\\n\
                    \      'opt.dat' using "++show (i+2)++":1 every ::"++show itr++"::"++show itr++" lt 7 lc rgb '#ff0000' with points\n\
                    \ \n"

                -- plot 2d cross-sections
                forM [i+1..VG.length basis-1] $ \j -> hPutStrLn hgnuplot $
                    " set auto; \n\
                    \ set output 'crosssection2d-itr"++padInt itr++"-basis"++padInt i++"x"++padInt j++".eps\n\
                    \ splot 'crosssection2d-itr"++padInt itr++"-basis"++padInt i++"x"++padInt j++".dat' using 2:1:3 lc rgb \"#007700\" with pm3d,\\\n\
                    \       'opt.dat' using "++show (j+2)++":"++show (i+2)++":1 lt 1 lw 1 lc rgb '#ff0000' w lines,\\\n\
                    \       'opt.dat' using "++show (j+2)++":"++show (i+2)++":1 every ::"++show itr++"::"++show itr++" lt 7 lc rgb '#ff0000' with points\n\
                    \ \n"

                hPutStrLn hgnuplot "\n"

        hClose hgnuplot
        putStrLn "done."

        -- create shell script
        hshell <- openFile (tmpdir++"/mkplots.sh") WriteMode
        hClose hshell

        -- run the gnuplot commands
        putStr "generating plots..."
--         exitcode <- system $ "cd "++tmpdir++" && gnuplot mkplots.gnu && convert $(for a in *.png; do printf -- \"-delay 20 %s \" $a; done; ) result.gif && konqueror result.gif &"
--         exitcode <- system $
--             "cd "++tmpdir++" && gnuplot mkplots.gnu && for i in *.eps; do echo \"converting $i\"; epstool --copy --bbox \"$i\" \"$i.2\"; convert -density 300 \"$i.2\" -resize 400x300! \"$i.png\"; done && montage "++files++" -geometry 120x80 -border 1 -bordercolor white
--
--         case exitcode of
--             ExitSuccess -> putStrLn "done."
--             ExitFailure i -> putStrLn $ "ExitFailure"++show i

padInt :: (Show a, Integral a) => a -> String
padInt a = replicate (4-length str) '0' ++ str
    where
        str = show a


numImage2layout :: Int -> String
numImage2layout i = show rows++","++show cols
    where
        rows = i `div` cols + if i`mod`cols == 0 then 0 else 1
        cols = P.ceiling $ sqrt (fromIntegral i :: Double) :: Int

-- mkPlotBasis :: Double -> Double -> opt -> v -> FilePath -> IO ()
-- mkPlotBasis biggest smallest opt basisvec filepath = do
--     hfunc <- openFile filepath WriteMode
--     let start = smallest - (biggest-smallest)*0.2
--         end = biggest + (biggest-smallest)*0.2
--         numstep = 100::Int
--         step = (end-start)/(fromIntegral numstep)
--     forM [0..numstep-1] $ \j -> do
--         let xpos = start+step*(fromIntegral j)
--         hPutStr   hfunc $ show xpos
--         hPutStr   hfunc " "
--                 hPutStrLn hfunc $ show $ f $ (minx + ((negate $ minx <> basisvec) *. basisvec)) + (xpos *. basisvec)
--             hClose hfunc

-- mkOptimizationPlot _ path rep = case fromDynamic $ dyn rep :: Maybe (opt v) of
--     Nothing ->  return ()
--     Just opt -> liftIO $ do
--         hout <- openFile path AppendMode
--         hPutStrLn hout (optDP opt rep)
--         firstCall rep $ do
--             hPutStrLn hout "first"
--         lastCall rep $ do
--             hPutStrLn hout "last"
--         hClose hout

mkTmpFileName :: FilePath -> IO FilePath
mkTmpFileName path = go 0
    where
        go :: Int -> IO FilePath
        go i = do
            let numstr = show i
                path' = path ++ "." ++ (replicate (4-length numstr) '0') ++ numstr
            b1 <- doesFileExist path'
            b2 <- doesDirectoryExist path'
            if b1 || b2
                then go $ i+1
                else return path'



-------------------------------------------------------------------------------

-- historyToFile :: FilePath -> DisplayMethod
-- historyToFile filename = do
--     hout <- liftIO $ openFile filename ReadWriteMode
--
--     let go = do
--             next <- await
--             case next of
--                 Nothing -> return ()
--                 Just x -> do
--                     liftIO $ hPrint hout next
--                     go
--     go
--
--     liftIO $ hClose hout

-------------------------------------------------------------------------------


