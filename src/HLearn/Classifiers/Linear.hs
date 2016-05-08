module HLearn.Classifiers.Linear
    where

import SubHask
import SubHask.Category.Trans.Derivative
import SubHask.Compatibility.Containers

import HLearn.History
import HLearn.Optimization.Univariate
import HLearn.Optimization.Multivariate

import qualified Prelude as P
import qualified Data.List as L
import Debug.Trace

--------------------------------------------------------------------------------

-- | The data type to represent arbitrary <https://en.wikipedia.org/wiki/Linear_classifier linear classifiers>.
-- Important examples include least squares regression, logistic regression, and support vector machines.
-- In statistics, these models are called <https://en.wikipedia.org/wiki/Generalized_linear_model Generalized Linear Models>.
data GLM x y = GLM
    { weights :: Map' y x
    , numdp :: Scalar x
    }

deriving instance (Show x, Show y, Show (Scalar x)) => Show (GLM x y)

type instance Scalar (GLM x y) = Scalar x

--------------------------------------------------------------------------------

-- type Foldable' xs x = (Foldable xs, Elem xs~x, Scalar xs~Int)

type IsFoldable xs x = {-forall xs.-} (Foldable xs, Elem xs~x, Scalar xs~Int)

{-# INLINEABLE trainLogisticRegression #-}
trainLogisticRegression ::
    ( Ord y
    , Show y
    , Hilbert x
    , BoundedField (Scalar x)
    ) => Scalar x                                           -- ^ regularization parameter
      -> IsFoldable xys (Labeled' x y) => xys               -- ^ dataset
      -> ( cxt (LineBracket (Scalar x))
         , cxt (Iterator_cgd x)
         , cxt (Iterator_cgd (Scalar x))
         , cxt (Iterator_brent (Scalar x))
         , cxt (Backtracking x)
         , cxt (Map' y x)
         , cxt Int
         ) => History cxt (GLM x y)
trainLogisticRegression lambda xs = trainGLM_
    ( fminunc_cgd_
--         hestenesStiefel
--         polakRibiere
--         steepestDescent
        fletcherReeves
        (lineSearch_brent (stop_brent 1e-6 || maxIterations 50 || noProgress || fx1grows))
--         (backtracking (strongCurvature 1e-10))
--         (backtracking fx1grows)
        (mulTolerance 1e-9 || maxIterations 50 || noProgress {-- || fx1grows-})
    )
    loss_logistic
    lambda
    (toList xs)

{-# INLINEABLE trainGLM_ #-}
trainGLM_ :: forall xys x y cxt opt.
    ( Ord y
    , Show y
    , Hilbert x
    ) => Has_x1 opt x => (x -> C1 (x -> Scalar x) -> forall s. History_ cxt s (opt x))   -- ^ optimization method
      -> (y -> Labeled' x y -> C2 (x -> Scalar x))          -- ^ loss function
      -> Scalar x                                           -- ^ regularization parameter
      -> [Labeled' x y]                                     -- ^ dataset
      -> History cxt (GLM x y)
trainGLM_ optmethod loss lambda xs = do
--     let ys = fromList $ map yLabeled' $ toList xs :: Set y
--     ws <- fmap sum $ mapM go (toList ys)
--     return $ GLM
--         { weights = ws
--         , numdp = fromIntegral $ length xs
--         }
    let Just (y0,ys) = uncons (fromList $ map yLabeled' $ toList xs :: Set y)
    ws <- fmap sum $ mapM go (toList ys)
    return $ GLM
        { weights = insertAt y0 zero ws
        , numdp = fromIntegral $ length xs
        }
    where
        go :: Show y => y -> forall s. History_ cxt s (Map' y x)
        go y0 = beginFunction ("trainGLM("++show y0++")") $ do
            w <- fmap x1 $ optmethod zero (totalLoss y0)
            return $ singletonAt y0 w

        totalLoss :: y -> C1 (x -> Scalar x)
        totalLoss y0 = unsafeProveC1 f f'
            where
                g = foldMap (loss y0) xs

                f  w = (           g $ w) + lambda*size w
                f' w = (derivative g $ w) + lambda*.w


--------------------------------------------------------------------------------
-- loss functions

classify :: (Ord y, Hilbert x) => GLM x y -> x -> y
classify (GLM ws _) x
    = fst
    $ P.head
    $ L.sortBy (\(_,wx1) (_,wx2) -> compare wx2 wx1)
    $ toIxList
    $ imap (\_ w -> w<>x) ws

validate ::
    ( Ord y
    , Hilbert x
    , cat <: (->)
    ) => (y -> Labeled' x y -> (x `cat` Scalar x)) -> [Labeled' x y] -> GLM x y -> Scalar x
validate loss xs model@(GLM ws _) = (sum $ map go xs) -- /(fromIntegral $ length xs)
    where
        go xy@(Labeled' x y) = loss y' xy $ ws!y'
            where
                y' = classify model x

{-# INLINEABLE loss_01 #-}
loss_01 :: (HasScalar x, Eq y) => y -> Labeled' x y -> x -> Scalar x
loss_01 y0 (Labeled' x y) = indicator $ y0/=y

{-# INLINEABLE loss_squared #-}
loss_squared :: (Hilbert x, Eq y) => y -> Labeled' x y -> C2 (x -> Scalar x)
loss_squared y0 (Labeled' x y) = unsafeProveC2 f f' f''
    where
        labelscore = bool2num $ y0==y

        f   w = 0.5 * (w<>x-labelscore)**2
        f'  w = x.*(w<>x-labelscore)
        f'' w = x><x

{-# INLINEABLE loss_logistic #-}
loss_logistic :: (Hilbert x, Eq y) => y -> Labeled' x y -> C2 (x -> Scalar x)
loss_logistic y0 (Labeled' x y) = unsafeProveC2 f f' f''
    where
        labelscore = bool2num $ y0==y

        f   w = logSumOfExp2 zero $ -labelscore*w<>x
        f'  w = -labelscore*(1-invlogit (labelscore*w<>x)) *. x
        f'' w = x><x ./ (1+exp (-labelscore*w<>x))

{-# INLINEABLE loss_hinge #-}
loss_hinge :: (Hilbert x, Eq y) => y -> Labeled' x y -> C2 (x -> Scalar x)
loss_hinge y0 (Labeled' x y) = unsafeProveC2 f f' f''
    where
        labelscore = bool2num $ y0==y

        f   w = max 0 $ 1 -labelscore*w<>x
        f'  w = if x<>w > 1 then zero else -labelscore*.x
        f'' w = zero

----------------------------------------
-- helpers

bool2num True = 1
bool2num False = -1

invlogit x = 1 / (1 + exp (-x))

-- | calculates log . sum . map exp in a numerically stable way
logSumOfExp xs = m + log (sum [ exp $ x-m | x <- xs ] )
    where
        m = maximum xs

-- | calculates log $ exp x1 + exp x2 in a numerically stable way
logSumOfExp2 x1 x2 = big + log ( exp (small-big) + 1 )
    where
        big = max x1 x2
        small = min x1 x2

-- logSumOfExp2 x1 x2 = m + log ( exp (x1-m) + exp (x2-m) )
--     where
--         m = max x1 x2
