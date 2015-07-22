-- | This module provides numerical routines for minimizing/maximizing one dimensional functions.
-- These methods are guaranteed to converge to the correct solution if your function has exactly one minimum.
--
-- The module is divided into simple and advanced interface.
-- The simple interface is reminiscent of most optimization packages.
-- The advanced interface uses the "History" monad to provide complex debugging information.
module HLearn.Optimization.Univariate
    (
    -- * Simple interface
    -- | These simple functions provide an interface similar to Matlab/Ocatave's optimization toolkit.
    -- (See for example the [fminunc documentation](http://www.mathworks.com/help/optim/ug/fminunc.html).)
    -- They are recommended for quick prototypes and working from within GHCI.
    -- Here's an example:
    --
    -- >>> let f x = (x-5)**2 :: Double
    -- >>> fminunc f
    -- 5.000000000000001
    --
    -- In many applications, we want to nest these optimization calls.
    -- Debugging these nested optimiztions is notoriously difficult.
    -- Consider the following example:
    --
    -- >>> let g x y = x*x + (y-5)**2
    -- >>> fminunc (\x -> fminunc (\y -> g y x))
    -- -2.821859578658409
    --
    -- It's clear that the value of @x@ that minimizes @g x y@ should be zero;
    -- but we get a ridiculous answer instead.
    -- The advanced interface gives us much more debugging information and control over the optimization process.
    -- We can use it to solve problems that can't be solved with the simple interface.
    fminunc
    , fmaxunc

    -- * Advanced interface
    -- | The advanced interface is more complex, but has two advantages.
    -- First, it lets us specify a detailed level of debugging information using the "History" monad.
    -- Second, it lets us specify in more detail the algorithms used in the optimization.
    --
    -- Let's repeat the first example from the simple interface.
    -- This time we will use Brent's method on "Rational" numbers and ask for higher precision than we could get with floating point numbers.
    --
    -- >>> let f x = (x-5)*(x-5) :: Rational
    -- >>> :{
    -- >>> traceHistory $ do
    -- >>>     lb <- lineBracket f 0 1
    -- >>>     fminuncM_bracketed_brent_ (return . f) lb (maxIterations 10 || stop_brent 1e-10)
    -- >>> }:
    -- Iterator_brent; x=5%1; f(x)=0%1
    --
    -- In this case, we get an exact answer.
    -- The "traceHistory" command also gives us a record of how our optimization proceeded.
    --
    -- FIXME: Add an example of nested optimization

    , UnivariateMinimizer
    , UnivariateMinimizerLB

    -- ** Brent's Method
    -- | Brent's method uses parabolic interpolation to find the minimum.
    -- This is a good black box method.
    --
    -- See <http://en.wikipedia.org/wiki/Iterator_brent%27s_method wikipedia>
    -- and Section 10.2 of "Numerical Recipes in C" for more details.
    , fminuncM_brent
    , fminuncM_bracketed_brent
    , stop_brent
    , Iterator_brent

    -- ** Brent's method with derivatives
    -- | We can make Brent's method a bit faster sometimes by using the function's derivative.
    -- This method can be slower than Brent's method for some functions.
    --
    -- See section 10.3 of "Numerical Recipes in C" for more details.
    , fminuncM_bracketed_dbrent
    , stop_dbrent
    , Iterator_dbrent

    -- ** Golden section search
    -- | Golden section search is used to find the minimum of "poorly behaved" functions.
    -- Other methods almost always converge faster.
    --
    -- See <http://en.wikipedia.org/wiki/Golden_section_search wikipedia>
    -- and Section 10.1 of "Numerical Recipes in C" for more details.
    , fminuncM_gss
    , fminuncM_bracketed_gss
    , stop_gss
    , Iterator_gss

    -- ** Line bracketing
    -- | The univariate optimization methods above require an interval within which a minimum is guaranteed to exist.
    --
    -- See Section 10.1 of "Numerical Recipes in C" for more details.
    --
    -- FIXME:
    -- Should we switch "LineBracket" to the "Interval" type provided by subhask?
    -- This gives us a more consistent interface at the expensive of (sometimes) an extra function call.
    , lineBracket
    , lineBracketM
    , LineBracket

    -- * Test Functions
    , slopes
    , slopesWithDiscontinuity

    -- ** Lipshitz Univariate
    -- | These problems come from a paper by Famularo, Sergeyev, and Pugliese (hence fsp).
    -- See <wwwinfo.deis.unical.it/~yaro/tests.pdf test problems for lipshitz univariate global optimization> for more details.
    , fsp_problem1
    )
    where

import SubHask
import SubHask.Category.Trans.Derivative

import HLearn.History

-------------------------------------------------------------------------------
-- simple interface

-- | Finds the minimum of a function
{-# INLINABLE fminunc #-}
fminunc ::
    ( OrdField a
    ) => a              -- ^ initial guess of the input with maximum value
      -> (a -> a)       -- ^ function to minimize
      -> a              -- ^ input with (approximately) the minimum value
fminunc x0 f = x1 $ evalHistory $ fminuncM_brent (maxIterations 50 || noProgress) x0 (return . f)

-- | Finds the maximum of a function
{-# INLINABLE fmaxunc #-}
fmaxunc ::
    ( OrdField a
    ) => a              -- ^ initial guess of the input with maximum value
      -> (a -> a)       -- ^ function to maximize
      -> a              -- ^ input with (approximately) the maximum value
fmaxunc x0 f = fminunc x0 (-f)

-------------------------------------------------------------------------------
-- helpers

-- | The type of univariate minimization functions that require a starting point
type UnivariateMinimizer cxt opt a
    = StopCondition (opt a)     -- ^ controls when to stop
   -> a                         -- ^ initial guess of the input with minimum value
   -> (a -> History cxt a)      -- ^ monadic function we're minimizing
   -> History cxt (opt a)       -- ^ result of the optimization

-- | The type of univariate minimization functions that require a "LineBracket" bounding the solution
type UnivariateMinimizerLB cxt opt a
    = StopCondition (opt a)     -- ^ controls when to stop
   -> LineBracket a             -- ^ search for the solution within this range
   -> (a -> History cxt a)      -- ^ monadic function we're minimizing
   -> History cxt (opt a)       -- ^ result of the optimization

-- | Converts an optimization function that expects a "LineBracket" input into one that only requires an initial starting point.
unbracket ::
    ( cxt (opt a)
    , cxt (LineBracket a)
    , cxt (Scalar a)
    , cxt a
    , OrdField a
    ) => UnivariateMinimizerLB cxt opt a
      -> UnivariateMinimizer cxt opt a
unbracket fminuncM_bracketed stop x0 f = do
    lb <- lineBracketM f (x0/2) (x0*2)
    fminuncM_bracketed stop lb f

-------------------------------------------------------------------------------
-- line bracketing

-- | Variable names correspond to the algorithm in "Numerical Recipes in C"
data LineBracket a = LineBracket
    { _ax :: !a
    , _bx :: !a
    , _cx :: !a
    , _fa :: !a
    , _fb :: !a
    , _fc :: !a
    }
    deriving (Typeable)

type instance Scalar (LineBracket a) = Scalar a

instance Show a => Show (LineBracket a) where
    show lb = "LineBracket; a="++show (_ax lb)++"; b="++show (_bx lb)++"; c="++show (_cx lb)

-- | finds two points ax and cx between which a minimum is guaranteed to exist
-- this is a transliteration of the 'lineBracket' function from the \"Numerical
-- Recipes\" series
{-# INLINEABLE lineBracket #-}
lineBracket ::
    ( OrdField a
    ) => (a -> a) -- ^ the function we're bracketing
      -> a        -- ^ an initial guess for the lower bound
      -> a        -- ^ an initial guess for the upper bound
      -> History cxt (LineBracket a)
lineBracket f = lineBracketM (return . f)

{-# INLINEABLE lineBracketM #-}
lineBracketM ::
    ( OrdField a
    ) => (a -> History cxt a)   -- ^ the function we're bracketing
      -> a                      -- ^ an initial guess for the lower bound
      -> a                      -- ^ an initial guess for the upper bound
      -> History cxt (LineBracket a)
lineBracketM !f !pt1 !pt2 = beginFunction "lineBracketM" $ do
    let gold = 1.618034

    fpt1 <- f pt1
    fpt2 <- f pt2
    let (ax,fax,bx,fbx) = if fpt1 > fpt2
            then (pt1,fpt1,pt2,fpt2)
            else (pt2,fpt2,pt1,fpt1)

    let cx = bx+gold*(bx-ax)
    fcx <- f cx

    let lb0 = LineBracket
            { _ax = ax
            , _bx = bx
            , _cx = cx
            , _fa = fax
            , _fb = fbx
            , _fc = fcx
            }

    iterate (step_LineBracket f) stop_LineBracket lb0

stop_LineBracket :: OrdField a => StopCondition (LineBracket a)
stop_LineBracket _ lb = if _fb lb /= _fb lb
    then error "NaN in linebracket"
    else return $ _fb lb <= _fc lb

step_LineBracket :: OrdField a
    => (a -> History cxt a)
    -> LineBracket a
    -> History cxt (LineBracket a)
step_LineBracket !f lb@(LineBracket ax bx cx fa fb fc) = do

    let sign a b = if b>0 then abs a else -(abs a)
        tiny = 1e-20
        glimit = 100
        gold = 1.618034

        r = (bx-ax)*(fb-fc)
        q = (bx-cx)*(fb-fa)
        u = bx-((bx-cx)*q-(bx-ax)*r)/2*(sign (max (abs $ q-r) tiny) (q-r))

        u' = cx+gold*(cx-bx)
        ulim = bx+glimit*(cx-bx)

    -- due to laziness, we will only evaluate the function if we absolutely have to
    fu    <- f u
    fu'   <- f u'
    fulim <- f ulim

    return $ if (bx-u)*(u-cx) > 0
        then if fu < fc -- Got a minimum between a and c
            then lb
                { _ax = bx
                , _bx = u
                , _fa = fb
                , _fb = fu
                }
            else if fu > fb -- Got a minimum between a and u
                then lb
                    { _cx = u
                    , _fc = fu
                    }
                else lb -- Parabolic fit was no use.  Use default magnification
                    { _ax = bx
                    , _bx = cx
                    , _cx = u'
                    , _fa = fb
                    , _fb = fc
                    , _fc = fu'
                    }
        else if (cx-u)*(u-ulim) > 0 -- parabolic fit is between c and its allowed limit
            then if fu < fc
                then lb
                    { _ax = cx
                    , _bx = u
                    , _cx = u'
                    , _fa = fc
                    , _fb = fu
                    , _fc = fu'
                    }
                else lb
                    { _ax = bx
                    , _bx = cx
                    , _cx = u
                    , _fa = fb
                    , _fb = fc
                    , _fc = fu
                    }
            else if (u-ulim)*(ulim-cx) >= 0
                then lb -- limit parabolic u to maximum allowed value
                    { _ax = bx
                    , _bx = cx
                    , _cx = ulim
                    , _fa = fb
                    , _fb = fc
                    , _fc = fulim
                    }
                else lb -- reject parabolic u, use default magnification
                    { _ax = bx
                    , _bx = cx
                    , _cx = u'
                    , _fa = fb
                    , _fb = fc
                    , _fc = fu'
                    }

-------------------------------------------------------------------------------
-- golden section search

-- | Variable names correspond to the algorithm in "Numerical Recipes in C"
data Iterator_gss a = Iterator_gss
    { _gss_fxb :: !a
    , _gss_fxc :: !a
    , _gss_xa  :: !a
    , _gss_xb  :: !a
    , _gss_xc  :: !a
    , _gss_xd  :: !a
    }
    deriving (Typeable)

type instance Scalar (Iterator_gss a) = Scalar a

instance (IsScalar a, POrd a) => Has_x1 Iterator_gss a where
    x1 gss = if (_gss_fxb < _gss_fxc) gss then _gss_fxb gss else _gss_fxc gss

instance (IsScalar a, POrd a) => Has_fx1 Iterator_gss a where
    fx1 = inf _gss_fxb _gss_fxc

instance (ClassicalLogic a, Show a, OrdField a) => Show (Iterator_gss a) where
    show gss = "Iterator_gss; "++"x="++show x++"; f(x)="++show fx
        where
            x  = gss_x  gss
            fx = gss_fx gss


gss_x gss = if _gss_fxb gss < _gss_fxc gss
    then _gss_xb gss
    else _gss_xc gss

gss_fx gss =  min (_gss_fxb gss) (_gss_fxc gss)

---------------------------------------

-- | Golden section search initialized by a starting point
{-# INLINEABLE fminuncM_gss #-}
fminuncM_gss ::
    ( cxt (LineBracket a)
    , cxt a
    , OrdField a
    ) => UnivariateMinimizer cxt Iterator_gss a
fminuncM_gss = unbracket fminuncM_bracketed_gss

-- | Golden section search that only searches within a specified "LineBracket"
{-# INLINEABLE fminuncM_bracketed_gss #-}
fminuncM_bracketed_gss ::
    ( OrdField a
    , cxt a
    ) => UnivariateMinimizerLB cxt Iterator_gss a
fminuncM_bracketed_gss stop (LineBracket ax bx cx fa fb fc) f = beginFunction "goldenSectionSearch_" $ do
    let r = 0.61803399
        c = 1-r

    let xb = if abs (cx-bx) > abs (bx-ax) then bx           else bx-c*(bx-ax)
        xc = if abs (cx-bx) > abs (bx-ax) then bx+c*(cx-bx) else bx

    fxb <- f xb
    fxc <- f xc

    let gss0 = Iterator_gss
            { _gss_fxb = fxb
            , _gss_fxc = fxc
            , _gss_xa = ax
            , _gss_xb = xb
            , _gss_xc = xc
            , _gss_xd = cx
            }

    iterate (step_gss f) stop gss0

    where
        step_gss :: OrdField a => (a -> History cxt a) -> Iterator_gss a -> History cxt (Iterator_gss a)
        step_gss f (Iterator_gss f1 f2 x0 x1 x2 x3) = if f2 < f1
            then do
                let x' = r*x2+c*x3
                fx' <- f x'
                return $ Iterator_gss f2 fx' x1 x2 x' x3
            else do
                let x' = r*x1+c*x0
                fx' <- f x'
                return $ Iterator_gss fx' f1 x0 x' x1 x2
            where
                r = 0.61803399
                c = 1-r

-- | Does not stop until the independent variable is accurate to within the tolerance passed in.
{-# INLINEABLE stop_gss #-}
stop_gss :: OrdField a => a -> StopCondition (Iterator_gss a)
stop_gss tol _ (Iterator_gss _ _ !x0 !x1 !x2 !x3 )
    = return $ abs (x3-x0) <= tol*(abs x1+abs x2)

-------------------------------------------------------------------------------
-- Brent's method

-- | Variable names correspond to the algorithm in "Numerical Recipes in C"
data Iterator_brent a = Iterator_brent
    { _brent_a :: !a
    , _brent_b :: !a
    , _brent_d :: !a
    , _brent_e :: !a
    , _brent_fv :: !a
    , _brent_fw :: !a
    , _brent_fx :: !a
    , _brent_v :: !a
    , _brent_w :: !a
    , _brent_x :: !a
    }
    deriving Typeable

type instance Scalar (Iterator_brent a) = Scalar a
type instance Logic (Iterator_brent a) = Bool

instance Eq a => Eq_ (Iterator_brent a) where
    b1==b2
        = (_brent_a  b1 == _brent_a  b2)
       && (_brent_b  b1 == _brent_b  b2)
       && (_brent_d  b1 == _brent_d  b2)
       && (_brent_e  b1 == _brent_e  b2)
       && (_brent_fv b1 == _brent_fv b2)
       && (_brent_fw b1 == _brent_fw b2)
       && (_brent_fx b1 == _brent_fx b2)
       && (_brent_v  b1 == _brent_v  b2)
       && (_brent_w  b1 == _brent_w  b2)
       && (_brent_x  b1 == _brent_x  b2)

instance (Field a, IsScalar a, Show a) => Show (Iterator_brent a) where
    show b = "Iterator_brent; x="++show (_brent_x b)++"; f(x)="++show (fx1 b)

instance IsScalar a => Has_x1 Iterator_brent a where
    x1 = _brent_x

instance IsScalar a => Has_fx1 Iterator_brent a where
    fx1 = _brent_fx

-- | Brent's method initialized by a starting point
{-# INLINEABLE fminuncM_brent #-}
fminuncM_brent ::
    ( cxt (LineBracket a)
    , cxt a
    , OrdField a
    ) => UnivariateMinimizer cxt Iterator_brent a
fminuncM_brent = unbracket fminuncM_bracketed_brent

-- | Brent's method that only searches within a specified "LineBracket"
{-# INLINEABLE fminuncM_bracketed_brent #-}
fminuncM_bracketed_brent ::
    ( OrdField a
    , cxt a
    ) => UnivariateMinimizerLB cxt Iterator_brent a
fminuncM_bracketed_brent stop (LineBracket ax bx cx fa fb fc) f = beginFunction "brent" $ do
    fbx <- f bx
    iterate (step_brent_noTolerance f) stop $ Iterator_brent
        { _brent_a = min ax cx
        , _brent_b = max ax cx
        , _brent_d = zero
        , _brent_e = zero
        , _brent_v = bx
        , _brent_w = bx
        , _brent_x = bx
        , _brent_fv = fbx
        , _brent_fw = fbx
        , _brent_fx = fbx
        }

-- | This version of brent's method is slightly simpler than the one presented in "Numerical Recipes".
-- In particular, the "Numerical Recipes" version has a minor optimization where it does not reevaluate a function if the input changes below some tolerance.
-- Removing this check actually *improves* numeric stability.
-- We can use types the are more accurate than "Double"s (e.g. "Rational"s) where this check doesn't make sense.
step_brent_noTolerance ::
    ( OrdField a
    ) => (a -> History cxt a) -> Iterator_brent a -> History cxt (Iterator_brent a)
step_brent_noTolerance f brent@(Iterator_brent a b d e fv fw fx v w x) = do
    let cgold = 0.3819660
        xm = 0.5*(a+b)

        r' = (x-w)*(fx-fv)
        q' = (x-v)*(fx-fw)
        p' = (x-v)*q'-(x-w)*r'
        q'' = 2*(q'-r')
        p'' = if q''>0 then -p' else p'
        q''' = abs q''
        etemp' = e

        (d',e') = if abs p'' >= abs (0.5*q'''*etemp') || p'' <= q'''*(a-x) || p'' >= q'''*(b-x)
            then let e'' = if x>=xm then a-x else b-x in (cgold*e'',e'')
            else let d''=p''/q'''; u''=x+d'' in (d'',d)

        u' = x+d'

    fu' <- f u'

    return $ if fu' <= fx
        then brent
            { _brent_e = e'
            , _brent_d = d'
            , _brent_a = if u' >= x then x else a
            , _brent_b = if u' >= x then b else x
            , _brent_v = w
            , _brent_w = x
            , _brent_x = u'
            , _brent_fv = fw
            , _brent_fw = fx
            , _brent_fx = fu'
            }
        else brent
            { _brent_e = e'
            , _brent_d = d'
            , _brent_a = if u' < x then u' else a
            , _brent_b = if u' < x then b  else u'
            , _brent_v  = if fu' <= fw || w==x then w   else if fu' <= fv || v==x || v==w then u'  else v
            , _brent_fv = if fu' <= fw || w==x then fw  else if fu' <= fv || v==x || v==w then fu' else fv
            , _brent_w  = if fu' <= fw || w==x then u'  else w
            , _brent_fw = if fu' <= fw || w==x then fu' else fw
            }

-- | This version of Brent's method is a direct translation from "Numerical Recipes".
-- It is very slightly faster than "step_brent_noTolerance" but has worse accuracy.
step_brent_orig ::
    ( OrdField a
    ) => (a -> History cxt a) -> Iterator_brent a -> History cxt (Iterator_brent a)
step_brent_orig f brent@(Iterator_brent a b d e fv fw fx v w x) = do
    let cgold = 0.3819660
        xm = 0.5*(a+b)

        sign a b = if b>0 then abs a else -(abs a)
        zeps = 1e-10
        tol = 1e-12
        tol1' = tol*(abs x)+zeps
        tol2' = 2*tol1'

        (d',e') = if abs e > tol1'
            then let
                r' = (x-w)*(fx-fv)
                q' = (x-v)*(fx-fw)
                p' = (x-v)*q'-(x-w)*r'
                q'' = 2*(q'-r')
                p'' = if q''>0 then -p' else p'
                q''' = abs q''
                etemp' = e
                in if abs p'' >= abs (0.5*q'''*etemp') || p'' <= q'''*(a-x) || p'' >= q'''*(b-x)
                    then let e'' = if x>=xm then a-x else b-x in (cgold*e'',e'')
                    else let d''=p''/q'''; u''=x+d'' in
                        if u''-a < tol2' || b-u'' < tol2'
                            then (sign tol1' (xm-x),d)
                            else (d'',d)
            else let e'' = if x>=xm then a-x else b-x in (cgold*e'',e'')

        u' = if abs d' >= tol1'
            then x+d'
            else x+sign tol1' d'

    fu' <- f u'

    return $ if fu' <= fx
        then brent
            { _brent_e = e'
            , _brent_d = d'
            , _brent_a = if u' >= x then x else a
            , _brent_b = if u' >= x then b else x
            , _brent_v = w
            , _brent_w = x
            , _brent_x = u'
            , _brent_fv = fw
            , _brent_fw = fx
            , _brent_fx = fu'
            }
        else brent
            { _brent_e = e'
            , _brent_d = d'
            , _brent_a = if u' < x then u' else a
            , _brent_b = if u' < x then b  else u'
            , _brent_v  = if fu' <= fw || w==x then w   else if fu' <= fv || v==x || v==w then u'  else v
            , _brent_fv = if fu' <= fw || w==x then fw  else if fu' <= fv || v==x || v==w then fu' else fv
            , _brent_w  = if fu' <= fw || w==x then u'  else w
            , _brent_fw = if fu' <= fw || w==x then fu' else fw
            }

-- | Does not stop until the independent variable is accurate to within the tolerance passed in.
{-# INLINEABLE stop_brent #-}
stop_brent :: OrdField a => a -> StopCondition (Iterator_brent a)
stop_brent tol _ opt = return $ abs (x-xm) < tol2'-0.5*(b-a)
    where
        (Iterator_brent a b d e fv fw fx v w x) = opt
        xm = 0.5*(a+b)
        tol1' = tol*(abs x)+zeps
        tol2' = 2*tol1'
        zeps = 0 -- 1e-10

-------------------------------------------------------------------------------
-- Brent's method with derivatives

-- | Variable names correspond to the algorithm in "Numerical Recipes in C"
data Iterator_dbrent a = Iterator_dbrent
    { _dbrent_a :: !a
    , _dbrent_b :: !a
    , _dbrent_d :: !a
    , _dbrent_e :: !a
    , _dbrent_fv :: !a
    , _dbrent_fw :: !a
    , _dbrent_fx :: !a
    , _dbrent_dv :: !a
    , _dbrent_dw :: !a
    , _dbrent_dx :: !a
    , _dbrent_v :: !a
    , _dbrent_w :: !a
    , _dbrent_x :: !a
    , _dbrent_break :: Bool
    }
    deriving Typeable

type instance Scalar (Iterator_dbrent a) = Scalar a

instance Show a => Show (Iterator_dbrent a) where
    show b = "Iterator_brent; x="++show (_dbrent_x b)++"; f(x)="++show (_dbrent_fx b)

-- | Brent's method with derivatives that only searches within a specified "LineBracket"
--
-- FIXME:
-- Update this function so it uses the derivative category.
-- This will require some careful thought about:
-- 1) how derivatives interact with monads,
-- 2) restructuring the "UnivariateMinimization" types to handle arbitrary categories.
--
{-# INLINEABLE fminuncM_bracketed_dbrent #-}
fminuncM_bracketed_dbrent ::
    ( OrdField a
    , cxt a
    ) => StopCondition (Iterator_dbrent a)      -- ^ controls when to stop
      -> LineBracket a                          -- ^ search for the solution within this range
      -> (a -> History cxt a)                   -- ^ monadic function we're minimizing
      -> (a -> History cxt a)                   -- ^ derivative of the function we're minimizing
      -> History cxt (Iterator_dbrent a)        -- ^ result of the optimization
fminuncM_bracketed_dbrent stop (LineBracket ax bx cx fa fb fc) f df = beginFunction "dbrent" $ do
    fbx <- f bx
    dfx <- df bx
    iterate (step_dbrent f df) stop $ Iterator_dbrent
        { _dbrent_a = min ax cx
        , _dbrent_b = max ax cx
        , _dbrent_d = zero
        , _dbrent_e = zero
        , _dbrent_v = bx
        , _dbrent_w = bx
        , _dbrent_x = bx
        , _dbrent_fv = fbx
        , _dbrent_fw = fbx
        , _dbrent_fx = fbx
        , _dbrent_dv = dfx
        , _dbrent_dw = dfx
        , _dbrent_dx = dfx
        , _dbrent_break = False
        }

    where
        step_dbrent f df dbrent@(Iterator_dbrent a b d e fv fw fx dv dw dx v w x _) = do
            let zeps = 1e-10
                sign a b = if b>0 then abs a else -(abs a)
                tol = 1e-6

                xm = 0.5*(a+b)
                tol1' = tol*(abs x)+zeps
                tol2' = 2*tol1'

                (d',e') = if abs e > tol1'
                    then let
                        d1 = if dw /= dx then (w-x)*dx/(dx-dw) else 2*(b-a)
                        d2 = if dv /= dx then (v-x)*dx/(dx-dv) else 2*(b-a)
                        u1 = x+d1
                        u2 = x+d2
                        ok1 = (a-u1)*(u1-b) > 0 && dx*d1 <= 0
                        ok2 = (a-u2)*(u2-b) > 0 && dx*d2 <= 0
                        in if ok1 || ok2
                            then let
                                d'' = if ok1 && ok2
                                    then if abs d1 < abs d2 then d1 else d2
                                    else if ok1 then d1 else d2
                                in if abs d'' <= abs (0.5 * e)
                                    then let u' = x + d''
                                        in if u'-a < tol2' || b-u' < tol2'
                                            then (sign tol1' xm-x, d)
                                            else (d'', d)
                                    else
                                        let e'' = if dx>=0 then a-x else b-x
                                        in (0.5*e'',e'')
                            else
                                let e'' = if dx>=0 then a-x else b-x
                                in (0.5*e'',e'')
                    else
                        let e'' = if dx>=0 then a-x else b-x
                        in (0.5*e'',e'')

                u' = if abs d' >= tol1' then x+d' else x+sign tol1' d'

            fu' <- f u'
            du' <- df u'

            return $ if abs d' < tol1' && fu' > fx
                then dbrent
                    { _dbrent_x = x
                    , _dbrent_fx = fx
                    , _dbrent_break = True
                    }
                else
                    if fu' <= fx
                        then dbrent
                            { _dbrent_e = e'
                            , _dbrent_d = d'
                            , _dbrent_a = if u' >= x then x else a
                            , _dbrent_b = if u' >= x then b else x
                            , _dbrent_v = w
                            , _dbrent_w = x
                            , _dbrent_x = u'
                            , _dbrent_fv = fw
                            , _dbrent_fw = fx
                            , _dbrent_fx = fu'
                            , _dbrent_dv = dw
                            , _dbrent_dw = dx
                            , _dbrent_dx = du'
                            }
                        else dbrent
                            { _dbrent_e = e'
                            , _dbrent_d = d'
                            , _dbrent_a = if u' < x then u' else a
                            , _dbrent_b = if u' < x then b  else u'
                            , _dbrent_v  = if fu' <= fw || w==x then w   else if fu' <= fv || v==x || v==w then u'  else v
                            , _dbrent_fv = if fu' <= fw || w==x then fw  else if fu' <= fv || v==x || v==w then fu' else fv
                            , _dbrent_dv = if fu' <= fw || w==x then dw  else if fu' <= fv || v==x || v==w then du' else dv
                            , _dbrent_w  = if fu' <= fw || w==x then u'  else w
                            , _dbrent_fw = if fu' <= fw || w==x then fu' else fw
                            , _dbrent_dw = if fu' <= fw || w==x then du' else dw
                            }

-- | Does not stop until the independent variable is accurate to within the tolerance passed in.
{-# INLINEABLE stop_dbrent #-}
stop_dbrent :: OrdField a => a -> StopCondition (Iterator_dbrent a)
stop_dbrent tol _ opt = return $ should_break || abs (x-xm) < tol2'-0.5*(b-a)
    where
        (Iterator_dbrent a b d e fv fw fx dv dw dx v w x should_break) = opt
        xm = 0.5*(a+b)
        tol1' = tol*(abs x)+zeps
        tol2' = 2*tol1'
        zeps = 1e-10

--------------------------------------------------------------------------------
-- test functions

-- | A simple piecewise linear function.
{-# INLINEABLE slopes #-}
slopes :: (ClassicalLogic r, OrdField r) => r -> r
slopes x
    | x <  0  = -2*x
    | x <  30 = -x
    | x >= 30 = x-60

-- | This problem is interesting because it has no minimum when the domain is the real numbers.
-- Notice the discontinuity at x==30.
{-# INLINEABLE slopesWithDiscontinuity #-}
slopesWithDiscontinuity :: (ClassicalLogic r, OrdField r) => r -> r
slopesWithDiscontinuity x
    | x <  0  = -2*x
    | x <  30 = -x
    | x >= 30 = x-60+10

{-# INLINEABLE fsp_problem1 #-}
fsp_problem1 :: Real r => r -> r
fsp_problem1 x = -13/6*x + sin (13/4*(2*x+5)) - 53/12


{-
tester :: History Double
tester = do
    lb <- lineBracketM f 0 1
    gss <- fminuncM_gss_ f lb (maxIterations 5)
    return $ gss_x gss
    where
        f x = do
            let f a = abs x + (a+5-x)*(a+5)

            lb <- lineBracket f 0 1
            b <- fminuncM_brent_
                (return . f)
                lb
                (maxIterations 10)
            return $ _brent_fx b

-------------------------------------------------------------------------------

-}
