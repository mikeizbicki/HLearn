-- | This module provides numerical routines for minimizing/maximizing one dimensional functions.
-- All function are translated from "Numerical Recipes in C".
-- The module is divided into simple and advanced sections.
module HLearn.Optimization.LineMinimization.Univariate
    (

    -- * Simple interface
    -- | These simple functions provide an interface similar to Matlab/Ocatave's optimization toolkit.
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
    , fminuncM
    , fmaxuncM

    -- * Advanced interface
    -- | The advanced interface is more complex, but has two advantages.
    -- First, it lets us specify a level of debugging information we want.
    -- Second, it lets us specify in more detail the algorithms used in the optimization.
    --
    -- Let's repeat the first example from the simple interface.
    -- This time we will use Brent's method on "Rational" numbers and ask for higher precision than we could get with floating point numbers.
    --
    -- >>> let f x = (x-5)*(x-5) :: Rational
    -- >>> :{
    -- >>> traceHistory $ do
    -- >>>     lb <- lineBracket f 0 1
    -- >>>     fminuncM_brent_ (return . f) lb (maxIterations 10 || stop_brent 1e-10)
    -- >>> }:
    -- Iterator_brent; x=5%1; f(x)=0%1
    --
    -- In this case, we get an exact answer.
    -- The "traceHistory" command also gives us a record of how our optimization proceded.

    -- ** Brent's Method
    -- | Brent's method uses parabolic interpolation to find the minimum.
    --
    -- See <http://en.wikipedia.org/wiki/Iterator_brent%27s_method wikipedia>
    -- and Section 10.2 of "Numerical Recipes in C" for more details.
    , fminunc_brent
    , fminuncM_brent
    , fminuncM_brent_
    , stop_brent
    , Iterator_brent (..)

    -- ** Brent's method with derivatives
    -- |
    --
    -- FIXME: approve implementation of Section 10.3 of "Numerical Recipes in C"
    , dbrent_test_
    , fminunc_dbrent
    , fminuncM_dbrent
    , fminuncM_dbrent_
    , stop_dbrent
    , Iterator_dbrent (..)

    -- ** Golden section search
    -- | Golden section search is used to find the minimum of "poorly behaved" functions.
    -- Other methods almost always converge faster.
    --
    -- See <http://en.wikipedia.org/wiki/Golden_section_search wikipedia>
    -- and Section 10.1 of "Numerical Recipes in C" for more details.
    , fminunc_gss
    , fminuncM_gss
    , fminuncM_gss_
    , stop_gss
    , Iterator_gss (..)

    -- ** Line bracketing
    -- | The univariate optimization methods above require an interval within which a minimum is guaranteed to exist.
    --
    -- FIXME: switch LineBracket to the Interval type provided by subhask
    --
    -- See Section 10.1 of "Numerical Recipes in C" for more details.
    , lineBracket
    , lineBracketM
    , LineBracket (..)

    )
    where

import SubHask
import HLearn.History

-------------------------------------------------------------------------------

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

-- | Finds the minimum of a function
fminunc :: (Optimizable a, OrdField a) => (a -> a) -> a
fminunc = fminunc_brent

-- | Finds the maximum of a function
fmaxunc :: (Optimizable a, OrdField a) => (a -> a) -> a
fmaxunc f = fminunc_brent (-f)

-- | Finds the minimum of a monadic function
fminuncM :: (Optimizable a, OrdField a) => (a -> History a) -> a
fminuncM = fminuncM_brent

-- | Finds the maximum of a monadic function
fmaxuncM :: (Optimizable a, OrdField a) => (a -> History a) -> a
fmaxuncM f = fminuncM_brent (-f)

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

instance Show a => Show (LineBracket a) where
    show lb = "LineBracket; a="++show (_ax lb)++"; b="++show (_bx lb)++"; c="++show (_cx lb)

-- | finds two points ax and cx between which a minimum is guaranteed to exist
-- this is a transliteration of the 'lineBracket' function from the \"Numerical
-- Recipes\" series
lineBracket ::
    ( OrdField a
    ) => (a -> a) -- ^ the function we're bracketing
      -> a        -- ^ an initial guess for the lower bound
      -> a        -- ^ an initial guess for the upper bound
      -> Optimizable a => History (LineBracket a)
lineBracket f = lineBracketM (return . f)

lineBracketM ::
    ( OrdField a
    ) => (a -> History a)       -- ^ the function we're bracketing
      -> a                      -- ^ an initial guess for the lower bound
      -> a                      -- ^ an initial guess for the upper bound
      -> Optimizable a => History (LineBracket a)
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

    iterate
        (step_LineBracket f)
        lb0
        stop_LineBracket

stop_LineBracket :: OrdField a => LineBracket a -> LineBracket a -> History Bool
stop_LineBracket _ lb = if _fb lb /= _fb lb
    then error "NaN in linebracket"
    else return $ _fb lb <= _fc lb

step_LineBracket :: OrdField a
    => (a -> History a)
    -> LineBracket a
    -> History (LineBracket a)
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

-- | A simple interface to golden section search
fminunc_gss :: ( Optimizable a , OrdField a ) => (a -> a) -> a
fminunc_gss f = fminuncM_gss (return . f)

-- | A simple monadic interface to golden section search
fminuncM_gss :: forall a. ( Optimizable a , OrdField a ) => (a -> History a) -> a
fminuncM_gss f = evalHistory $ do
    lb <- lineBracketM f 0 1
    gss <- fminuncM_gss_ f lb (maxIterations 200 || stop_gss (1e-12 :: a))
    return $ gss_x gss

-- | The most generic interface to golden section search
fminuncM_gss_ :: ( Optimizable a , OrdField a )
    => (a -> History a)                 -- ^ the function we're minimizing
    -> LineBracket a                    -- ^ bounds between which a minimum must exist
    -> StopCondition_ (Iterator_gss a)  -- ^ controls the number of iterations
    -> History (Iterator_gss a)
fminuncM_gss_ f (LineBracket ax bx cx fa fb fc) stop = beginFunction "goldenSectionSearch_" $ do
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

    iterate
        (step_gss f)
        gss0
        stop

    where
        step_gss :: OrdField a => (a -> History a) -> Iterator_gss a -> History (Iterator_gss a)
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
stop_gss :: OrdField a => a -> StopCondition_ (Iterator_gss a)
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

instance Show a => Show (Iterator_brent a) where
    show b = "Iterator_brent; x="++show (_brent_x b)++"; f(x)="++show (_brent_fx b)

-- | A simple interface to Brent's method
fminunc_brent :: ( Optimizable a, OrdField a ) => (a -> a) -> a
fminunc_brent f = fminuncM_brent (return . f)

-- | A simple monadic interface to Brent's method
fminuncM_brent :: ( Optimizable a, OrdField a ) => (a -> History a) -> a
fminuncM_brent f = evalHistory $ do
    lb <- lineBracketM f 0 1
    b <- fminuncM_brent_ f lb ( maxIterations 200 || stop_brent 1e-12 )
    return $ _brent_x b

-- | The most generic interface to Brent's method
fminuncM_brent_ ::
    ( Optimizable a , OrdField a
    ) => (a -> History a)                   -- ^ the function we're minimizing
      -> LineBracket a                      -- ^ bounds between which a minimum must exist
      -> StopCondition_ (Iterator_brent a)  -- ^ controls the number of iterations
      -> History (Iterator_brent a)
fminuncM_brent_ f (LineBracket ax bx cx fa fb fc) stop = beginFunction "brent" $ do
    fbx <- f bx
    iterate
        (step_brent f)
        ( Iterator_brent
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
        )
        stop

    where
        step_brent ::
            ( OrdField a
            ) => (a -> History a) -> Iterator_brent a -> History (Iterator_brent a)
        step_brent f brent@(Iterator_brent a b d e fv fw fx v w x) = do
            let cgold = 0.3819660
                zeps = 1e-10
                sign a b = if b>0 then abs a else -(abs a)
                tol = 1e-6

                xm = 0.5*(a+b)
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
--
-- FIXME: if we get an exact solution this doesn't stop the optimization
stop_brent :: (Show a, OrdField a) => a -> StopCondition_ (Iterator_brent a)
-- stop_brent tol _ opt = trace ("a="++show a++"; b="++show b++"; left="++show (abs (x-xm))++"; right="++show (tol2'-0.5*(b-a))) $
--     return $ abs (x-xm) <  tol2'-0.5*(b-a)
stop_brent tol _ opt = return $ abs (x-xm) <  tol2'-0.5*(b-a)
    where
        (Iterator_brent a b d e fv fw fx v w x) = opt
        xm = 0.5*(a+b)
        tol1' = tol*(abs x)+zeps
        tol2' = 2*tol1'
        zeps = 1e-10



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

instance Show a => Show (Iterator_dbrent a) where
    show b = "Iterator_brent; x="++show (_dbrent_x b)++"; f(x)="++show (_dbrent_fx b)

-- | A simple interface to Brent's method with derivatives
fminunc_dbrent :: ( Optimizable a, OrdField a ) => (a -> a) -> (a -> a) -> a
fminunc_dbrent f df = fminuncM_dbrent (return . f) (return . df)

-- | A simple monadic interface to Brent's method with derivatives
fminuncM_dbrent :: ( Optimizable a, OrdField a ) => (a -> History a) -> (a -> History a) -> a
fminuncM_dbrent f df = evalHistory $ do
    lb <- lineBracketM f 0 1
    b <- fminuncM_dbrent_ f df lb ( maxIterations 200 || stop_dbrent 1e-12 )
    return $ _dbrent_x b

-- | The most generic interface to Brent's method with derivatives
fminuncM_dbrent_ ::
    ( Optimizable a , OrdField a
    ) => (a -> History a)                    -- ^ the function we're minimizing
      -> (a -> History a)                    -- ^ the function's derivative
      -> LineBracket a                       -- ^ bounds between which a minimum must exist
      -> StopCondition_ (Iterator_dbrent a)  -- ^ controls the number of iterations
      -> History (Iterator_dbrent a)
fminuncM_dbrent_ f df (LineBracket ax bx cx fa fb fc) stop = beginFunction "dbrent" $ do
    fbx <- f bx
    dfx <- df bx
    iterate
        (step_dbrent f df)
        ( Iterator_dbrent
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
        )
        stop

    where
        step_dbrent ::
            ( OrdField a
            ) => (a -> History a) -> (a -> History a) -> Iterator_dbrent a -> History (Iterator_dbrent a)
        step_dbrent f df dbrent@(Iterator_dbrent a b d e fv fw fx dv dw dx v w x _) = do
            let zeps = 1e-10
                sign a b = if b>0 then abs a else -(abs a)
                tol = 1e-6

                xm = 0.5*(a+b)
                tol1' = tol*(abs x)+zeps
                tol2' = 2*tol1'

                (d',e') = if abs e > tol1' then 
                            let
                              d1 = if dw /= dx then (w-x)*dx/(dx-dw) else 2*(b-a) 
                              d2 = if dv /= dx then (v-x)*dx/(dx-dv) else 2*(b-a)
                              u1 = x+d1
                              u2 = x+d2
                              ok1 = (a-u1)*(u1-b) > 0 && dx*d1 <= 0
                              ok2 = (a-u2)*(u2-b) > 0 && dx*d2 <= 0
                            in 
                              if ok1 || ok2 then
                                let d'' = if ok1 && ok2 then
                                            if abs d1 < abs d2 then d1 else d2
                                          else 
                                            if ok1 then d1 else d2
                                in if abs d'' <= abs (0.5 * e) then
                                     let u' = x + d''
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

            return $ 
                if abs d' < tol1' && fu' > fx  
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
--
-- FIXME: if we get an exact solution this doesn't stop the optimization
stop_dbrent :: (Show a, OrdField a) => a -> StopCondition_ (Iterator_dbrent a)
-- stop_brent tol _ opt = trace ("a="++show a++"; b="++show b++"; left="++show (abs (x-xm))++"; right="++show (tol2'-0.5*(b-a))) $
--     return $ abs (x-xm) <  tol2'-0.5*(b-a)
stop_dbrent tol _ opt = return $ should_break || abs (x-xm) < tol2'-0.5*(b-a)
    where
        (Iterator_dbrent a b d e fv fw fx dv dw dx v w x should_break) = opt
        xm = 0.5*(a+b)
        tol1' = tol*(abs x)+zeps
        tol2' = 2*tol1'
        zeps = 1e-10

dbrent_test_ = let
    f :: Rational -> Rational
    f x = (x-5)*(x-5)
    df :: Rational -> Rational
    df x = 2*x - 10
    
    in traceHistory $ do 
            lb <- lineBracket f 0 1
            fminuncM_dbrent_ (return . f) (return . df) lb (maxIterations 10 || stop_dbrent 1e-10)

