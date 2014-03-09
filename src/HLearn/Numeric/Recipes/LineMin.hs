module HLearn.Numeric.Recipes.LineMin
    where

import Debug.Trace

-------------------------------------------------------------------------------

-- stepGoldenSectionSearch :: (Fractional a,Ord a) => (a -> a) -> a -> a -> a -> a
-- stepGoldenSectionSearch f ax bx cx = go 0 (f x1_) (f x2_) 0 ax x1_ x2_ cx
goldenSectionSearch !f !ax !bx !cx = go (f ax) (f x1_) (f x2_) (f cx) ax x1_ x2_ cx
    where
        x1_ = if abs (cx-bx) > abs (bx-ax)
            then bx
            else bx-c*(bx-ax)
        x2_ = if abs (cx-bx) > abs (bx-ax)
            then bx+c*(cx-bx)
            else bx

        tol = 1e-3
        r = 0.61803399
        c = 1-r

        go !f0 !f1 !f2 !f3 !x0 !x1 !x2 !x3 = trace ("go x1="++show x1++"; f1="++show f1++"; x2="++show x2++"; f2="++show f2) $
          if abs (x3-x0) > tol * (abs x1 + abs x2)
            then if f2 < f1
                then let 
                    x0' = x1
                    x1' = x2
                    x2' = r*x2+c*x3
                    x3' = x3
                    f0' = f1
                    f1' = f2
                    f2' = f x2'
                    f3' = f3
                    in go f0' f1' f2' f3' x0' x1' x2' x3'
                else let 
                    x3' = x2
                    x2' = x1
                    x1' = r*x1+c*x0
                    x0' = x0
                    f3' = f2
                    f2' = f1
                    f1' = f x1'
                    f0' = f0 
                    in go f0' f1' f2' f3' x0' x1' x2' x3'
            else trace "stopping" $ if f1 < f2
                then x1
                else x2

gss f (LineBracket ax bx cx fa fb fc) = itr 100 result_GoldenSectionSearch stop_GoldenSectionSearch (step_GoldenSectionSearch f) ( f x1,f x2,ax,x1,x2,cx )
    where
        x1 = if abs (cx-bx) > abs (bx-ax)
            then bx
            else bx-c*(bx-ax)
        x2 = if abs (cx-bx) > abs (bx-ax)
            then bx+c*(cx-bx)
            else bx

        r = 0.61803399
        c = 1-r

itr :: Int -> (tmp -> a) -> (tmp -> Bool) -> (tmp -> tmp) -> tmp -> a
itr i result stop step init = --trace ("i="++show i++"; init="++show init) $ 
  if i==0 || stop init
    then result init
    else itr (i-1) result stop step (step init)

-------------------------------------------------------------------------------

result_GoldenSectionSearch :: (Fractional a, Ord a) => ( a,a,a,a,a,a ) -> a
result_GoldenSectionSearch ( !f1,!f2,_,!x1,!x2,_ ) = if f1 < f2
    then x1
    else x2

stop_GoldenSectionSearch :: (Fractional a, Ord a) => ( a,a,a,a,a,a ) -> Bool
stop_GoldenSectionSearch ( _,_,!x0,!x1,!x2,!x3 ) = abs (x3-x0) <= tol*(abs x1+abs x2)
    where
        tol = 1e-6

step_GoldenSectionSearch :: (Fractional a, Ord a) => (a -> a) -> ( a,a,a,a,a,a ) -> ( a,a,a,a,a,a )
step_GoldenSectionSearch !f !( !f1, !f2, !x0, !x1, !x2, !x3 ) = if f2 < f1
    then let x' = r*x2+c*x3 in ( f2, f x', x1, x2, x', x3 )
    else let x' = r*x1+c*x0 in ( f x', f1, x0, x', x1, x2 )
    where
        r = 0.61803399
        c = 1-r
    
-------------------------------------------------------------------------------

data LineBracket a = LineBracket { _ax :: !a, _bx :: !a, _cx :: !a, _fa :: !a, _fb :: !a, _fc :: !a }
    deriving (Read,Show)

lineBracket !f !pt1 !pt2 = itr 100 id stop_LineBracket (step_LineBracket f) $ LineBracket
    { _ax = ax
    , _bx = bx
    , _cx = cx
    , _fa = f ax
    , _fb = f bx
    , _fc = f cx
    }
    where
        (ax,bx) = if f pt1 > f pt2
            then (pt1,pt2)
            else (pt2,pt1)
        cx = bx+gold*(bx-ax)
        gold = 1.618034


stop_LineBracket :: (Fractional a, Ord a) => LineBracket a -> Bool
stop_LineBracket !lb = _fb lb <= _fc lb


step_LineBracket !f lb@(LineBracket ax bx cx fa fb fc) = ret
    where
        sign a b = if b>0 then abs a else -(abs a)
        tiny = 1e-20
        glimit = 100
        gold = 1.618034

        r = (bx-ax)*(fb-fc)
        q = (bx-cx)*(fb-fa)
        u = bx-((bx-cx)*q-(bx-ax)*r)/2*(sign (max (abs $ q-r) tiny) (q-r))
        fu = f u
        ulim = bx+glimit*(cx-bx)

        ret = if (bx-u)*(u-cx) > 0
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
                    else let u' = cx+gold*(cx-bx) in lb -- Parabolic fit was no use.  Use default magnification
                        { _ax = bx
                        , _bx = cx
                        , _cx = u'
                        , _fa = fb
                        , _fb = fc
                        , _fc = f u' 
                        }
            else if (cx-u)*(u-ulim) > 0 -- parabolic fit is between c and its allowed limit
                then if fu < fc 
                    then let u' = cx+gold*(cx-bx) in lb
                        { _ax = cx
                        , _bx = u
                        , _cx = u'
                        , _fa = fc
                        , _fb = fu
                        , _fc = f u'
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
                        , _fc = f ulim
                        }
                    else let u' = cx+gold*(cx-bx) in lb -- reject parabolic u, use default magnification
                        { _ax = bx
                        , _bx = cx
                        , _cx = u'
                        , _fa = fb
                        , _fb = fc
                        , _fc = f u'
                        }

-------------------------------------------------------------------------------

data Brent a = Brent { _a :: !a, _b :: !a, _d :: !a, _e :: !a, _fv :: !a, _fw :: !a, _fx :: !a, _v :: !a, _w :: !a, _x :: !a }
    deriving (Read,Show)

brent !f (LineBracket ax bx cx fa fb fc) = itr 100 id stop_Brent (step_Brent f) init
    where
        init = Brent
            { _a = min ax cx
            , _b = max ax cx
            , _e = 0
            , _v = bx
            , _w = bx
            , _x = bx
            , _fv = f bx
            , _fw = f bx
            , _fx = f bx

            , _d = 0
            }

stop_Brent :: (Fractional a, Ord a) => Brent a -> Bool
stop_Brent (Brent a b d e fv fw fx v w x) = abs (x-xm) <= tol2'-0.5*(b-a)
    where
        xm = 0.5*(a+b)
        tol1' = tol*(abs x)+zeps
        tol2' = 2*tol1'
        tol = 1e-6
        zeps = 1e-10

findmin 1f = _x $ brent f $ lineBracket f 10 100 
findmin_gss f = gss f $ lineBracket f 10 100 

-- step_Brent :: (Fractional a, Ord a) => Brent a -> Brent a
step_Brent !f brent@(Brent a b d e fv fw fx v w x) = brent'
    where
        cgold = 0.3819660
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
--                     else trace "a" $ let e'' = if x>=xm then a-x else b-x in (cgold*e'',e'')
                    else let d''=p''/q'''; u''=x+d'' in
                        if u''-a < tol2' || b-u'' < tol2'
                            then (sign tol1' (xm-x),d)
                            else (d'',d)
            else let e'' = if x>=xm then a-x else b-x in (cgold*e'',e'')

        u' = if abs d' >= tol1'
            then x+d'
            else x+sign tol1' d'
        fu' = f u'

        brent' = if fu' <= fx
            then brent
                { _e = e'
                , _d = d'
                , _a = if u' >= x then x else a
                , _b = if u' >= x then b else x
                , _v = w
                , _w = x
                , _x = u'
                , _fv = fw
                , _fw = fx
                , _fx = fu'
                }
            else brent
                { _e = e'
                , _d = d'
                , _a = if u' < x then u' else a
                , _b = if u' < x then b  else u'
                , _v  = if fu' <= fw || w==x then w   else if fu' <= fv || v==x || v==w then u'  else v
                , _fv = if fu' <= fw || w==x then fw  else if fu' <= fv || v==x || v==w then fu' else fv
                , _w  = if fu' <= fw || w==x then u'  else e
                , _fw = if fu' <= fw || w==x then fu' else fw
                }

-------------------------------------------------------------------------------

