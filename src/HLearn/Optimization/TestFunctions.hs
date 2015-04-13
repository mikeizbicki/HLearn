module HLearn.Optimization.TestFunctions
    where


import SubHask
import SubHask.Category.Trans.Derivative

import HLearn.History
import HLearn.Optimization.Multivariate
import HLearn.Optimization.Univariate

-------------------------------------------------------------------------------

-- import SubHask.Compatibility.Vector
-- import SubHask.Compatibility.HMatrix
-- import qualified Data.Vector.Generic as VG

sphere :: Hilbert v => C2 (v -> Scalar v)
sphere = unsafeProveC2 f f' f''
    where
        f   v = let x = size v in x*x
        f'  v = 2*.v
        f'' v = 2

logloss :: ( IsScalar v, Floating v ) => C2 ( v -> v )
logloss = unsafeProveC2 f f' f''
    where
        f   x = log (1 + exp (-x) )
        f'  x = -1/(1 + exp x)
        f'' x = exp x / (1 + exp x)**2

{-
f :: Hilbert v => v -> v -> Scalar v
f  x y = (x-y)<>(x-y)

f' :: Hilbert v => v -> v -> v
f' x y = 2*.(x-y)

f'' :: (TensorAlgebra v, Module (Outer v), Hilbert v) => v -> v -> Outer v
f'' x y = 2*.1

u = VG.fromList [1,2,3,4,5,6,7]     :: Vector Double
v = VG.fromList [2,3,1,0,-2,-3,-1]  :: Vector Double
w = VG.fromList [1,1]  :: Vector Double

beale :: (FiniteModule v, Hilbert v) => v -> Scalar v
beale v = (1.5-x+x*y)**2 + (2.25 - x + x*y**2)**2 + (2.625 - x + x*y**3)**2
    where
        x=v!0
        y=v!1

beale' :: (FiniteModule v, Hilbert v) => v -> v
beale' v = unsafeToModule
    [ 2*(-1+y)*(1.5-x+x*y)**2 + 2*(-1+y**2)*(2.25 - x + x*y**2)**2 + 2*(-1+y**3)*(2.625 - x + x*y**3)**2
    , 2*x*(1.5-x+x*y)**2 + 2*x*2*y*(2.25 - x + x*y**2)**2 + 2*x*3*y*y*(2.625 - x + x*y**3)**2
    ]
    where
        x=v!0
        y=v!1

-- beale'' :: (FiniteModule v, TensorAlgebra, Hilbert v) => v -> Outer v
-- beale'' v = mkMatrix 2 2
--     [ 2*(-1+y)*(-1+y)**2 + 2*(-1+y**2)*(- 1 + y**2)**2 + 2*(-1+y**3)*(-1 + y**3)**2
--     , 2*(-1+y)*(1.5-x+x*y)**2
--     + 2*(-1+y)*(1.5-x+x*y)**2
--     + 2*(-1+y**2)*(2.25 - x + x*y**2)**2
--     + 2*(-1+y**3)*(2.625 - x + x*y**3)**2
--     , 2*x*(1.5-x+x*y)**2 + 2*x*2*y*(2.25 - x + x*y**2)**2 + 2*x*3*y*y*(2.625 - x + x*y**3)**2
--     , 2*x*(1.5-x+x*y)**2 + 2*x*2*y*(2.25 - x + x*y**2)**2 + 2*x*3*y*y*(2.625 - x + x*y**3)**2
--     ]
     -}
