
module HLearn.Models.Distributions
    where

import SubHask
import SubHask.TemplateHaskell.Deriving

--------------------------------------------------------------------------------

-- | In measure theory, it is common to treat discrete and continuous distributions the same.
-- This lets us more easily generalize to multivariate distributions.
--
-- FIXME:
-- We need to think carefully about this class hierarchy.
-- Every distribution has a cdf, but these are often difficult to calculate.
-- Not every distribution has a density, but most of the useful ones do.
class Distribution d where

    mean :: d -> Elem d

    -- | The density (or mass) function of the distribution.
    pdf  :: d -> Elem d -> Scalar d

    -- | Gives a result proportional to pdf.
    -- This is usually faster to compute and many algorithms don't require the pdf be normalized.
    pdf_ :: d -> Elem d -> Scalar d
    pdf_ = pdf

--------------------------------------------------------------------------------

-- | Stores the unnormalized raw 0th, 1st, and 2nd moments of a distribution.
-- These are sufficient statistics for many distributions.
-- We can then easily construct other distributions from this distribution.
--
-- FIXME:
-- This data type has some numeric stability built-in.
-- But for many distributions, there exist methods of calculating the parameters that are even more stable.
data Moments v = Moments
    { m0 :: !(Scalar v)
    , m1 :: !v
    , m2 :: !(v><v)
    }

mkMutable [t| forall v. Moments v |]

type instance Scalar (Moments v) = Scalar v
type instance Logic (Moments v) = Logic v
type instance Elem (Moments v) = v

type instance Moments v >< r = Moments (v><r)

-------------------
-- numeric hierarchy

instance Hilbert v => Semigroup (Moments v) where
    (Moments a1 b1 c1)+(Moments a2 b2 c2) = Moments (a1+a2) (b1+b2) (c1+c2)

instance Hilbert v => Monoid (Moments v) where
    zero = Moments zero zero zero

instance Hilbert v => Abelian (Moments v)

instance Hilbert v => Cancellative (Moments v) where
    (Moments a1 b1 c1)-(Moments a2 b2 c2) = Moments (a1-a2) (b1-b2) (c1-c2)

instance Hilbert v => Group (Moments v) where
    negate (Moments a b c) = Moments (negate a) (negate b) (negate c)

instance Hilbert v => Module (Moments v) where
    (Moments a b c).*r = Moments (r*a) (b.*r) (c.*r)

instance Hilbert v => FreeModule (Moments v) where
    -- TODO: what is the probabilistic interpretation of this?
    (Moments a1 b1 c1).*.(Moments a2 b2 c2) = Moments (a1*a2) (b1.*.b2) (c1.*.c2)

instance Hilbert v => VectorSpace (Moments v) where
    (Moments a b c)./r = Moments (r/a) (b./r) (c./r)

    (Moments a1 b1 c1)./.(Moments a2 b2 c2) = Moments (a1/a2) (b1./.b2) (c1./.c2)

-------------------
-- container hierarchy

instance Hilbert v => Constructible (Moments v) where
    singleton v = Moments 1 v (v><v)

    -- FIXME: use kahan summation
    -- fromList1

----------------------------------------

-- | FIXME: I haven't properly tested this code
newtype Normal v = Normal (Moments v)

mkMutable [t| forall v. Normal v |]

-- deriveHierarchy ''Normal [ ''Hilbert, ''Constructible ]

type instance Scalar (Normal v) = Scalar v
type instance Elem (Normal v) = v

instance Hilbert v => Semigroup (Normal v) where
    (Normal n1)+(Normal n2)=Normal $ n1+n2

instance Hilbert v => Monoid (Normal v) where
    zero = Normal zero

train1Normal :: Hilbert v => v -> Normal v
train1Normal v = Normal $ singleton v

instance (FiniteModule v, Hilbert v) => Distribution (Normal v) where

    mean (Normal (Moments m0 m1 m2)) = m1 ./ m0

    pdf (Normal (Moments m0 m1 m2)) v
        = (2*pi*size sigma)**(-fromIntegral (dim v)/2)*exp((-1/2)*(v' `vXm` reciprocal sigma)<>v')
        where
            v' = v - mu

            mu    = m1 ./ m0
            sigma = 1 + m2 ./ m0 - mu><mu

-- variance :: Hilbert v => Normal v -> v><v
-- variance (Normal (Moments m0 m1 m2)) = m2 ./ m0 - mu><mu
--     where
--         mu    = m1 ./ m0

