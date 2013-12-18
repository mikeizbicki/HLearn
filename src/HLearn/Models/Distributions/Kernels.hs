-- | This list of kernels is take from wikipedia's: <https://en.wikipedia.org/wiki/Uniform_kernel#Kernel_functions_in_common_use>
module HLearn.Models.Distributions.Kernels
    (
    
    -- * Kernels
     Uniform (..)
    , Triangular (..)
    , Epanechnikov (..)
    , Quartic (..)
    , Triweight (..)
    , Tricube (..)
    , Gaussian (..)
    , Cosine (..)
    
    )
    where
   
import Control.DeepSeq
import HLearn.Algebra
    
data Uniform = Uniform deriving (Read,Show)
instance (Fractional num, Ord num) => Function Uniform num num where
    function _ u = if abs u < 1
        then 1/2
        else 0

data Triangular = Triangular deriving (Read,Show)
instance (Fractional num, Ord num) => Function Triangular num num where
    function _ u = if abs u<1
        then 1-abs u
        else 0
        
data Epanechnikov = Epanechnikov deriving (Read,Show)
instance (Fractional num, Ord num) => Function Epanechnikov num num where
    function _ u = if abs u<1
        then (3/4)*(1-u^^2)
        else 0

data Quartic = Quartic deriving (Read,Show)
instance (Fractional num, Ord num) => Function Quartic num num where
    function _ u = if abs u<1
        then (15/16)*(1-u^^2)^^2
        else 0
        
data Triweight = Triweight deriving (Read,Show)
instance (Fractional num, Ord num) => Function Triweight num num where
    function _ u = if abs u<1
        then (35/32)*(1-u^^2)^^3
        else 0

data Tricube = Tricube deriving (Read,Show)
instance (Fractional num, Ord num) => Function Tricube num num where
    function _ u = if abs u<1
        then (70/81)*(1-u^^3)^^3
        else 0
        
data Cosine = Cosine deriving (Read,Show)
instance (Floating num, Ord num) => Function Cosine num num where
    function _ u = if abs u<1
        then (pi/4)*(cos $ (pi/2)*u)
        else 0
        
data Gaussian = Gaussian deriving (Read,Show)
instance (Floating num, Ord num) => Function Gaussian num num where
    function _ u = (1/(2*pi))*(exp $ (-1/2)*u^^2)
