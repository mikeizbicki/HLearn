-- | This list of kernels is take from wikipedia's: <https://en.wikipedia.org/wiki/Uniform_kernel#Kernel_functions_in_common_use>
module HLearn.Models.Distributions.Kernels
    (
    -- * Data types
    Kernel (..)
    , KernelBox (..)
    
    -- * Kernels
    , Uniform (..)
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

-- | A kernel is function in one parameter that takes a value on the x axis and spits out a "probability."  We create a data object for each kernel, and a corresponding class to make things play nice with the type system.
class Kernel kernel num where
    evalKernel :: kernel -> num -> num

-- | A KernelBox is a universal object for storing kernels.  Whatever kernel it stores, it becomes a kernel with the same properties.
data KernelBox num where KernelBox :: (Kernel kernel num, Show kernel) => kernel -> KernelBox num

instance Kernel (KernelBox num) num where
    evalKernel (KernelBox k) p = evalKernel k p
instance Show (KernelBox num) where
    show (KernelBox k) = "KB "++show k
instance Eq (KernelBox num) where
    KernelBox k1 == KernelBox k2 = (show k1) == (show k2)
instance Ord (KernelBox num) where
    _ `compare` _ = EQ
instance NFData (KernelBox num) where
    rnf (KernelBox num)= seq num ()
    
data Uniform = Uniform deriving (Read,Show)
instance (Fractional num, Ord num) => Kernel Uniform num where
    evalKernel Uniform u = if abs u < 1
        then 1/2
        else 0

data Triangular = Triangular deriving (Read,Show)
instance (Fractional num, Ord num) => Kernel Triangular num where
    evalKernel Triangular u = if abs u<1
        then 1-abs u
        else 0
        
data Epanechnikov = Epanechnikov deriving (Read,Show)
instance (Fractional num, Ord num) => Kernel Epanechnikov num where
    evalKernel Epanechnikov u = if abs u<1
        then (3/4)*(1-u^^2)
        else 0

data Quartic = Quartic deriving (Read,Show)
instance (Fractional num, Ord num) => Kernel Quartic num where
    evalKernel Quartic u = if abs u<1
        then (15/16)*(1-u^^2)^^2
        else 0
        
data Triweight = Triweight deriving (Read,Show)
instance (Fractional num, Ord num) => Kernel Triweight num where
    evalKernel Triweight u = if abs u<1
        then (35/32)*(1-u^^2)^^3
        else 0

data Tricube = Tricube deriving (Read,Show)
instance (Fractional num, Ord num) => Kernel Tricube num where
    evalKernel Tricube u = if abs u<1
        then (70/81)*(1-u^^3)^^3
        else 0
        
data Cosine = Cosine deriving (Read,Show)
instance (Floating num, Ord num) => Kernel Cosine num where
    evalKernel Cosine u = if abs u<1
        then (pi/4)*(cos $ (pi/2)*u)
        else 0
        
data Gaussian = Gaussian deriving (Read,Show)
instance (Floating num, Ord num) => Kernel Gaussian num where
    evalKernel Gaussian u = (1/(2*pi))*(exp $ (-1/2)*u^^2)
