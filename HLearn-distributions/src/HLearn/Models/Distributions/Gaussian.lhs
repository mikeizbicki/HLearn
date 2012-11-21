\documentclass{article}
%include polycode.fmt

\usepackage{amsmath,amsthm,amsfonts,amssymb,graphicx,color}

\newcommand{\todo}[1]{\textcolor{red}{[TODO: #1]}}
%\newcommand{\todo}[1]{[TODO: #1]}

\newtheorem{theorem}{Theorem}
\newtheorem{claim}{Claim}
\newtheorem{lemma}{Lemma}
\newtheorem{corollary}{Corollary}

\newtheoremstyle{namedtheorem}{}{}{\itshape}{}{\bfseries}{.}{.5em}{\thmnote{#3 }#1}
\theoremstyle{namedtheorem}
\newtheorem*{namedtheorem}{Theorem}
\newtheorem*{namedcorollary}{Corollary}

\newtheoremstyle{nameddefinition2}{}{}{}{}{\bfseries}{}{.5em}{#1\thmnote{#3}.}
\theoremstyle{nameddefinition2}
\newtheorem*{nnote}{}


\long\def\ignore#1{}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Document info
% 

\title{Algebraic Properties of the Gaussian Distribution}

\author{
Michael Izbicki
}

% 
% Document
% 

\begin{document}
\maketitle

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lhs2TeX Formating
% 

%format m1 = m"_1"
%format m2 = m"_2"
%format m1a = m"_{1a}"
%format m2a = m"_{2a}"
%format na = n"_a"
%format m1b = m"_{1b}"
%format m2b = m"_{2b}"
%format nb = n"_b"
%format m1' = m"_1''"
%format m2' = m"_2''"
%format n' = n"''"

%format g_a = g"_a"
%format g_b = g"_b"
%format g_c = g"_c"

%format ^ = "^"
%format <> = "\diamond"
%format <> = "\diamond"
%format Infinity = "\infty"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code Header
%

\ignore{
\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE BangPatterns #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | The Gaussian distribution is an instance of 'HomTrainer.'  For technical details, view the extended documentation <todo>.

module HLearn.Models.Distributions.Gaussian
    ( Gaussian (..)
    , GaussianParams (..)
    )
    where
    
import Control.Monad
import Control.Monad.Random
import Data.Random.Normal
import Debug.Trace
import Numeric.SpecFunctions (logFactorial)
import Test.QuickCheck

import Data.Vector.Generic.Base
import Data.Vector.Generic.Mutable
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Deriving

import qualified Statistics.Distribution as D
import qualified Statistics.Distribution.Normal as N

import HLearn.Algebra
import HLearn.Models.Distributions.Common

{-# INLINE fi #-}
fi :: (Integral a, Num b) => a -> b
fi=fromIntegral

\end{code}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Document Begins Here
%

\section{Introduction}

\subsection{About this document}
This document covers two things.  First, it covers the theory of using algebra to manipulate Gaussian distributions.  To my knowledge, this is fairly unexplored territory.  Second, this document covers the design decisions made in HLearn's implementation of this algebra.  This document is specifically not meant to be a reference manual for using the HLearn library.  That's what the Haddock documentation is for.  
Finally, the HLearn library was written using literate Haskell, so this file is actually also the source code to the library!

\subsection{About the Gaussian distribution}
The Gaussian distribution (also called the Normal distribution) is one of the most common distributions in statistics and machine learning.

\section{Data Types}
A Gaussian distribution is uniquely determined from its mean and variance.  In order to estimate these quantities from a sample, we must also know the number of data points in the sample.  Therefore, we define a Gaussian distribution as the following 4-tuple:

\begin{code}

data Gaussian datapoint = Gaussian
    { n  :: {-# UNPACK #-} !Int     -- ^ The number of samples  trained on
    , m1 :: !datapoint              -- ^ The mean (first moment) of the trained distribution
    , m2 :: !datapoint              -- ^ The variance (second moment) of the trained distribution times (n-1)
    , dc :: {-# UNPACK #-} !Int     -- ^ The number of \"dummy points\" that have been added to the distribution.  Required for numerical stability reasons.
    } 
    deriving (Show,Read)

{-
data Gaussian = Gaussian
    { n   :: {- UNPACK -} !Int         -- the number of samples seen
    , m1  :: {- UNPACK -} !Double      -- the first moment (mean)
    , m2  :: {- UNPACK -} !Double      -- the second moment (pop. variance) times n
    , dc  :: {- UNPACK -} !Int         -- the number of "dummy" points that have been added to the Gaussian
    }
    deriving (Show,Read)
-}

\end{code}

There are several important notes to make about this data structure:
\begin{enumerate}
\item Normally a Gaussian distribution would have non-negative values for both the standard deviation and the number of samples.  We make no such assumption.  The normal operations we perform on a distribution (e.g. the pdf) will only be well defined in these circumstances, but the algebraic properties of the distribution require that all parameters can be negative.

\item We have introduced a fourth variable called |dc|.  This will be necessary to track how many "dummy datapoints" we have added to the Gaussian for numerical stability reasons.  See section \ref{sec:dummy}.

%\item We do not specify the base data type to store our estimates of the parameters.  Normally, we will use the |Double| type to do this, but if we have millions of estimated Gaussians, we may want to use the |Float| type instead.  Similarly, if we require extra precision we may use the |LogFloat| data type at the expense of performance.
\end{enumerate}

For convenience, we also define the following accessor functions to the sample mean and sample variance:
\begin{code}

mean :: (Floating datatype) => Gaussian datatype -> datatype
mean (Gaussian n m1 m2 dc) = m1
 
var :: (Floating datatype) => Gaussian datatype -> datatype
var (Gaussian n m1 m2 dc) = m2/(fi $ n-1)

stddev :: (Floating  datapoint, Ord datapoint) => (Gaussian datapoint) -> datapoint
stddev = sqrt . var

\end{code}

Finally, we make our Gaussian type capable of being unboxed:
\begin{code}

-- derivingUnbox "Gaussian"
--     [d| instance Unbox' (Gaussian) (Int, Double, Double, Int) |]
--     [| \ (Gaussian n m1 m2 f) -> (n,m1,m2,f) |]
--     [| \ (n,m1,m2,f) -> (Gaussian n m1 m2 f) |]

derivingUnbox "Gaussian"
    [d| instance (U.Unbox a) => Unbox' (Gaussian a) (Int, a, a, Int) |]
    [| \ (Gaussian n m1 m2 dc) -> (n,m1,m2,dc) |]
    [| \ (n,m1,m2,dc) -> (Gaussian n m1 m2 dc) |]

-- instance NFData Gaussian where
instance (NFData datapoint) => NFData (Gaussian datapoint) where
    rnf (Gaussian n m1 m2 dc) = seq (rnf m1) $ seq (rnf m2) $ seq (rnf dc) (rnf n)

\end{code}

\section{Algebra}

We are following the homomorphic learning framework.  In this section, we will concern ourselves with algebraic manipulations of fully trained Gaussian models.  In particular, we will see how to convert them into other fully trained Gaussian models.

%We will do this by working backwards from a known batch trainer for the Gaussian distribution.  Knuth presents the %following recurrence relations in pg 232 of Vol2 AoCP:
%\begin{align*}
%m1_k &= m1_{k-1}+(x_k-m1_{k-1})/k\\
%m2_k &= m2_{k-1}+(x_k-m1_{k-1})/(x_k-m1_k)
%\end{align*}

\subsection{Semigroup}
We want to construct the semigroup operation for our Gaussian distribution so that our batch trainer will be a semigroup homomorphism.  That is, we want the property:
\begin{spec}
(train xs)<>(train ys) = train (xs++ys)
\end{spec}
To do this, we must construct appropriate definitions of |n'|, |m1'|, and |m2'| below:
\begin{spec}
(Gaussian na m1a m2a) <> (Gaussian nb m1b m2b) = Gaussian n' m1' m2'
\end{spec}
This is a somewhat long but straightforward process of applying the definitions in Section \ref{sec:defn} and then performing symbol manipulation.  We start by calculating the number of samples |n'|.  The number of samples in the resulting distribution is simply the sum of the number of samples in both the input distributions:
\begin{align}
\label{eq:n'}
n' = n_a+n_b
\end{align}

Second, we calculate the new average |m1'|.  We start with the definition that:
\begin{align}
m_1' &= \frac{1}{n'}\sum_{i=1}^{n'} x_i
\end{align}
Then we split the summation according to whether the input element $x_i$ was from the left Gaussian a or right Gaussian b, substitute equation \ref{eq:}, and rearrange:
\begin{align}
m_1' &= \frac{1}{n'}\left(\sum_{i=1}^{n_a} x_{ia} + \sum_{i=1}^{n_b} x_{ib}\right)\\
&= \frac{1}{n'}\left(n_a m_{1a} + n_b m_{1b}\right)\\
&= m_{1a}\frac{n_a}{n'} + m_{1b}\frac{n_b}{n'}
\end{align}
Notice that equation \ref{eq:m1'} is simply the weighted average of the two means.  This makes intuitive sense.  But there is a slight problem with this definition: When implemented on a computer with floating point arithmetic, we will get |Infinity| whenever |n'| is 0.  
We will solve this problem by adding a ``dummy'' element into the Gaussian whenever |n'| would be zero.  This prevents the division by zero and maintains our associativity.  See section \ref{dummy} for more info. 

Finally, we must calculate the new variance |m2'|.  We start with the definition that:
\begin{align}
m_2' &= \sum_{i=1}^{n'}(x_i - m_1')^2
\end{align}
Then, we substitute with equation \ref{eq}, split the summations, and substitute with equation \ref{}:
\begin{align}
%m_2' &= \sum_{i=1}^{n'}(x_i^2 - 2x_im_1' + m_1'^2)\\
m_2' &= \sum_{i=1}^{n'}(x_i^2 - m_1'^2)\\
&= \sum_{i=1}^{n_a}(x_{ia}^2) + \sum_{i=1}^{n_b}(x_{ib}^2) - n'm_1'^2\\
\label{eq:m2'}
&= m_{2a} + n_a m_{1a}^2 + m_{2b} + n_b m_{1b}^2 - n' m_1'^2
%&= m_{2a} + \frac{m_{1a}^2}{n_a} + m_{2b} + \frac{m_{2b}^2}{n_b} - \frac{m_1'^2}{n'}
%m_2' = m_{2a}+m_{2b}+\frac{n_an_b}{n'}(m_{1a}-m_{2a})^2
\end{align}
We combine equations \ref{eq:n'}, \ref{eq:m1'} and \ref{eq:m2'} to get the semigroup instance for the |Gaussian| data type:

\subsubsection{Proof of Validity}

For this to form a valid semigroup instance, we must satisfy the associativity law by showing that for any three |g_a, g_b, g_c :: Gaussian|:
\begin{align}
(g_a \diamond g_b) \diamond g_c  = g_a \diamond (g_b \diamond g_c)
\end{align}

We do this by showing that each field in the left side of the equation ($g_l$) equals the corresponding field in the right side ($g_r$).  We can easily see this for |n| because:
$$
n_l = (n_a + n_b) + n_c = n_a + (n_b + n_c) = n_r
$$

%For |m2|, we have:
%\begin{align*}
%m_{2l} &= m_{2a} + n_a m_{1a}^2 + m_{2b} + n_b m_{2b}^2 - n' m_1'^2
%\end{align*}

%\begin{align*}
%%((m_{1a}, m_{2a}, n_a) \diamond (m_{1b}, m_{2b}, n_b)) \diamond (m_{1c}, m_{2c}, n_c)
%\end{align*}

\todo{Finish proof}


\subsubsection{The Dummy Point}
\label{dummy}
Then, when we add another Gaussian, taking the non-adjusted |n'| away from zero, we can remove the dummy element to restore the Gaussian to its original condition.  We actually depend on the group inverse to perform this operation.

We define an equivalence relation over the Gaussians that takes this into account:
\begin{align}
g_a \sim g_b \iff g_a \diamond d^i = g_b \diamond d^j
\end{align}

\subsubsection{Performance Notes}
For some reason, the commented out line results in |(<>)| an almost 10% decrease in speed.  None of the other lines cause any performance hit at all.  The commented line is not actually necessary for consistency, it's just a nicety.

The commented out line in |merge| also results in a huge performance hit.  The division must eat up a serious amount of time.
% ga == inverse gb              = identity                 -- See below.

\subsubsection{The Code}
Finally we are ready to see the actual code:

\begin{code}

-- instance Semigroup Gaussian where
instance (Fractional datapoint) => Semigroup (Gaussian datapoint) where
    {-# INLINE (<>) #-}
    (<>) ga@(Gaussian na m1a m2a fa) gb@(Gaussian nb m1b m2b fb)
        | n'==0 && fa >0                = (inverse dummy `merge` ga) `merge` gb
        | n'==0 && fb >0                = (inverse dummy `merge` gb) `merge` ga
        | n'==0                         = (dummy `merge` ga) `merge` gb
        | n' >1 && (fa >0 || fb>0)      = (ga `merge` gb) `merge` inverse dummy
        | otherwise                     = merge ga gb
            where n' = na+nb
        
{-# INLINE dummy #-}
-- dummy :: Gaussian
dummy :: (Num datatype) => Gaussian datatype
dummy = Gaussian 2 2 2 1

{-# INLINE merge #-}
-- merge :: Gaussian -> Gaussian -> Gaussian
merge :: (Fractional datatype) => Gaussian datatype -> Gaussian datatype -> Gaussian datatype
merge !ga@(Gaussian na m1a m2a fa) !gb@(Gaussian nb m1b m2b fb) = Gaussian n' m1' m2' f'
    where
        n'  = na+nb
        m1' = (m1a*(fi na)+m1b*(fi nb))/(fi n')
        m2' = m2a + m2b + (m1a^2)*(fi na) + (m1b^2)*(fi nb) - (m1'^2)*(fi n')
        f' = fa+fb        

\end{code}
And our dummy data point is:
\begin{code}
\end{code}
It's possible there is a numerically optimal distribution to use for best accuracy, however, this seems to work pretty well enough in practice.


\subsection{Monoid}
Now, we must explicitly define a Monoid instance for our class as:
\begin{code}

-- instance Monoid Gaussian where
instance (Fractional datapoint) => Monoid (Gaussian datapoint) where
    {-# INLINE mempty #-}
    mempty = Gaussian 0 0 0 0
    {-# INLINE mappend #-}
    mappend = (<>)

\end{code}
We will show that this is a valid left identity (the right identity is similar) by showing:
\begin{align}
%identity <> Gaussian nb m1b m2b = Gaussian n' m1' m2'
\epsilon \diamond g_b = g' \Rightarrow g_b = g'
\end{align}
This is straightforward:
\begin{align}
n' & = 0 + n_b = n_b\\
m_1' &= 0 \frac {0}{n_b} + m_{1b}\frac{n_b}{n_b} = m_{1b} \\
m_2' &= 0 + 0 + m_{2b} + n_b m_{1b}^2 - n' m_1'^2 = m_{2b} + n_b m_{1b}^2 - n_b m_{1b}^2 = m_{2b}
\end{align}

\subsection{Group}

The inverse operation for the Gaussian is defined as:
\begin{code}
  
-- instance Invertible Gaussian where    
instance (Fractional datapoint) => RegularSemigroup (Gaussian datapoint) where
    {-# INLINE inverse #-}
    inverse (Gaussian n m1 m2 dc) = Gaussian (-n) m1 (-m2) (-dc)

instance (Fractional datapoint) => Group (Gaussian datapoint)
\end{code}
We will show this is a valid left inverse (the right inverse is similar) by showing:
\begin{align}
g^{-1}\diamond g = \epsilon
\end{align}
This is straightforward:
\begin{align}
n' &= n + (-n) = 0 \\
m_1' &= 0 \text{     Only true after factoring in the dummy elements} \\
m_2' &=
\end{align}

\section{Model Training}
We use the homomorphic learning method to define the training routines for our Gaussian distribution.  That means we need to define only a single training method.  The singleton trainer is particularly simple:
\begin{code}

-- | Training a Gaussian distribution takes no parameters
data GaussianParams datatype = GaussianParams

instance Model (GaussianParams datatype) (Gaussian datatype) where
    getparams _ = GaussianParams
    
instance DefaultModel (GaussianParams datatype) (Gaussian datatype) where
    defparams = GaussianParams

instance NFData (GaussianParams datatype) where
    rnf params = ()

instance HomTrainer (GaussianParams Double) Double (Gaussian Double) where
    train1dp' GaussianParams x = Gaussian 1 x 0 0

\end{code}

\section{Model Use}
\begin{code}
convdistr :: Gaussian Double -> N.NormalDistribution
convdistr g = N.normalDistr (mean g) (stddev g)

instance Distribution (Gaussian Double) Double Double where
    pdf g x = D.density (convdistr g) x
    cdf g x = D.cumulative (convdistr g) x
    cdfInverse g x = D.quantile (convdistr g) x
\end{code}

\section{Distribution Functions}

\subsection{Probability Density Function (PDF)}

The PDF for a Gaussian distribution is defined as:
$$
P(x) = \frac{1}{\sigma\sqrt{2\pi}}e^{-\frac{(x-\mu)^2}{2\sigma^2}}
$$
\begin{code}
data GaussianPDF = GaussianPDF
    { prob :: Double
    , weight :: Double
    }
    
data GaussianPDFParams = GaussianPDFParams

instance Model GaussianPDFParams GaussianPDF where
    getparams _ = GaussianPDFParams
    
instance DefaultModel GaussianPDFParams GaussianPDF where
    defparams = GaussianPDFParams

instance Semigroup GaussianPDF where
    (<>) = undefined
    
instance Monoid GaussianPDF where
    mempty = GaussianPDF 0 0
    mappend = (<>)

instance HomTrainer GaussianPDFParams (Gaussian Double) GaussianPDF where
    train' GaussianPDFParams = undefined
    
\end{code}

\section{Misc}
\begin{code}

\end{code}

\section{Appendix: Symbol Manipulation}
We have defined |m2| so that:
\begin{align}
m_2 &= \sum_{i=1}^{n}(x_i - m_1)^2 \\
&= \sum_{i=1}^{n}(x_i^2 -2x_i m_1 +m_1^2)\\
&= \sum_{i=1}^{n}x_i^2 -2m_1 \sum_{i=1}^{n}x_i +\sum_{i=1}^{n}m_1^2\\
&= \sum_{i=1}^{n}x_i^2 -2m_1 n'm_1' +nm_1^2\\
\label{eq:m_2-1} &= \sum_{i=1}^{n}x_i^2 -nm_1^2\\
\label{eq:m_2-2} m_2+nm_1^2 &= \sum_{i=1}^{n}x_i^2
\end{align}

\end{document}