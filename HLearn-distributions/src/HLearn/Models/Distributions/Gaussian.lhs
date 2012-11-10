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

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module HLearn.Models.Distributions.Gaussian2
    where
    
import Control.Monad
import Control.Monad.Random
import Data.Random.Normal
import Debug.Trace
import Numeric.SpecFunctions (logFactorial)
import Test.QuickCheck

import qualified Statistics.Distribution as D
import qualified Statistics.Distribution.Normal as N

import HLearn.Algebra
import HLearn.Models.Distributions.Common

fi :: (Integral a, Num b) => a -> b
fi=fromIntegral

data GaussianParams = GaussianParams

instance ModelParams GaussianParams

instance NFData GaussianParams where
    rnf params = ()

instance (NFData datapoint) => Model GaussianParams (Gaussian datapoint) where
    params model = GaussianParams

instance (NFData datapoint) => NFData (Gaussian datapoint) where
    rnf (Gaussian m1 m2 n) = seq (rnf m1) $ seq (rnf m2) (rnf n)

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
A Gaussian distribution is uniquely determined from its mean and variance.  In order to estimate these quantities from a sample, we must also know the number of data points in the sample.  Therefore, we define a Gaussian distribution as the following 3-tuple:

\begin{code}
data Gaussian datatype = Gaussian
        { n   :: {-# UNPACK #-} !Int    -- the number of samples seen
        , m1  :: !datatype              -- equal to the first moment (mean)
        , m2  :: !datatype              -- equal to the second moment (pop. variance) times n
        } 
    deriving (Show,Read)

\end{code}
There are several important notes to make about this data structure:
\begin{enumerate}
\item Normally a Gaussian distribution would have non-negative values for both the standard deviation and the number of samples.  We make no such assumption.  The normal operations we perform on a distribution (e.g. the pdf) will only be well defined in these circumstances, but the algebraic properties of the distribution require that all parameters can be negative.

\item We do not specify the base data type to store our estimates of the parameters.  Normally, we will use the |Double| type to do this, but if we have millions of estimated Gaussians, we may want to use the |Float| type instead.  Similarly, if we require extra precision we may use the |LogFloat| data type at the expense of performance.
\end{enumerate}

For convenience, we also define the following accessor functions to the sample mean and sample variance:
\begin{code}

-- mean :: (Floating datatype) => Gaussian datatype -> datatype
-- mean (Gaussian n m1 m2) = m1
-- 
-- var :: (Floating datatype) => Gaussian datatype -> datatype
-- var (Gaussian n m1 m2) = m2/(fi $ n-1)

\end{code}

\section{Algebra}

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
Notice that equation \ref{eq:m1'} is simply the weighted average of the two means.  This makes intuitive sense.  But there is a slight problem with this definition: When implemented on a computer with floating point arithmetic, we will get |Infinity| whenever |n'| is 0.  This would break the associativity of the semigroup operator, which is not allowed.  
%We solve this problem by handling it as a special case.  We can preserve associativity by defining |m1'| as:
%\begin{align}
%m_1' = 
%\left\{
%    \begin{array}{ll}
%      m_{1a}\frac{n_a}{n'} + m_{1b}\frac{n_b}{n'}   & \text{if |n' /= 0| } \\ 
%      m_{1a}n_a + m_{1b}n_b                         & \text{otherwise} \\
%    \end{array}
%  \right.
%\label{eq:m1'}
%\end{align}
%The pdf is undefined if |n < 1|, so we don't need to worry about if this definition makes sense from a probabilistic point of view.  We only need to worry about associativity.

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
&= m_{2a} + n_a m_{1a}^2 + m_{2b} + n_b m_{2b}^2 - n' m_1'^2
%&= m_{2a} + \frac{m_{1a}^2}{n_a} + m_{2b} + \frac{m_{2b}^2}{n_b} - \frac{m_1'^2}{n'}
%m_2' = m_{2a}+m_{2b}+\frac{n_an_b}{n'}(m_{1a}-m_{2a})^2
\end{align}
We combine equations \ref{eq:n'}, \ref{eq:m1'} and \ref{eq:m2'} to get the semigroup instance for the |Gaussian| data type:

\begin{code}

fakeGaussian = Gaussian 1 0 1 

instance (Floating datapoint) => Semigroup (Gaussian datapoint) where
    {-# INLINE (<>) #-}
    Gaussian na m1a m2a <> Gaussian nb m1b m2b = Gaussian n' m1' m2'
        where
            n'  = na+nb
            m1' = m1a*(fi na)/(fi n')+m1b*(fi nb)/(fi n')
            m2' = m2a + m2b + (m1a^2)*(fi na) + (m1b^2)*(fi nb) - (m1'^2)*(fi n')
            -- (m1a^2)/(fi na) + (m1b^2)/(fi nb) - (m1'^2)/(fi n')

mean :: (Floating datatype) => Gaussian datatype -> datatype
mean (Gaussian n m1 m2) = m1

var :: (Floating datatype) => Gaussian datatype -> datatype
var (Gaussian n m1 m2) = m2/(fi $ n-1)


-- instance (Floating datapoint) => Semigroup (Gaussian datapoint) where
--     {-# INLINE (<>) #-}
--     Gaussian na m1a m2a <> Gaussian nb m1b m2b = Gaussian n' m1' m2'
--         where
--             n'  = na+nb
--             m1' = if n' == 0
--                 then 0 -- m1a*(fi na)+m1b*(fi nb)
--                 else m1a*(fi na)/(fi n')+m1b*(fi nb)/(fi n')
--             m2' = m2a+m2b+ (fi na)*m1a^2+(fi nb)*m1b^2-(fi n')*m1'^2


data SafeGaussian = SafeGaussian {q :: Int, w1 :: Double, w2 :: Double, dummycount :: Int }
    deriving (Show,Eq)

instance Invertible SafeGaussian where
    {-# INLINE inverse #-}
    inverse (SafeGaussian n m1 m2 f) = SafeGaussian (-n) m1 (-m2) (-f)

instance HasIdentity SafeGaussian where
    {-# INLINE identity #-}
    identity = SafeGaussian 0 0 0 0 

trainSG :: Double -> SafeGaussian
trainSG x = SafeGaussian 1 x 0 0

-- It's possible there is a numerically optimal distribution to use here to maintain best accuracy, however, this seems to work pretty well in practice.
dummy :: SafeGaussian
dummy = SafeGaussian 2 2 2 1

instance Semigroup SafeGaussian where
    {-# INLINE (<>) #-}
    ga@(SafeGaussian na m1a m2a fa) <> gb@(SafeGaussian nb m1b m2b fb)
        | ga == inverse gb              = identity
        | n'==0 && fa >0                = (inverse dummy `merge` ga)<>gb
        | n'==0 && fb >0                = (inverse dummy `merge` ga)<>gb
        | n'==0                         = (dummy `merge` ga) `merge` gb
        | n' >0 && (fa >0 || fb>0)      = (ga `merge` gb) `merge` inverse dummy
        | otherwise                     = merge ga gb
            where n' = na+nb
        
merge :: SafeGaussian -> SafeGaussian -> SafeGaussian
merge ga@(SafeGaussian na m1a m2a fa) gb@(SafeGaussian nb m1b m2b fb) = SafeGaussian n' m1' m2' f'
    where
        n'  = na+nb
        m1' = m1a*(fi na)/(fi n')+m1b*(fi nb)/(fi n')
        m2' = m2a+m2b+ (fi $ na*nb)/(fi n')*(m1a - m1b)^2
        f' = fa+fb
        
\end{code}

\begin{theorem}
The Gaussian semigroup instance is valid because it satisfies the associativity law.
\end{theorem}
\begin{proof}
We must show that for any three |g_a, g_b, g_c :: Gaussian|, we can calculate:
\begin{spec}
g_l = (g_a <> g_b) <> g_c 
g_r = g_a <> (g_b <> g_c)
\end{spec}

Then we must show that |g_l| and |g_r| are equal by showing that each field is equals.  We can easily see this for |n| because:
$$
n_l = (n_a + n_b) + n_c = n_a + (n_b + n_c) = n_r
$$

For |m2|, we have:
\begin{align*}
m_{2l} &= m_{2a} + n_a m_{1a}^2 + m_{2b} + n_b m_{2b}^2 - n' m_1'^2
\end{align*}

%\begin{align*}
%%((m_{1a}, m_{2a}, n_a) \diamond (m_{1b}, m_{2b}, n_b)) \diamond (m_{1c}, m_{2c}, n_c)
%\end{align*}
\end{proof}

\todo{Finish proof}

\section{Monoid}

A monoid $M$ is just a semigroup that has a unique identity $\epsilon$ such that for all $x \in M$:
$$
x \diamond \epsilon = \epsilon \diamond x = x
$$
The identity for Gaussians can very simply be defined as:
\begin{code}
instance (Floating datapoint) => HasIdentity (Gaussian datapoint) where
    {-# INLINE identity #-}
    identity = Gaussian 0 0 0
\end{code}

\begin{theorem} 
The Gaussian data type has a valid identity.
\end{theorem}
\begin{proof}

\begin{spec}
identity <> Gaussian nb m1b m2b = Gaussian n' m1' m2'
\end{spec}
We easily show that the identity doesn't effect |n|:
$$
n' = 0 + n_b = n_b
$$
Next we show that the identity doesn't affect |m1|, but we must be more careful here due to our if statement.  If |nb /= 0|, then we get:
$$
m_1' = 0 \frac {0}{n_b} + m_{1b}\frac{n_b}{n_b} = m_{1b}
$$
And, if |nb == 0| we get:
$$
m_1' = 0 * 0 + m_{1b}*n_b = m_{1b}*0 = 0
$$
\end{proof}

Now, we must explicitly define a Monoid instance for our class as:
\begin{code}
instance (Floating datapoint) => Monoid (Gaussian datapoint) where
    {-# INLINE mempty #-}
    mempty = identity
    {-# INLINE mappend #-}
    mappend = (<>)
\end{code}

\section{Group}

A group is a monoid where every element x has an inverse such that:
$$
xx^{-1} = x^{-1}x = 1
$$

\begin{code}
instance (Floating datapoint) => Invertible (Gaussian datapoint) where
    {-# INLINE inverse #-}
    inverse (Gaussian n m1 m2) = Gaussian (-n) m1 (-m2)
\end{code}

\section{Misc}
\begin{code}
untrain :: (Eq datatype, Floating datatype) => Gaussian datatype -> [datatype]
untrain (Gaussian 0  0  0) = []
untrain (Gaussian 1 m1  0) = [m1]
untrain (Gaussian n m1 m2) = trace ("WARNING Gaussian.untrain: need better data input types") $ [m1+x,m1-x]
    where x=sqrt $ m2

instance (Floating datapoint) => SingletonTrainer GaussianParams datapoint (Gaussian datapoint) where
    {-# INLINE train #-}
    train GaussianParams x = Gaussian 1 x 0



-- g1=batch train GaussianParams [1,2,3::Double]
-- g2=batch train GaussianParams [3.414213562373095,0.5857864376269049::Double]
-- g2'=batch train GaussianParams [3.414213562373095,0.5857864376269049,10::Double]



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