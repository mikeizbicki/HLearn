\documentclass{article}
%include polycode.fmt

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Document info
% 

\title{k-Dimensional Trees}

\author{
Michael Izbicki
}

% 
% Document
% 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code Header
%
\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module HLearn.DataStructures.KDTree
    where
    
import Data.List
import Debug.Trace
import GHC.TypeLits

import HLearn.Algebra
import HLearn.Models.Distributions.Categorical

\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Document Begins Here
%

\section{Data Types}

\begin{code}
class KDPoint kdp where
    bin :: kdp -> [Int]
    
binkd :: [Double] -> [Bool]
binkd = (concat . transpose . map bin1d)

bin1d :: Double -> [Bool]
bin1d x = 
    (x>0):                                  -- first digit specifies sign
    (abs x>1):                              -- second digit specifies if |x|>1
    (replicate x_powers True)               -- next, we repeat 1's the number of times in the exponent
    ++
    go lowerbound upperbound                -- finally, we "home in" on our value x via a recursive splitting strategy
    where
        x_powers = ceiling $ abs $ log10 $ abs x
        x' = 10**(abs $ log10 $ abs x)
        lowerbound = 10^(x_powers-1)
        upperbound = 10^(x_powers)
        
        go lower upper = --trace ("lower="++show lower++", split="++show split++", upper="++show upper) $
            if x'>split
                then True:(go split upper)
                else False:(go lower split)
            where split = (upper+lower)/2
        
log10 x = (log x)/(log 10)
\end{code}

We define the kD-Tree with the data type:
\begin{code}

data KDTree = KDTree
    {
    }

\end{code}

We define the singleton trainer as:
\begin{code}
\end{code}

We define the semigroup instance as:
\begin{code}
\end{code}
