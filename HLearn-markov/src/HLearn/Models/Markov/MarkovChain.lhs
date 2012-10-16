\documentclass{article}
%include polycode.fmt

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Document info
% 

\title{Algebraic Properties of Markov Chains}

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

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module HLearn.Models.Markov.MarkovChain
    where
    
import Data.List
import GHC.TypeLits

import HLearn.Algebra
import HLearn.Models.Distributions.Categorical

\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Document Begins Here
%

\section{Data Types}

We define the Markov chain with the data type:
\begin{code}

data MarkovChain {-(order::Nat)-} datatype = MarkovChain
    { transition :: Categorical [datatype]
    , startchain :: [datatype]
    , endchain :: [datatype]
    , order :: Int
    }
    deriving (Show,Read,Eq)
\end{code}

We define the singleton trainer as:
\begin{code}
trainMC :: (NFData datatype) => Int -> datatype -> MarkovChain datatype
trainMC order dp = MarkovChain
    { transition = train CategoricalParams [dp]
    , startchain = [dp]
    , endchain = [dp]
    , order = order
    }
\end{code}

We define the semigroup instance as:
\begin{code}
instance (NFData datatype, Label datatype) => Semigroup (MarkovChain datatype) where
    mc1 <> mc2 = MarkovChain
        { transition = (transition mc1) <> (transition mc2) <> mergetransitions
        , startchain = if (length $ startchain mc1) >= order'
            then startchain mc1
            else (startchain mc1)++(take (order' - (length $ startchain mc1)) $ startchain mc2)
        , endchain = if (length $ endchain mc2) >= order'
            then endchain mc2
            else (endchain mc2)++(take (order' - (length $ endchain mc2)) $ endchain mc1)
        , order = order'
        }
        where
            order' = if (order mc1) == (order mc2)
                then order mc1
                else error "MarkovChain.(<>): non-matching orders"
            mergetransitions = batch train CategoricalParams
                [ x++y
                | x <- drop 0 $ init $ tails $ endchain mc1
                , y <- take 1 $ tail $ inits $ startchain mc2
                ]
\end{code}