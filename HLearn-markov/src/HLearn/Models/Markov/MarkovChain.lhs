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

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module HLearn.Models.Markov.MarkovChain
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

We define the Markov chain with the data type:
\begin{code}

data MarkovChain (order::Nat) datatype = MarkovChain
    { transition :: Categorical [datatype]
    , startchain :: [datatype]
    , endchain :: [datatype]
    }
    deriving (Show,Read,Eq)
    
order :: forall n. forall datatype. SingI n => MarkovChain n datatype -> Int
order _ = fromInteger $ fromSing (sing :: Sing n)
\end{code}

We define the singleton trainer as:
\begin{code}
trainMC :: forall order. forall datatype. (SingI order, NFData datatype) => datatype -> MarkovChain order datatype
trainMC dp = MarkovChain
    { transition = train CategoricalParams [dp]
    , startchain = chain
    , endchain = chain
    }
    where
        chain = if (fromSing (sing :: Sing order)) == 0
            then []
            else [dp]
\end{code}

We define the semigroup instance as:
\begin{code}
instance (SingI order, NFData datatype, Label datatype) => Semigroup (MarkovChain order datatype) where
    mc1 <> mc2 = MarkovChain
        { transition = (transition mc1) <> (transition mc2) <> mergetransitions
        , startchain = (startchain mc1)++(take (order' - (length $ startchain mc1)) $ startchain mc2)
        , endchain = (takeLast (order' - (length $ endchain mc2)) $ endchain mc1)++(endchain mc2)
        }
        where
            order' = order mc1
            mergetransitions = batch (train CategoricalParams) $ transL $ endchain mc1
            transL []     = []
            transL startL = 
                [ startL ++ end
                | end <- take (order' - length startL) $ tail $ inits $ startchain mc2
                ] ++ (transL $ tail startL)

takeLast :: Int -> [a] -> [a]
takeLast n xs = drop ((length xs)-n) xs

x=batch trainMC "ab" :: MarkovChain 3 Char
 
instance HasIdentity (MarkovChain n datatype) where
    identity = MarkovChain 
        { transition = identity
        , startchain = []
        , endchain = []
        }
        
instance 
    ( Semigroup (MarkovChain order datatype)
    ) => Monoid (MarkovChain order datatype)
    where
        mempty  = identity
        mappend = (<>)
\end{code}
