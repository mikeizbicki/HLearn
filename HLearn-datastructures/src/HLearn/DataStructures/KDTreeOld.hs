{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HLearn.DataStructures.KDTree
    where
    
import Data.List
import Debug.Trace
import GHC.TypeLits
import Unsafe.Coerce
import Data.Bits
import Data.Word

import HLearn.Algebra
import HLearn.Models.Distributions.Categorical

import HLearn.DataStructures.BinarySearchTree

-------------------------------------------------------------------------------
-- data types

newtype KDPointBox a = KDPointBox a
    deriving (Read,Show,Eq)
    
instance (KDPoint a) => Ord (KDPointBox a) where
    compare (KDPointBox a) (KDPointBox b) = compare (bin a) (bin b)

newtype KDTree a = KDTree (BST (KDPointBox a))
    deriving (Read,Show,Semigroup,Monoid)

-------------------------------------------------------------------------------
-- kdtree classes

class (Eq kdp) => KDPoint kdp where
    bin :: kdp -> [Bool]
    
instance KDPoint [Double] where
    bin = binkd
    
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

-------------------------------------------------------------------------------
-- algebra


-------------------------------------------------------------------------------
-- model

instance Model (NoParams (KDTree a)) (KDTree a) where
    getparams model = NoParams

instance DefaultModel (NoParams (KDTree a)) (KDTree a) where
    defparams = NoParams
    
instance (KDPoint a) => HomTrainer (NoParams (KDTree a)) a (KDTree a) where
    train1dp' NoParams dp = KDTree $ train1dp $ KDPointBox dp

bindouble :: Word64 -> IO ()
bindouble d = go 63
    where  
        d' = unsafeCoerce d :: Int
        go (-1) = putStrLn ""
        go i = do
            if testBit d' i
                then putStr "1"
                else putStr "0"
            go (i-1)
