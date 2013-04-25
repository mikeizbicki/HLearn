{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HLearn.Models.Markov.MarkovChain
    where
    
import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Data.List
import Debug.Trace
import GHC.TypeLits

import HLearn.Algebra
import HLearn.Models.Distributions

-------------------------------------------------------------------------------
-- data types

data MarkovChain (order::Nat) datatype prob = MarkovChain
    { transition :: Categorical [datatype] prob
    , startchain :: [datatype]
    , endchain :: [datatype]
    }
    deriving (Show,Read,Eq,Ord)
    
order :: forall n datatype prob. (SingI n) => MarkovChain n datatype prob -> Int
order _ = fromInteger $ fromSing (sing :: Sing n)

newtype ChainGang (order::Nat) datatype prob = ChainGang (Categorical [datatype] prob)
    deriving (Show,Read,Eq,Ord,Monoid,Group)

-------------------------------------------------------------------------------
-- Algebra

instance 
    ( SingI order
    , Ord datatype
    , Num prob
    ) => Monoid (MarkovChain order datatype prob)
        where
    mempty  = MarkovChain mempty mempty mempty
    mc1 `mappend` mc2 = MarkovChain
        { transition = (transition mc1) <> (transition mc2) <> mergetransitions
        , startchain = (startchain mc1)++(take (order' - (length $ startchain mc1)) $ startchain mc2)
        , endchain = (takeLast (order' - (length $ endchain mc2)) $ endchain mc1)++(endchain mc2)
        }
        where
            order' = order mc1
            mergetransitions = train $ transL $ endchain mc1
            transL []     = []
            transL startL = 
                [ startL ++ end
                | end <- take (order' - length startL) $ tail $ inits $ startchain mc2
                ] ++ (transL $ tail startL)

takeLast :: Int -> [a] -> [a]
takeLast n xs = drop ((length xs)-n) xs

---------------------------------------

-- instance Semigroup (ChainGang order datatype prob) where
--     cg1 <> cg2 = MarkovChain
--         { transition = transition cg1 <> transition cg2
--         , startchain = undefined

-------------------------------------------------------------------------------
-- training

instance (SingI order, Ord datatype, Num prob) => HomTrainer (MarkovChain order datatype prob) where
    type Datapoint (MarkovChain order datatype prob) = datatype
    
    train1dp dp = MarkovChain
        { transition = train1dp [dp]
        , startchain = [dp]
        , endchain = [dp]
        }
 
---------------------------------------

instance (SingI order,Ord datatype, Num prob) =>HomTrainer (ChainGang order datatype prob) where
    type Datapoint (ChainGang order datatype prob) = [datatype]   

    train1dp dp = ChainGang $ transition $ (train dp :: MarkovChain order datatype prob)
 
-------------------------------------------------------------------------------
-- Markov

instance (SingI order, Ord datatype, Num prob) => Probabilistic (ChainGang order datatype prob) where
    type Probability (ChainGang order datatype prob) = prob

instance (SingI order, Ord datatype, Num prob) => PDF (ChainGang order datatype prob) where
--     pdf (ChainGang cat) (x:xs) = 0
--         where
--             go orderlen rest = 
    
-- class (Sampler mc) => Probability mc where
--     type Probability mc
--     prob :: mc -> [Sample mc] -> Probability mc
-- 
-- instance Probability (ChainGang order sampletype prob) where
--     type Probability (ChainGang order sampletype prob) = prob

---------------------------------------

class Sampler t where
    type Sample t
    sampleRight :: (RandomGen g) => t -> [Sample t] -> Rand g (Sample t)

    replicateSamples :: (RandomGen g) => Int -> t -> [Sample t] -> Rand g [Sample t]
    replicateSamples 0 mc dpL = return []
    replicateSamples n mc dpL = do
        x <- sampleRight mc dpL
        xs <- replicateSamples (n-1) mc (dpL++[x])
        return $ x:xs

instance 
    ( SingI order
    , Ord datatype, Show datatype
    , Random prob, Num prob, Ord prob, Show prob
    ) => Sampler (MarkovChain order datatype prob) 
        where
    type Sample (MarkovChain order datatype prob) = datatype
    sampleRight mc dpL = do 
        index <- getRandomR (1,totalcount)
        return $ go index dist
        where
            go count ((dp,n):xs)
                | count <= n = dp
                | otherwise  = go (count-n) xs
            
            totalcount = sum $ map snd dist
            dist = map (\(str,n)-> (last str,n)) matchingTransitions
            matchingTransitions = filter (\(str,n) -> length str==order mc && take (min (order mc-1) (length dp)) str==dp) $ dist2list $ transition mc
            dp = takeLast (order mc-1) dpL

---------------------------------------

instance 
    ( SingI order
    , Ord datatype, Show datatype
    , Random prob, Num prob, Ord prob, Show prob
    ) => Sampler (ChainGang order datatype prob) 
        where
    type Sample (ChainGang order datatype prob) = datatype
    
    sampleRight (ChainGang dist) dpL = sampleRight (MarkovChain dist [] [] :: MarkovChain order datatype prob) dpL


-------------------------------------------------------------------------------
-- testing

x=train "ATTATTATATATGCGCATGCATGCT" :: MarkovChain 3 Char Double
 
y = train
    [ "AAAAAAAAAAAAAAAAAA"
    , "CCCCCCCCCCCCCCCCCC"
    , "TGTGTGTGTGTGTGGGTG"
    ]
    :: ChainGang 2 Char Double
    