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
import Data.Number.LogFloat (LogFloat)
import Debug.Trace
import GHC.TypeLits
import qualified Data.Vector as V

import HLearn.Algebra
import HLearn.Models.Distributions

-------------------------------------------------------------------------------
-- data types

data MarkovChain (order::Nat) datatype prob = MarkovChain
    { transitions :: !(V.Vector (Categorical [datatype] prob))
    , startchain  :: ![datatype]
    , endchain    :: ![datatype]
    }
    deriving (Show,Read,Eq,Ord)
    
instance (NFData datatype, NFData prob) => NFData (MarkovChain order datatype prob) where
    rnf mc = deepseq (transitions mc) 
           $ deepseq (startchain mc)
           $ rnf (endchain mc)

instance NFData LogFloat where
    rnf lf = seq lf ()

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
    mempty = MarkovChain 
        { transitions = V.replicate (order (undefined::MarkovChain order datatype prob)) mempty 
        , startchain = mempty 
        , endchain = mempty
        }
    mc1 `mappend` mc2 = MarkovChain
        { transitions = V.zipWith (<>) transitionVec (V.zipWith (<>) (transitions mc1) (transitions mc2))
        , startchain = (startchain mc1)++(take (order' - (length $ startchain mc1)) $ startchain mc2)
        , endchain = (takeLast (order' - (length $ endchain mc2)) $ endchain mc1)++(endchain mc2)
        }
        where
            order' = order mc1
            transitionVec = V.fromList $ [mempty] ++ (map train1dp newTransitions) ++ (replicate (order mc1 - length newTransitions) mempty)
            newTransitions = reverse . transL $ endchain mc1
            transL []     = []
            transL startL = 
                [ startL ++ end
                | end <- take (order' - length startL) $ tail $ inits $ startchain mc2
                ] ++ (transL $ tail startL)

takeLast :: Int -> [a] -> [a]
takeLast n xs = drop ((length xs)-n) xs

-------------------------------------------------------------------------------
-- training

instance (SingI order, Ord datatype, Num prob) => HomTrainer (MarkovChain order datatype prob) where
    type Datapoint (MarkovChain order datatype prob) = datatype
    
    train1dp dp = MarkovChain
        { transitions = V.fromList $ [train1dp [dp]] ++ (replicate (fromIntegral $ fromSing (sing :: Sing order)) mempty)
        , startchain = [dp]
        , endchain = [dp]
        }
 
---------------------------------------

{-instance (SingI order,Ord datatype, Num prob) =>HomTrainer (ChainGang order datatype prob) where
    type Datapoint (ChainGang order datatype prob) = [datatype]   

    train1dp dp = ChainGang $ transition $ (train dp :: MarkovChain order datatype prob)-}
 
-------------------------------------------------------------------------------
-- Markov

transitionProbability :: (Ord datatype, Ord prob, Fractional prob) => MarkovChain order datatype prob -> [datatype] -> prob
transitionProbability mc [] = 1
transitionProbability mc dp = pdf (transitions mc V.! (length dp-1)) dp

data TransitionProbabilities = TransitionProbabilities

instance 
    ( SingI order
    , Ord datatype
    , Fractional prob
    , Ord prob
    ) => Function TransitionProbabilities (MarkovChain order datatype prob,[datatype]) [prob] 
        where
    function _ (mc,dpL) = {-trace (show $ drop 1 $ reverse $ tails $ take order dpL) $-} prefix ++ remainder
        where
            order = fromIntegral $ fromSing (sing :: Sing order)
            
            prefix = map (transitionProbability mc) $ drop 1 $ reverse $ tails $ take order dpL
                                                  
            remainder = go $ dpL
            go xs = if length x < order
                then []
                else (transitionProbability mc x):(go $ tail xs)
                
                where
                    x = take order xs

data LogProbability = LogProbability
instance 
    ( SingI order
    , Ord datatype
    , Floating prob
    , Ord prob
    ) => Function LogProbability (MarkovChain order datatype prob,[datatype]) prob
        where
    function _ domain = sum $ map log $ function TransitionProbabilities domain

data GeometricProbability = GeometricProbability
instance 
    ( SingI order
    , Ord datatype
    , Floating prob
    , Ord prob
    ) => Function GeometricProbability (MarkovChain order datatype prob,[datatype]) prob
        where
    function _ domain = exp $ (sum $ map log probL) / (fromIntegral $ length probL)
        where
            probL = function TransitionProbabilities domain

-- class (Probabilistic m) => DatasetModel m where
--     type Dataset m
--     datasetProb :: m -> Dataset m -> Probability m
--     normProb :: m -> Dataset m -> Probability m
-- 
-- instance Probabilistic (MarkovChain order datatype prob) where
--     type Probability (MarkovChain order datatype prob) = prob
-- 
-- instance 
--     ( SingI order
--     , Ord datatype
--     , Floating prob
--     , Ord prob
--     ) => DatasetModel (MarkovChain order datatype prob) 
--         where
--     type Dataset (MarkovChain order datatype prob) = [datatype]
--     
--     datasetProb mc dp = prefixProbability*remainderProbability
--         where
--             order = fromIntegral $ fromSing (sing :: Sing order)
--             
--             prefixProbability = foldr (\a b -> if a>0 
--                                                   then a*b 
--                                                   else b) 1 $ map (pdf $ transition mc) $ tails $ take order dp
--                                                   
--             remainderProbability = go $ tail dp
--             
--             go xs = if length x < order
--                 then 1
--                 else (pdf (transition mc) x)*(go $ tail xs)
--                 
--                 where
--                     x = take order xs
--                 
--     normProb mc dp = (datasetProb mc dp)**(1/(fromIntegral $ length dp))
-- --     normProb mc dp = (datasetProb mc dp)/(0.5^(length dp))
--                 
--         -- pdf (ChainGang $ transition mc) dp
-- 
-- ---------------------------------------
-- 
-- instance Probabilistic (ChainGang order datatype prob) where
--     type Probability (ChainGang order datatype prob) = prob
-- 
-- -- instance (SingI order, Ord datatype, Num prob) => PDF (ChainGang order datatype prob) where
--     
-- 
-- -- instance (SingI order, Ord datatype, Num prob) => PDF (ChainGang order datatype prob) where
-- --     pdf (ChainGang cat) (x:xs) = 0
-- --         where
-- --             go orderlen rest = 
--     
-- -- class (Sampler mc) => Probability mc where
-- --     type Probability mc
-- --     prob :: mc -> [Sample mc] -> Probability mc
-- -- 
-- -- instance Probability (ChainGang order sampletype prob) where
-- --     type Probability (ChainGang order sampletype prob) = prob
-- 
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
            go count list = error $ "MarkovChain.sampleRight: count="++show count++"; list="++show list++"; matchingTransitions="++show matchingTransitions
            
            totalcount = sum $ map snd dist
            dist = map (\(str,n)-> (last str,n)) matchingTransitions
            matchingTransitions = filter (\(str,n) -> take (length dp) str == dp) $ dist2list $ transitions mc V.! (length dp)
            dp = takeLast (order mc-1) dpL

---------------------------------------

-- instance 
--     ( SingI order
--     , Ord datatype, Show datatype
--     , Random prob, Num prob, Ord prob, Show prob
--     ) => Sampler (ChainGang order datatype prob) 
--         where
--     type Sample (ChainGang order datatype prob) = datatype
--     
--     sampleRight (ChainGang dist) dpL = sampleRight (MarkovChain dist [] [] :: MarkovChain order datatype prob) dpL


-------------------------------------------------------------------------------
-- helpers

-- instance Random LogFloat where
--     randomR (lo,hi) g = (logFloat a,g')
--         where
--             (a,g') = randomR (fromLogFloat lo :: Double,fromLogFloat hi) g
--     random g = randomR (0::LogFloat,1) g

-------------------------------------------------------------------------------
-- testing

x=train "ATTATTATATATGCGCATGCATGCT" :: MarkovChain 3 Char Double
 
-- y = train
--     [ "AAAAAAAAAAAAAAAAAA"
--     , "CCCCCCCCCCCCCCCCCC"
--     , "TGTGTGTGTGTGTGGGTG"
--     ]
--     :: ChainGang 2 Char Double
--     