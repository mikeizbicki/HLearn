module HLearn.Models.Classifiers.HTree
    where

import Debug.Trace
import qualified Data.Map as Map

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Models.Classification
import HLearn.Models.Classifiers.Bayes

newtype HTreeParams distparams = HTreeParams (BayesParams distparams)
    deriving (Read,Show,Eq,Ord)
    
newtype HTree dist label prob = HTree (Bayes dist label prob)
    deriving (Read,Show,Eq,Ord)
    
-- instance (Ord prob, Show (dist prob), Show label, Untup (dist prob) a b, Show b) => ProbabilityClassifier (HTree dist label prob) dp label prob where
--     probabilityClassify (HTree (SGJust (Bayes' labelDist attrDist))) dp = 
--         trace (show $ untup $ map snd $ Map.toList attrDist) $ undefined

debug (HTree (SGJust (Bayes' labelDist attrDist))) dp = map snd $ Map.toList attrDist

htree = HTree model

-- class TupTranspose tup where
--     tuptrans :: tup -> 

-- tuptrans [a:::b] -> [a]:::[b]
-- tuptrans [] = []
-- tuptrans x:xs = 
    
class Untup tup a b where
    untup :: [tup] -> a:::[b]
    
instance (Untup a a' b') => Untup (a:::b) (a':::[b']) b where
    untup xs = (untup a) ::: b
        where (a:::b) = untupzip xs
            
instance Untup (a:::b) [a] b where
    untup = untupzip
    
  
kindswap :: ((a:::.b) c) -> (a c):::(b c)
kindswap (a:::.b) = (a:::b)
{-instance (Untup (a c) (a' c) (b' c)) => Untup ((a:::.b) c) ((a' c):::[b' c]) (b c) where
    untup xs = (untup a) ::: b
        where (a:::.b) = untupzip xs
              
instance Untup ((a:::.b) c) [a c] (b c) where
    untup = untupzip-}
            
untupzip    :: [(a:::b)] -> ([a]:::[b])
{-# INLINE untupzip #-}
untupzip    =  foldr (\(a:::b) ~(as:::bs) -> ((a:as):::(b:bs))) ([]:::[])