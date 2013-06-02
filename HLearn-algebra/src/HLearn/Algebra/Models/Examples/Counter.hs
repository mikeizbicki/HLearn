-- | Used for testing.  All it does is count the number of samples.

module HLearn.Algebra.Models.Examples.Counter
    where

import HLearn.Algebra   

data Counter sampletype = Counter {c::sampletype}
    deriving (Read,Show,Eq,Ord)

instance (Num sampletype) => Monoid (Counter sampletype) where
    mempty = Counter 0
    c1 `mappend` c2 = Counter $ c c1+c c2

instance (Num sampletype) => HomTrainer (Counter sampletype) where
    type Datapoint (Counter sampletype) = sampletype
    train1dp dp = Counter 1
