
>{-# LANGUAGE DataKinds #-}
>import Control.Monad.Random
>import Data.Number.LogFloat
>import HLearn.Algebra
>import HLearn.Models.Markov.MarkovChain

One common use case for Markov chains is analyzing sequences of DNA.
DNA consists of a sequence of four base pairs, represented by the letters A, G, C, and T.
These are the data points, and a data set would be a list of these letters.
A very, very short strand of DNA might look something like:

>dna1 = "AGCTGCATAGCGCGATTACGATACG"

We can train a Markov chain from these data points with the command:

>mc1 = train dna1 :: MarkovChain 3 Char Double

There's a few interesting tidbits here.
Working from right to left, the Double specifies how we will be internally representing our probabilities;
the Char is the type of our data points;
and the number three is called the "order" of our Markov chain.
In a Markov chain, we make the assumption that any given data point is dependent only on some previous number of data points.

There are two main ways to use a Markov chain model.
The first is to generate new data.

>fakedna1 = evalRand  (replicateSamples 1000 mc1 "") $ mkStdGen 1

The second is to check 

Markov chains are different than most other models in the HLearn library because the order of our data points matters.
For example, we could shuffle around the data points inside dna1 to create the data set:

>dna2 = "AAAAAAACCCCCCGGGGGGGTTTTT"
>mc2 = train dna2 :: MarkovChain 3 Char Double

But this is clearly a much different data set.

>fakedna2 = evalRand  (replicateSamples 1000 mc2 "") $ mkStdGen 1