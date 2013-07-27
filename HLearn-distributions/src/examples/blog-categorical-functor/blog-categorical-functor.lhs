>{-# LANGUAGE RebindableSyntax #-}
>{-# LANGUAGE TemplateHaskell #-}

Functors and monads are powerful design patterns used in Haskell. They give us two cool tricks for analyzing data.  First, we can “preprocess” data after we’ve already trained a model.  The model will be automatically updated to reflect the changes.  Second, this whole process happens asymptotically faster than the standard method of preprocessing.  In some cases, you can do it in constant time no matter how many data points you have!

This post focuses on how to use functors and monads in practice with the HLearn library.  We won’t talk about their category theoretic foundations; instead, we’ll go through several examples involving the categorical distribution. This distribution is somewhat awkwardly named for our purposes because it has nothing to do with category theory—it is the most general distribution over non-numeric (i.e. categorical) data. It’s simplicity should make the examples a little easier to follow.

HLearn also provides functor and monad instances for more complex models, such as the kernel density estimator, multivariate distributions, and bayesian classifiers.  All the general principles you see here apply equally to those models as well.
Setting up the problem

Before we dive into using functors and monads, we need to set up our code and create some data. Let’s import our modules:

> import Control.ConstraintKinds
> import Prelude hiding (Functor(..), Monad (..))
>
> import HLearn.Algebra
> import HLearn.Models.Distributions

For efficiency reasons we’ll be using the Functor and Monad instances provided by the ConstraintKinds package and language extension. From the user’s perspective, everything works the same as normal monads.

Now let’s create a simple marble data type, and a small bag of marbles for our data set.

> data Marble = Red | Pink | Green | Blue | White
>   deriving (Read,Show,Eq,Ord)
>
> bagOfMarbles = [ Pink,Green,Red,Blue,Green,Red,Green,Pink,Blue,White ]

This is a very small data set just to make things easy to visualize. Everything we’ll talk about works just as well on arbitrarily large data sets.

We train a categorical distribution on this data set using the train function:

> marblesDist = train bagOfMarbles :: Categorical Double Marble

The Categorical type takes two parameters. The first is the type of our probabilities, and the second is the type of our data points.  If you stick your hand into the bag and draw a random marble, this distribution tells you the probability of drawing each color.

Let’s plot our distribution:

ghci> plotDistribution (plotFile "marblesDist" $ PNG 400 300) marblesDist

marblesDist-mod
Functors

Okay. Now we’re ready for the juicy bits. We’ll start by talking about the list functor.  This will motivate the advantages of the categorical distribution functor.

A functor is a container that lets us “map” a function onto every element of the container.  Lists are a functor, and so we can apply a function to our data set using the map function.

map :: (a -> b) -> [a] -> [b]

Example 1:

Let’s say instead of a distribution over the marble color, I want a distribution over the marble’s weights. I might have a function that associates a weight with each type of marble:

> marbleWeight :: Marble -> Int -- weight in grams
> marbleWeight Red   = 3
> marbleWeight Pink  = 2
> marbleWeight Green = 3
> marbleWeight Blue  = 6
> marbleWeight White = 2

I can generate my new distribution by first transforming my data set, and then training on the result.  Notice that the type of our distribution has changed.  It is no longer a categorical distribution over marbles; it’s a distribution over ints.

> weightsDist = train $ fmap marbleWeight bagOfMarbles :: Categorical Double Int

ghci> plotDistribution (plotFile "weightsDist" $ PNG 400 300) weightsDist

weightsDist-mod

This is the standard way of preprocessing data. But we can do better because the categorical distribution is also a functor. We have a function fmap that we can apply directly to the distribution:

fmap :: (Ord dp0, Ord dp1) => (dp0 -> dp1) -> Categorical prob dp0 -> Categorical prob dp1

We can use fmap to apply the marbleWeights function directly to the distribution:

> weightDist' = fmap marbleWeight marblesDist

This is guaranteed to generate the same exact answer, but it is much faster. It takes only constant time to call Categorical’s fmap, no matter how much data we have!

Let me put that another way. Below is a diagram showing the two possible ways to generate a model on a preprocessed data set.  Every arrow represents a function application.

blog-categorical-functor

The normal way to preprocess data is to take the bottom left path.  But because our model is a functor, the top right path becomes available.  This path is better because it has the shorter run time.

Furthermore, let’s say we want to experiment with k different preprocessing functions.  The standard method will take \Theta(nk) time, whereas using the categorical functor takes time \Theta(n k).

Example 2:

For another example, what if we don’t want to differentiate between red and pink marbles? The following function converts all the pink marbles to red.

> pink2red :: Marble -> Marble 
> pink2red Pink = Red
> pink2red dp   = dp

Let’s apply it to our distribution, and plot the results:

> nopinkDist = fmap pink2red marblesDist

ghci> plotDistribution (plotFile "nopinkDist" $ PNG 400 300) nopinkDist

nopinkDist-mod

Example 3:



That’s about all that a Functor can do by itself. When we call fmap, we can only process individual data points.  We can’t change the number of points in the resulting distribution or do other complex processing. Monads give us this power.
Monads

Monads are functors with two more functions. The first is called return. It’s type signature is

return :: (Ord dp) => dp -> Categorical prob dp

We’ve actually seen this function already in previous posts. It’s equivalent to the train1dp function found in the HomTrainer type class. All it does is train a categorical distribution on a single data point.

The next function is called join. It’s a little bit trickier, and it’s where all the magic lies. It’s type signature is:

join :: (Ord dp) => Categorical prob (Categorical prob dp) -> Categorical prob dp

As input, join takes a categorical distribution whose data points are other categorical distributions. It then “flattens” the distribution into one that does not take other distributions as input.

Example 1

For our first example, let’s write a function that removes all the pink marbles from out data set.  Whenever we encounter a pink marble, we’ll want to replace it with an empty categorical distribution.  If the marble is not pink, we’ll create a singleton distribution from it.

> forgetPink :: (Num prob) => Marble -> Categorical prob Marble
> forgetPink Pink = mempty
> forgetPink dp   = train1dp dp
>
> nopinkDist2 = join $ fmap forgetPink marblesDist

ghci> plotDistribution (plotFile "nopinkDist2" $ PNG 400 300) nopinkDist2

nopinkDist2-mod

This idiom of join ( fmap … ) is used a lot. So we can use another function called bind (>>=) that combines these steps for us.

(>>=) :: Categorical prob dp0 -> (dp0 -> Categorical prob dp1) -> Categorical prob dp1
dist >>= f = join $ fmap f dist

Under this notation, our new distribution can be defined as:

> nopinkDist2' = marblesDist >>= forgetPink

Example 2

Besides removing data points, we can also add new ones. Let’s double the importance of pink marbles in our distribution:

> doublePink :: (Num prob) => Marble -> Categorical prob Marble
> doublePink Pink = 2 .* train1dp Pink
> doublePink dp   = train1dp dp
>
> doublepinkDist = marblesDist >>= doublePink

ghci> plotDistribution (plotFile "doublepinkDist" $ PNG 400 300) doublepinkDist

doublepinkDist-mod

Example 3

One common machine learning task is to factor noise into our sampling process.  Mistakes are often made when collecting data.  Adding noise let’s us consider the likelihood of those mistakes on our final distribution.  In this example, we’ll add a uniform noise to every sample.

Notice that we are using fractional weights for our noise, and that the weights are carefully adjusted so that the total number of marbles in the distribution still sums to one.  We don’t want to add or remove marbles while creating noise.

> addNoise :: (Fractional prob) => Marble -> Categorical prob Marble
> addNoise dp = 0.5 .* train1dp dp <> 0.1 .* train [ Red,Pink,Green,Blue,White ]
>
> noiseDist = marblesDist >>= addNoise

ghci> plotDistribution (plotFile "noiseDist" $ PNG 400 300) noiseDist

noiseDist-mod

Adding uniform noise just made all our probabilities closer together.

Example 4

Of course, the amount of noise we add to each sample doesn’t have to be the same everywhere. If I suffer from red-green color blindness, then I might use this as my noise function:

> rgNoise :: Marble -> Categorical Double Marble
> rgNoise Red   = trainW [(0.7,Red),(0.3,Green)]
> rgNoise Green = trainW [(0.1,Red),(0.9,Green)]
> rgNoise dp    = train1dp dp 
>
> rgNoiseDist = marblesDist >>= rgNoise

ghci> plotDistribution (plotFile "rgNoiseDist" $ PNG 400 300) rgNoiseDist

rgNoiseDist-mod

Because of my color blindness, the probability of drawing a red marble from the bag is now higher than drawing a green marble.  This is despite the fact that we had more green marbles in our training data.

===========

Example 4b

In the real world, we can never know exactly how much error we have in the samples.  Luckily, we can try to learn it by conducting a second experiment.

To determine our true error rate, we'll send off 50 marbles to a lab that has a spectrometer.  The spectrometer is very accurate, but it's also very expensive.  That's why we can only send 50 marbles.  Then, we'll also use a much cheaper method to determine the color of those same marbles.  We'll force undergrads to do it :)  This will give us a good estimate on the error rate of having undergrads determine the colors.

Here are our results.  The first parameter

> greenMarbles = [Green,Red,Green,Red,Green,Red,Red,Green,Green,Green]
> redMarbles = [Red,Green,Red,Green,Red,Red,Green,Green,Red,Red]

> greenDist = train greenMarbles  :: Categorical Double Marble
> redDist = train redMarbles :: Categorical Double Marble

> rgNoise2 :: Marble -> Categorical Double Marble
> rgNoise2 Green = greenDist /. numdp greenDist 
> rgNoise2 Red   = redDist /. numdp redDist 
> rgNoise2 dp    = train1dp dp 

> rgNoiseDist2  = marblesDist >>= rgNoise2

ghci> plotDistribution (plotFile "rgNoiseDist2" $ PNG 400 300) rgNoiseDist2

===========

Example 5

Finally, we can chain our preprocessing functions together in arbitrary ways.

> allDist = marblesDist >>= forgetPink >>= addNoise >>= rgNoise

ghci> plotDistribution (plotFile "allDist" $ PNG 400 300) allDist

allDist-mod

But wait!  Where’d that pink come from?  Wasn’t the call to forgetPink supposed to remove it?  The answer is that we did remove it, but then we added it back in with our noise functions.  When using monadic functions, we must be careful about the order we apply them in.  This is no different than when regular functions.

Here’s another distribution created from those same functions in a different order:

> allDist2 = marblesDist >>= addNoise >>= rgNoise >>= forgetPink

ghci> plotDistribution (plotFile "allDist" $ PNG 400 300) allDist2

allDist2-mod

We can also use Haskell’s do notation to accomplish the same exact thing:


> allDist2' :: Categorical Double Marble
> allDist2' = do 
>    dp <- marblesDist -- train bagOfMarbles
>    dp <- addNoise dp
>    dp <- rgNoise dp
>    dp <- forgetPink dp
>    return dp

(Since we’re using a custom Monad definition, this requires the RebindableSyntax extension.)
Conclusion

===========

Example 6

Do notation gives us a convenient way to preprocess multiple data sets into a single data set.  Let's create two new data sets and their corresponding distributions for us to work with:

> bag1 = [Red,Pink,Green,Blue,White]
> bag2 = [Red,Blue,White]
>
> bag1dist = train bag1 :: Categorical Double Marble
> bag2dist = train bag2 :: Categorical Double Marble

Now, we'll create a third data set that is a weighted combination of bag1 and bag2.  We will do this by repeated sampling.  On every iteration, with a 20% probability we'll sample from bag1, and with an 80% probability we'll sample from bag2.  Imperative pseudo-code for this algorithm is:

let comboDist be an empty distribution
loop until desired accuracy achieved:
    let r be a random number from 0 to 1
    if r > 0.2:
        sample dp1 from bag1
        add dp1 to comboDist
    else:
        sample dp2 from bag2
        add dp2 to comboDist

This sampling procedure will obviously not give us an exact answer.  But since the categorical distribution supports weighted data points, we can use this simpler pseudo-code to generate an exact answer:

let comboDist be an empty distribution
foreach datapoint dp1 in bag1:
    foreach datapoint dp2 in bag2:
        add dp1 with weight 0.2 to comboDist
        add dp2 with weight 0.8 to comboDist

Using do notation, we can express this as:

> comboDist :: Categorical Double Marble
> comboDist = do
>   dp1 <- bag1dist
>   dp2 <- bag2dist
>   trainW [(0.2,dp1),(0.8,dp2)]

And because the Categorical functor takes constant time, calculating comboDist also takes constant time.  The imperative algorithm would take time $latex \Theta (|bag1|*|bag2|)$.

When combining multiple distributions this way, the number of data points in our final distribution will be the product of the number of data points in the initial distributions:

ghci> numdp combination
15


---

Example 7

Finally, arbitrarily complex preprocessing functions can be written using Haskell's do notation.  And remember, no matter how complicated these functions are, their run time never depends on the number of elements in the initial data set.

This function adds uniform sampling noise to our bagOfMarbles, but only on those marbles that are also contained in bag2 above.

> comboDist2 :: Categorical Double Marble
> comboDist2 = do
>   dp1 <- marblesDist
>   dp2 <- bag2dist
>   if dp1==dp2
>       then addNoise dp1
>       else return dp1


==============

This application of monads to machine learning generalizes the monad used in probabilistic functional programming.  PFP focused on manipulating already known distributions, not training them from data.  Also, if you enjoy this kind of thing, you might be interested in the n-category cafe discussion on category theory in machine learning from a few years back.

In future posts, we’ll look at functors and monads for continuous distributions, multivariate distributions, and classifiers.  Things will get really interesting when we start working with multiple models at the same time.

Subscribe to the RSS feed to stay tuned!
