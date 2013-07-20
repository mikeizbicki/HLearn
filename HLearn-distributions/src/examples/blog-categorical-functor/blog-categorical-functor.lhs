Functors and monads are powerful design patterns used in Haskell.  In the context of machine learning, they let us preprocess our data asymptotically faster and in clever ways.  We won't be talking about the category theoretic foundations of these structures.  Instead, we'll focus just on how to use them in practice to analyze data.  

This post is a tutorial for the HLearn library, so we'll go through several examples involving the categorical distribution.  This distribution is somewhat awkwardly named for our purposes because it has nothing to do with category theory!  Instead, it is the most general distribution over categorical (i.e. non-numeric) data.  It's also very simple, which should make the examples a little easier to follow.  In later posts we'll look at more complex models, such as the kernel density estimator, multivariate distributions, and bayesian classifiers. 

-- setting up the problem 

Before we dive into how to use Functors and Monads, we need to set up our problem.  First, we import some modules:

> import Control.ConstraintKinds.Functor
> import Control.ConstraintKinds.Monad
> import Prelude hiding (Functor(..), Monad (..))

> import HLearn.Algebra
> import HLearn.Models.Distributions

For efficiency reasons we'll be using the Functor and Monad instances provided by the ConstraintKinds language extension.  You don't need to worry about this.  From the users perspective, everything works the same as normal. 
Notice that we won't be using Haskell's standard Functor and Monad classes.  That's because the standard type classes don't allow us to use constraints.  For efficiency reasons, we will need an Ord constraint.  The ConstraintKinds langauge extension lets us do that, and the ConstraintKinds package on hackage defines appropriate type classes.  If you're already familiar with Monads, everything should work just like you'd expect.

Now let's create a simple marble data type, and a small bag of marbles for our data set.

> data Marble = Red | Pink | Green | Blue | White
>   deriving (Read,Show,Eq,Ord)
>
> bagOfMarbles = [ Pink,Green,Red,Blue,Green,Red,Green,Pink,Blue,White ]

We train a categorical distribution on this data set using the train function:

> marblesDist = train bagOfMarbles :: Categorical Double Marble

If a stick my hand in the bag and draw a random marble, this distribution tells me the probability of drawing each color.

The type of Categorical distributions takes two parameters.  The first is the type of our probabilities, and the second is the type of our data points.

And plot our distribution:

ghci> plotDistribution (plotFile "marblesDist" $ PNG 400 300) marblesDist

-- Functors

Okay.  Now we're ready for the juicy bits.  We'll start by talking about the list functor, and use that to motivate the categorical distribution's functor.

A functor is a container that lets us "map" a function onto every element of the container.  Lists are a functor, and so we can apply a function to our data set.  Let's say instead of a distribution over the marble color, I want a distribution over the marble's weights.  I might have a function that associates a weight with each type of marble:

> marbleWeight :: Marble -> Int -- weight in grams
> marbleWeight Red = 3
> marbleWeight Pink = 2
> marbleWeight Green = 3
> marbleWeight Blue = 6
> marbleWeight White = 2

I can generate my new distribution by first transforming my data set, and then training on the result:

> weightsDist = train $ fmap marbleWeight bagOfMarbles :: Categorical Double Int

This is the standard way of preprocessing in machine learning.  But because the categorical distribution is also a functor, we can do better---we can just map the function directly onto the distribution:

> weightDist' = fmap marbleWeight marblesDist 

This is guaranteed to generate the same exact answer, but it is much faster.  It takes only constant time to preprocess this data, no matter how much data we have!

Let me put that another way.  A category theorist would say that the following diagram commutes:

--------

For another example, what if we don't want to differentiate between Red and Pink marbles?  The following function converts all the pink marbles to red.

> pink2red :: Marble -> Marble 
> pink2red Pink = Red
> pink2red dp   = dp

And we apply it to our distribution, and plot the results.

> nopinkDist = fmap pink2red marblesDist

ghci> plotDistribution (plotFile "nopinkDist" $ PNG 400 300) nopinkDist 

That's about all that a Functor can do by itself.  When we call fmap, we can only process on individual elements by themselves, and we can't change the number of elements in the resulting distribution.  Monads give us these powers.

-- Monads

Monads are functors with even more power.  They have two more functions.  The first is return.  It's type signature is

return :: (Ord datapoint) => datapoint -> Categorical prob datapoint

We've actually seen this function already in previous posts.  It's equivalent to the train1dp function found in the HomTrainer type class.  All it does is train a categorical distribution on a single data point.

The next function is called join.  It's a little bit trickier, and that's where all the magic lies.  It's type signature is:

join :: (Ord datapoint) => Categorical prob (Categorical prob datapoint) -> Categorical prob datapoint

As input, join takes a categorical distribution whose data points are other categorical distributions.  It then "flattens" the distribution into one that does not take other distributions as input.

-

For our first example, let's write a function that removes all the pink marbles from out data set.  

> forgetPink :: (Num prob) => Marble -> Categorical prob Marble
> forgetPink Pink = mempty
> forgetPink dp   = train1dp dp

> nopinkDist2 = join $ fmap forgetPink marblesDist

This idiom of join ( fmap ... ) is used a lot.  So we can use another function called bind that combines these steps for us.

(>>=) :: Categorical prob datapoint0 -> (datapoint0 -> Categorical prob datapoint1) -> Categorical prob datapoint1
dist >>= f = join $ fmap f dist

Under this notation, our new distribution is

> nopinkDist2' = marblesDist >>= forgetPink

-

Besides removing data points, we can also add new ones.  Let's double the importance of Pink marbles in our distribution:

> doublePink :: (Num prob) => Marble -> Categorical prob Marble
> doublePink Pink = 2 .* train1dp Pink
> doublePink dp   = train1dp dp

> doublepinkDist = marblesDist >>= doublePink

-

One common machine learning task is to factor in noise into our samples.

> addNoise :: (Fractional prob) => Marble -> Categorical prob Marble
> addNoise dp = 0.5 .* train1dp dp <> 0.1 .* train [ Red,Pink,Green,Blue,White ]

> noiseDist = marblesDist >>= addNoise

> rgNoise :: (Fractional prob) => Marble -> Categorical prob Marble
> rgNoise Red   = trainW [(0.9,Red),(0.1,Green)]
> rgNoise Green = trainW [(0.3,Red),(0.7,Green)]
> rgNoise dp    = train1dp dp 

> rgNoiseDist = marblesDist >>= rgNoise

-

Of course, we can also chain these functions together.  

> allDist = marblesDist >>= forgetPink >>= addNoise >>= rgNoise

> allDist2 = marblesDist >>= addNoise >>= rgNoise >>= forgetPink  

Finally, we can make this all a bit more readable using do notation.

 allDist' :: Categorical Double Marble
 allDist' = do
   dp <- train xs
   dp <- forgetPink dp
   dp <- addNoise dp
   dp <- rgNoise
   return dp

(Since we're using a custom Monad definition, this requires the RebindableSyntax extension to be enabled.)

-- Conclusion

Lot's of other mathematicians and programmers have known that probability distributions form a monad.  But this is the first application that I'm aware of to the learning aspect of distributions.

Subscribe to the RSS feed to stay tuned!
