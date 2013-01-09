The categorical distribution is the main distribution for handling discrete data.  Sometimes it's even called the "discrete distribution."  You can think of it like a histogram.  

The HLearn-distributions package contains all the functions we need to manipulate categorical distributions.  Let's install it:

$ cabal install HLearn-distributions

Now, let's import our libraries:

>import Control.DeepSeq
>import HLearn.Algebra
>import HLearn.Gnuplot.Distributions
>import HLearn.Models.Distributions

Simon has a bag full of marbles.  The marbles might be red, green, blue, or white.  We can create a marble data type like:

>data Marble = Red | Green | Blue | White
>   deriving (Read,Show,Eq,Ord)

We can represent Simon's bag of marbles as a list:

>simonBag :: [Marble]
>simonBag = [Red, Red, Red, Green, Blue, Green, Red, Blue, Green, Green, Red, Red, Blue, Red, Red, Red, White]

And we can generate a categorical distribution of the marbles in Simon's bag by using the train function:

>simonDist = train simonBag :: Categorical Marble Double

We can plot the distribution by calling the function plot:

plot (plotFile "simonDist") simonDist

to get the same histogram we saw earlier.

In the HLearn library, every statistical model is generated from data using either train or train'.  Because these functions are overloaded, we must specify the type of simonDist so that the compiler knows which model to generate.  The Categorical type takes two parameters.  The first is the type of the discrete data (Marble).  The second is the type of the probability (Double).  We could easily create Categorical distributions with different types depending on the requirements for our application.  For example:

>stringDist = train (map show simonBag) :: Categorical String Float

So what's the difference between train and train'?

We use train' to generate models that require parameters (e.g. the kernel density estimate requires parameters).  If no parameters are required, or there are reasonable default parameters, then we can just use the train function by itself.  This is the case for the categorical distribution.

Now that we have a distribution, we can find some probabilities.  If Simon pulls a marble from the bag, what's the probability that it would Red?  We can use the pdf function to find out:

ghci> pdf cat_bag Red
0.5626
ghci> pdf cat_bag Blue
0.1876
ghci> pdf cat_bag Green
0.1876
ghci> pdf cat_bag Blue
6.26e-2

If we sum all the probabilities, as expected we would get 1:

ghci> sum $ map (pdf cat_bag) [Red,Green,Blue,White]
1.0

Due to rounding errors, you may not always get 1.  If you absolutely, positively, have to avoid rounding errors, you should use the type "Categorical Marble Rational".  Rationals are slower, but won't be subject to floating point errors.

This is just about all the functionality you would get in a "normal" stats package like R or NumPy.  But using Haskell's nice support for algebra, we can get some extra cool features.

***

First, let's talk about semigroups.  A semigroup is any data structure that has a binary operation (<>) that joins two of those data structures together.  The categorical distribution is a semigroup.

Don wants to play marbles with Simon, and he has his own bag.  Don's bag contains only red and blue marbles:

>donBag = [Red,Blue,Red,Blue,Red,Blue,Blue,Red,Blue,Blue]

We can train a categorical distribution on Don's bag in the same way we did earlier:

>donDist = train donBag :: Categorical Marble Double

In order to play marbles together, Don and Simon will have to add their bags together.

>bothBag = simonBag ++ donBag

Now, we have two options for training our distribution. First is the naive way, we can train the distribution directly on the combined bag:

>bothDist = train bothBag :: Categorical Marble Double

This is the way we would have to approach this problem in most statistical libraries.  But with HLearn, we have a more efficient alternative.  We can combine the trained distributions using the semigroup operation:

>bothDist' = simonDist <> donDist

This method is more efficient because it avoids repeating work we've already done.  Categorical's semigroup operation runs in time O(1), so no matter how big the bags are, we can calculate the distribution very quickly.  The naive method, in contrast, requires time O(n).  If our bags had millions or billions of marbles inside them, this would be a considerable savings!

We get another cool performance trick "for free" based on the fact that Categorical is a Semigroup: The function train can be automatically parallelized using the higher order function parallel.  I won't go into the details about how this works, but here's how you do it in practice.

Since Marble is our own data type, we must show the compiler how to resolve it down to "normal form."  This basically means that the data type is fully computed.  (If we were using a built in type, like a String, we could skip this step.)  This is fairly easy for a type as simply as Marble:

>instance NFData Marble where
>   rnf Red   = ()
>   rnf Blue  = ()
>   rnf Green = ()
>   rnf White = ()

Then, we can perform the parallel computation by:

>simonDist_par = parallel train simonBag :: Categorical Marble Double

Other languages require a programmer to manually create parallel versions of their functions.  But in Haskell, when working with a Semigroup, we get these parallel versions for free!  All we have to do is ask for it!

***

A monoid is a semigroup with an empty element, which is called "mempty" in Haskell.  It obeys the law that:

M <> mempty == mempty <> M == M

And it is easy to show that Categorical is also a monoid.  We get this empty element by training on any empty data set:

mempty = train ([] :: [Marble]) :: Categorical Marble Double

The HomTrainer type class requires that all its instances also be instances of Monoid.  This lets the compiler automatically derive "online trainers" for us.  An online trainer is one where we can add new data points to our statistical model without retraining it from scratch.

For example, we could use the function add1dp (stand for: add one data point) to add another white marble into Simon's bag:

>simonDistWhite = add1dp simonDist White 

This gives us another approach for our earlier problem of combining Simon and Don's bags.  We could use the function addBatch:

>bothDist'' = addBatch simonDist donBag

Because Categorical is a monoid, we maintain the property that:

bothDist == bothDist' == bothDist''

***

A group is a monoid with the additional property that all elements have an "inverse."  This lets us perform subtraction on Categorical.

Ed wants to play marbles too, but he doesn't have any of his own.  So Simon offers to give Ed some of from his own bag.  He gives Ed one of each color:

>edBag = [Red,Green,Blue,White,Green]

Now, if Simon draws a marble from his bag, what's the probability it will be blue?

To answer this question without algebra, we'd have to go back to the original data set, subtract the marbles Simon gave Ed, then retrain the distribution.  This is awkward and computationally expensive.  But if we take advantage of Categorical's group structure, we can just subtract directly from the distribution itself.  This makes more sense intuitively and is also easier computationally.

>simonDist2 = subBatch simonDist edBag

This is a shorthand notation for using the group operations directly:

>edDist = train edBag :: Categorical Marble Double
>simonDist2' = simonDist <> (inverse edDist)

***

Finally, an R-Module is a group with two additional properties.  First, it is abelian.  That means that <> is commutative.  So, for all a, b:

a <> b == b <> a

Second, the data type supports multiplication by a any element in the ring R.  You can think of a ring as any member of the Num type class in Haskell.

How is this useful?

Well, Ed---being the clever guy that he is---recently developed a marble copying machine.  That's right!  You just stick some marbles in on one end, and on the other end out pop 10 exact duplicates.  So Ed duplicates his new marbles and gives ALL of them back to Simon.  What's Simon's new distribution look like?

Again, the naive way to answer this question would be to retrain from scratch:

>duplicateBag = simonBag ++ (concat $ replicate 10 edBag)
>duplicateDist = train duplicateBag :: Categorical Marble Double

Slightly better is to take advantage of the Semigroup property, and just apply that over and over again:

>duplicateDist' = simonDist <> (foldl1 (<>) $ replicate 10 edDist)

But even better is to take advantage of the fact that Categorical is a module:

>duplicateDist'' = simonDist <> 10 .* edDist