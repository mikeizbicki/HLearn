This is the first in a series of posts about the HLearn library for haskell that I've been working on for the past few months.  The idea of the library is to show that abstract algebra---specifically monoids, groups, and homomorphisms---are particularly useful concepts in machine learning/data mining algorithms.  By framing our learning algorithms in these terms, we can get online and parallel versions of the algorithm for free.  (We can also perform cross-validation asymptotically faster than with the standard algorithm, but we'll save that for a later post.)

To show how to get online and parallel algorithms for free, we'll use the example of a [Gaussian distribution](https://en.wikipedia.org/wiki/Normal_distribution).  Gaussians are ubiquitous in learning algorithms because they accurately describe most data, but more importantly they are easy to work with.  They are easy to work with because they are fully described by their mean and variance, and these parameters are easy to estimate.

In this post we'll start with examples of using the HLearn library to train Gaussians, then we'll look at the math underlying these examples, and finally we'll see just how fast this technique really is in practice.

First examples:

Let's load the library:

> import HLearn.Algebra
> import HLearn.Models.Distributions.Gaussian

Now let's create some data to work with.  For simplicity's sake, we'll use a made up data set of how much money people make.  Every entry represents one person making that salary.  (In a real world data set, there would obviously be many more data points.)

> gradstudents = [15e3,25e3,18e3,17e3,9e3] :: [Double]
> teachers = [40e3,35e3,89e3,50e3,52e3,97e3]:: [Double]
> doctors = [130e3,105e3,250e3]:: [Double]

In order to train a Gaussian distribution from the data, we simply use the train function, like so:

> gradstudents_gaussian = train gradstudents :: Gaussian Double
> teachers_gaussian = train teachers :: Gaussian Double
> doctors_gaussian = train doctors :: Gaussian Double

Now for the interesting bits.  We start by showing that the Gaussian is a semigroup.  A semigroup is any data structure that has an associative binary operation called (<>).  Basically, we can think of (<>) as "adding" the two structures together.  For the Haskellers, semigroups are just monoids without a mempty function.

So how do we use this?  Well, what if we decide we want a Gaussian over everyone's salaries?  Using the traditional approach, we'd have to recompute this from scratch.

> all_salaries = concat [gradstudents,teachers,doctors]
> traditional_all_gaussian = train all_salaries :: Gaussian Double

But this repeats work we've already done.  On a real world data set with millions or billions of samples, this would be very slow.  Better would be to merge the Gaussians we've already trained into one final Gaussian.  We can do that with the semigroup operation (<>):

> semigroup_all_gaussian = gradstudents_gaussian <> teachers_gaussian <> doctors_gaussian

Now, 

<<< traditional_all_gaussian == semigroup_all_gaussian

The coolest part about this is that the semigroup operation takes time O(1), no matter how much data we've trained the Gaussians on!  The naive approach takes time O(n).

Technically, this is true because the train function for the Gaussian is a semigroup homomorphism.  What makes the HLearn library special is that all such training functions are homomorphisms.

Now, in order for the Gaussian to be a monoid, it must be a semigroup with an identity.  The identity for a Gaussian is easy to define---simply train on the empty list!

> gaussian_identity = train ([]::[Double]) :: Gaussian Double

But we've still got one more trick up our sleeves: the Gaussian distribution is not just a monoid, but also a group.  Groups appear all the time in abstract algebra, but they haven't seen much attention in functional programming for some reason.  Well groups are simple; they're just monoids with an inverse.  This inverse lets us do "subtraction" on our data structures.

So back to our salary example.  Lets say we've calculated all our salaries, but we've realized that including grad students in the salary calculations was a mistake.  They're not real people after all.  In a normal library, we would have to recalculate everything from scratch again, excluding the grad students:

> nograds = concat [teachers,doctors]
> traditional_nograds_gaussian = train nograds :: Gaussian Double

But as we've already discussed, this takes a lot of time.  We can use the `inverse` function to do this same operation in constant time:

> group_nograds_gaussian = semigroup_all_gaussian <> (inverse gradstudents_gaussian)

And now, 

<<< traditional_nograds_gaussian == group_nograds_gaussian

Again, we've converted an operation that would have taken time O(n) into one that takes time O(1).  Can't get much better than that!

Next the math:

Finally performance:

I'll be