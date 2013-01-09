The Bulletin of the Atomic Scientists tracks the nuclear capabilities of every country.  We're going to use their data to demonstrate Haskell's HLearn library and the usefulness of abstract algebra to statistics.  Specifically, we'll see that categorical distributions and kernel density estimates have monoid, group, and module algebraic structures.  We'll then take advantage of these structures to efficiently answer real-world statistical questions about nuclear war.  It'll be a WOPR!

Before we get into the math, we'll need to review the basics of nuclear politics.

The nuclear Non-Proliferation Treaty (NPT) is the main treaty governing nuclear weapons.  Basically, it says that there are five countries that are "allowed" to have nuclear weapons: the USA, UK, France, Russia, and China.  "Allowed" is in quotes because the treaty specifies that these countries must eventually get rid of their nuclear weapons at some future, unspecified date.  When another country, for example Iran, signs the NPT, they are agreeing to not develop nuclear weapons.  What they get in exchange is help from the 5 nuclear weapons states in developing their own civilian nuclear power programs.  (Iran has the legitimate complaint that Western countries are actively trying to stop its civilian nuclear program when they're supposed to be helping it, but that's a whole 'nother can of worms.)

Currently, every country is a member of the NPT except for Israel, Pakistan, India, and North Korea.  These four countries have their own nuclear weapons programs, and are thus not allowed to join the NPT.  Because their nuclear stockpiles are relatively small and undeployed, we're going to ignore them in this analysis for the sake of clarity.

** Programming preliminaries

Now that we have the political background, we're ready to do some statistics.  First, let's import our libraries:

>import Control.Lens
>import Data.Csv
>import qualified Data.Vector as V
>import qualified Data.ByteString.Lazy.Char8  as BS
> 
>import HLearn.Algebra
>import HLearn.Models.Distributions
>import HLearn.Gnuplot.Distributions

Next, we load our data from nukes.csv using the Cassava package (http://hackage.haskell.org/package/cassava):

>main = do
>   Right rawdata <- fmap (fmap V.toList . decode True) $ BS.readFile "nukes-list.csv" 
>        :: IO (Either String [(String, String, String, Int)])

And we'll use the Lens package to parse the CSV file into a series of variables containing just the values we want:

>   let list_usa    = fmap (\row -> row^._4) $ filter (\row -> (row^._1)=="USA"   ) rawdata
>   let list_uk     = fmap (\row -> row^._4) $ filter (\row -> (row^._1)=="UK"    ) rawdata 
>   let list_france = fmap (\row -> row^._4) $ filter (\row -> (row^._1)=="France") rawdata 
>   let list_russia = fmap (\row -> row^._4) $ filter (\row -> (row^._1)=="Russia") rawdata 
>   let list_china  = fmap (\row -> row^._4) $ filter (\row -> (row^._1)=="China" ) rawdata 

NOTE: To understand the above code requires knowledge of the Cassava and Lens packages.  For the rest of this article, it's not important that you understand how it works.  All you need to understand is what each of these list_country variables looks like.  So let's print one:

>   putStrLn $ "List of American nuclear weapons = " ++ show list_usa

List of American nuclear weapons = fromList [335,335,335,335,335,335,335,335,335,335  ...  1200,1200,1200,1200,1200]

If we want to know how many weapons are in the American arsenal, we can take the length of the list:

>   putStrLn $ "Number of American weapons = " ++ show (length list_usa)
>   putStrLn $ "Explosive power of American weapons = " ++ show (sum list_usa)

>   putStrLn $ "USA = " ++ show (length list_usa) ++ ","++ show (sum list_usa)
>   putStrLn $ "UK = " ++ show (length list_uk) ++ ","++ show (sum list_uk)
>   putStrLn $ "France = " ++ show (length list_france) ++ ","++ show (sum list_france)
>   putStrLn $ "Russia = " ++ show (length list_russia) ++ ","++ show (sum list_russia)
>   putStrLn $ "China = " ++ show (length list_china) ++ ","++ show (sum list_china)

We get that there are 1951 American nuclear weapons.  

To get the total number of weapons in the world, we concatenate every country's list of weapons and find the length of that:

>   let list_all = list_usa ++ list_uk ++ list_france ++ list_russia ++ list_china
>   putStrLn $ "Number of nukes in the whole world = " ++ show (length list_all)

And we get that there are 5079 nuclear weapons.

Now let's do some algebra!

** Monoids and groups

In a previous post, we saw that the Gaussian distribution forms a group.  This means that is has all the properties of a monoid---an empty element (mempty) that represents the distribution trained on no data, and a binary operation (mappend) that merges two distributions together---plus an inverse.  This inverse lets us "subtract" two Gaussians from each other.

It turns out that many other distributions also have this group property.  For example, the categorical distribution is used for measuring discrete data.  We train our categorical distribution like:

>   let cat_usa     = train list_usa     :: Categorical Int Double
>   let cat_uk      = train list_uk      :: Categorical Int Double
>   let cat_france  = train list_france  :: Categorical Int Double
>   let cat_russia  = train list_russia  :: Categorical Int Double
>   let cat_china   = train list_china   :: Categorical Int Double

Now, because training the categorical distribution is a group homomorphism, we can train a distribution over all nukes by either:

>   let cat_allA = train list_all :: Categorical Int Double
        
or
        
>   let cat_allB = cat_usa <> cat_uk <> cat_france <> cat_russia <> cat_china
        
and we will get the same result no matter what.  Since we've already done the calculations for each  of the the countries already, method B will be more efficient---it won't have to repeat work we've already done.

But nuclear war planners don't particularly care about this complete list of nuclear weapons.  It contains nuclear weapons that have to be dropped from bombers.  These free fall nukes are rarely useful in a nuclear war because they can't survive a surprise "first strike" attack.  That is, if the US launched a surprise attack on the Russians, we would be able to destroy all their bombers and so wouldn't be vulnerable to them.

What we really care about is the "survivable" nuclear weapons.

>   let list_bomber = fmap (\row -> row^._4) $ filter (\row -> (row^._2)=="Bomber") rawdata 

Then, we use our group inverse to calculate the global survivable arsenal:

>   let cat_bomber = train list_bomber :: Categorical Int Double
>   let cat_survivable = cat_allB <> (inverse cat_bomber)

Notice that we calculated this value indirectly---there was no possible way to combine our variables above to generate this value without using the inverse!  This is the power of groups in statistics.

** More distributions

The categorical distribution is not sufficient to accurately describe the distribution of nuclear weapons.  This is because we don't actually know the yield of a given warhead.  Like all things, it has some manufacturing tolerances that we must consider.  For example, if we detonate a 300 kt warhead, the actual explosion might be 275 kt, 350 kt, or the bomb might even "fizzle out" and have almost a 0kt explosion.

We'll model this by using a kernel density estimator (KDE).  

>   let kdeparams = KDEParams
>        { bandwidth    = Constant 20
>        , samplePoints = genSamplePoints
>               0       -- minimum
>               4000    -- maximum
>               4000    -- number of samples
>        , kernel       = KernelBox Gaussian
>        } :: KDEParams Double

Now, we'll train kernel density estimates on our data:

        
>   let kde_usa     = train' kdeparams list_usa      :: KDE Double
>   let kde_uk      = train' kdeparams list_uk       :: KDE Double
>   let kde_france  = train' kdeparams list_france   :: KDE Double
>   let kde_russia  = train' kdeparams list_russia   :: KDE Double
>   let kde_china   = train' kdeparams list_china    :: KDE Double

>   let kde_all = kde_usa <> kde_uk <> kde_france <> kde_russia <> kde_china

The KDE is a powerful technique capable of capturing many different distributions.  The draw back is that it is computationally expensive, especially when a large number of sample points are used.  Fortunately, all computations in the HLearn library are easily parallelizable by applying the higher order function 'parallel'.

We can calculate the full KDE from scratch in parallel like this:

>   let list_double_all = map fromIntegral list_all :: [Double]
>   let kde_all_parA = (parallel (train' kdeparams)) list_double_all :: KDE Double
        
or we can perform a parallel reduction on the kde's for each country like this:

>   let kde_all_parB = (parallel reduce) [kde_usa, kde_uk, kde_france, kde_russia, kde_china]

Now, we'll calculate and plot the parallel version:

>   plotDistribution (genPlotParams "kde_all" kde_all) kde_all
>--   plotDistribution (genPlotParams "kde_usa" kde_usa) kde_usa

The parallel computation takes about 16 seconds on my Core2 Duo laptop running on 2 processors, whereas the serial computation takes about 28 seconds.

This is a considerable speedup, but we can still do better.  It turns out that there is a homomorphism from the Categorical distribution to the KDE.

>   let kde_fromcat_all = cat_allB $> kdeparams
>   plotDistribution (genPlotParams "kde_fromcat_all" kde_fromcat_all) kde_fromcat_all

This computation takes less than a second and gets the exact same result as the much more expensive computations above.

Why does this work?  Well, the categorical distribution is a structure called the "free module" in disguise.

** Modules, and the Free Module

Modules (like groups, but unlike monoids) have not seen much attention from functional programmers.  This is a shame, because they're quite handy.  It turns out they will increase our performance dramatically.

It's not super important to know the formal definition of an R-module, but here it is anyways: An R-module is a group with an additional property: it can be "multiplied" by any element of the ring R.  This is a generalization of vector spaces because R need only be a ring instead of a field.  (Rings do not necessarily have multiplicative inverses.)

Here's an example.

Let's say I have a vector:

>   let vec = [1,2,3,4,5] :: [Int]
        
I can perform scalar multiplication on that vector like this:

>   let vec2 = 3 .* vec

which as you matlab users might expect results in:

[3,6,9,12,15]

Our next example is the "free module."  A "free" structure is one that obeys only the axioms of the structure and nothing else.  Functional programmers are very familiar with the free monoid, i.e. the list data type.  The free Z-module is like a beefed up list.  Instead of just storing the elements in a list, it also stores the number of times that element occurred.  This lets us greatly reduce the memory required to store a repetitive data set.

In HLearn, we represent the free module over a ring r with the data type:

:: FreeMod r a

where a is the type of elements to be stored in the free module.  We can convert our lists into free modules using the function list2module like this:

>   let module_usa      = list2module list_usa
>   let module_uk       = list2module list_uk
>   let module_france   = list2module list_france
>   let module_russia   = list2module list_russia
>   let module_china    = list2module list_china
        
Because modules are also groups, we can combine them like so:

>   let module_allA = module_usa <> module_uk <> module_france <> module_russia <> module_china

or, we could train them from scratch:

>   let module_allB = list2module list_all
        
Because generating a free module is a homomorphism, we have that:

module_allA == module_allB

But what does the FreeModule actually look like? Let's print it to find out:

>   print module_usa

FreeMod (fromList [(100,768),(150,200),(300,250),(335,249),(340,50),(455,384),(1200,50)])

This is much more compact!  So this is the take away: The Free Module makes repetitive data sets easier to work with.

** Module distributions

The Categorical distribution and the KDE both have this module structure.  This gives us two cool properties for free.

First, we can train these distributions directly from the Free Module.  That is:

>   let cat_module_all = train module_allB :: Categorical Int Double
>   let kde_module_all = train' kdeparams module_allB :: KDE Double
        
and now, 

cat_mod_all == cat_all
kde_mod_all == kde_all == kde_cat_all

Second, if a distribution is a module, we can weight the importance of our data points.  Let's say we're a North Korean general, and we're planning our nuclear strategy.  The US and North Korea have a very strained relationship in the nuclear department.  It is much more likely that the US will try to nuke the DPRK than China will.  And modules let us model this!

>   let threats_dprk = 20 .* kde_usa
>                   <> 10 .* kde_uk
>                   <> 5  .* kde_france
>                   <> 2  .* kde_russia
>                   <> 1  .* kde_china
>
>   plotDistribution (genPlotParams "threats_dprk" threats_dprk) threats_dprk

Or, if we're an American general, then we might say:

>   let threats_usa = 1 .* kde_russia 
>                  <> 5 .* kde_china
>
>   plotDistribution (genPlotParams "threats_usa" threats_usa) threats_usa

Oh, and since we've already calculated all of the kde_country variables before, these computations take virtually no time at all to compute.

** Bonus Homework Questions

If you want to try out the HLearn library, here's two questions you can try to answer:

1) What's the probability of a submarine launched ballistic missile (SLBM) having a 

In our next post, we'll go into more detail about the mathematical plumbing that makes all this possible.  Then we'll start talking about Bayesian classification and full-on machine learning.

>   print "done!"