In this post, we're going to look at how to manipulate multivariate distributions in the HLearn library.
All of these distributions form monoids, and training them is a monoid homomorphism.

We start, as usual, with our language extensions and imports:

>{-# LANGUAGE DataKinds #-}
>{-# LANGUAGE TypeFamilies #-}
>{-# LANGUAGE TemplateHaskell #-}

>import Control.DeepSeq
>import HLearn.Algebra
>import HLearn.Models.Distributions

Next, we'll create a Character data type.
It will have a record for everything we might want to know about a character.

>data Character = Character
>   { _name      :: String
>   , _species   :: String
>   , _job       :: Job
>   , _isGood    :: Maybe Bool
>   , _age       :: Double -- in years
>   , _height    :: Double -- in feet
>   , _weight    :: Double -- in pounds
>   }
>   deriving (Read,Show,Eq,Ord)
>
>data Job = Manager | Crew | Henchman | Other
>   deriving (Read,Show,Eq,Ord)
>
>instance NFData Character where
>   rnf _ = ()
>
>instance NFData Job where
>   rnf j = seq j ()

Now, in order for our library to be able to interpret the Character type, we will need some "type level lenses."
You don't have to know anything about what this means in order to use the library.
All you have to do is call the template haskell function:

>makeTypeLenses ''Character

This function creates a bunch of data types and type classes for us.
For example, the type TH_age was created. 
This type gives us a convenient way to access the ages in our distribution, as we'll see shortly.
If you're curious what's going on under the hood here, then checkout the haddock documentation.

That's all we have to do to define our data types.
Now, we're ready to create a data set and start training.
Here's a small data set of our good friends at Planet Express.

>planetExpress = 
>   [ Character "Philip J. Fry"         "human" Crew     (Just True) 1026   5.8 195
>   , Character "Turanga Leela"         "alien" Crew     (Just True) 43     5.9 170
>   , Character "Professor Farnsworth"  "human" Manager  (Just True) 85     5.5 160
>   , Character "Hermes"                "human" Manager  (Just True) 36     5.3 210
>   , Character "Amy Wong"              "human" Other    (Just True) 21     5.4 140
>   , Character "Zoidberg"              "alien" Other    (Just True) 212    5.8 225
>   , Character "Cubert Farnsworth"     "human" Other    (Just True) 8      4.3 135
>   ]

Now, we're ready to train a distribution from this data.
Here's how we would train a distribution where every element is independent of every other element:

>dist1 = train planetExpress :: Multivariate Character
>  '[ Independent Categorical '[String,String,Job,Maybe Bool]
>   , Independent Normal '[Double,Double,Double]
>   ]
>   Double

As you can see, the Multivariate distribution takes three type parameters.
The first parameter is the type of our data point, in this case Character.
The second parameter describes the dependency structure of our distribution.
We'll get into more detail on this in the second half of this post, but for now, just observe that we're using a type level list and that all our attributes are independent of each other.
Finally, the third parameter is the type we will use to store our probabilities.

What can we do with this distribution?
One simple task we can do is to find "marginal distributions."
The marginal distribution is the distribution of a certain attribute ignoring all the other attributes.
For example, let's say I want a distribution of the species that work at planet express.
I can get this by:

>dist1a = getMargin TH_species dist1

Notice that we specified which attribute we're taking the marginal of by using the type level lens TH_species.
This guarantees for us 

ghci> :t dist1a
dist1a :: Categorical String Double

Now, if I wanted a distribution of the weights of the employees, I can get that by:

>dist1b = getMargin TH_weight dist1

And the type of this distribution is:

ghci> :t dist1b
dist1b :: Normal Double

I can easily plot these marginal distributions with the "plotDistribution" function, like so:

ghci> plotDistribution (plotFile "dist1a") dist1a
ghci> plotDistribution (plotFile "dist1b") dist1b

But wait!  I accidentally forgot to include Bender in the planetExpress data set!  What can I do?

Well, because all of the distributions form monoids, they implement the "HomTrainer" type class.
This class uses the monoid properties of our distribution to automatically derive an online training function for us.
Let's look at its type:

ghci> :t add1dp
add1dp :: HomTrainer model => model -> Datapoint model -> model

It's pretty simple.  
The function takes a model and adds the data point associated with that model.
It returns the model we would have gotten if the data point had been in our original data set.
Again, because our distributions form monoids, the compiler derived an efficient and exact online training algorithm for us automatically.

So let's create a new distribution that considers bender:

>bender = Character "Bender Rodriguez" "robot" Crew (Just True) 44 6.1 612
>dist1' = add1dp dist1 bender

And plot our new marginals:

ghci> plotDistribution (plotFile "dist1-withbender-species") $ getMargin TH_species dist1'
ghci> plotDistribution (plotFile "dist1-withbender-weight") $ getMargin TH_weight dist1'

That's cool, but our original distribution isn't very interesting.
We want to be able to say that certain attributes are dependent on other attributes.
For example, we've already seen that robots are much heavier than organic lifeforms, and are throwing off our statistics.
The HLearn library supports a small subset of Markov Networks for expressing these dependencies.

Normally, we represent Markov Networks as graphs with undirected edges.
Every attribute in our distribution is a node, and every dependence between attributes is an edge.
We can draw this graph with the command:

ghci> drawNetwork "dist1-network" dist1

As expected,there are no dependencies in the distribution, so no lines in the graph.
Let's create a more interesting distribution and plot its Markov network.

>dist2 = train planetExpress :: Multivariate Character
>  '[ Ignore '[String]
>   , MultiCategorical '[String]
>   , Independent Categorical '[Job,Maybe Bool]
>   , Independent Normal '[Double,Double,Double]
>   ]
>   Double

ghci> plotNetwork "dist2-network" dist2

We can also make everything fully dependent on everything else:

>dist3 = train planetExpress :: Multivariate Character
>  '[ Ignore '[String]
>   , Independent Categorical '[String]
>   , MultiCategorical '[Job]
>   , Independent Categorical '[Maybe Bool]
>   , Independent Normal '[Double,Double,Double]
>   ]
>   Double

>distb = train planetExpress :: Multivariate Character
>  '[ Ignore '[String]
>   , MultiCategorical '[String,Job,Maybe Bool]
>   , Dependent MultiNormal '[Double,Double,Double]
>   ]
>   Double

ghci> plotNetwork "dist3-network" dist3

Undoubtably, this is in always going to be the case---everything always has a slight influence on everything else.  
Unfortunately, it is not easy in practice to model these fully dependent distributions.
We need approximately \Theta(2^(v+e)) data points to accurately train a distribution, where n is the number of nodes in our graph and e is the number of edges in our network.
Thus, by selecting that two attributes are independent of each other, we can greatly reduce the amount of data we need to train an accurate distribution.

I think what makes the most sense in our case is to say that our age is probably relatively independent of our weight and height.

>dist4 = train planetExpress :: Multivariate Character
>  '[ Ignore '[String]
>   , MultiCategorical '[String,Job]
>   , Independent Categorical '[Maybe Bool]
>   , Independent Normal '[Double]
>   , Dependent MultiNormal '[Double,Double]
>   ]
>   Double

ghci> plotNetwork "dist4-network" dist4

We still don't have enough data to to train this network, so let's create some more.
We start by creating a type for Markov network called FuturamaDist.

>type FuturamaDist = Multivariate Character
>  '[ Ignore '[String]
>   , MultiCategorical '[String,Job]
>   , Independent Categorical '[Maybe Bool]
>   , Independent Normal '[Double]
>   , Dependent MultiNormal '[Double,Double]
>   ]
>   Double

Next, we train some more distribubtions of this type.

>momCorporation = 
>   [ Character "Mom"                   "human" Manager  (Just False) 100 5.5 130
>   , Character "Walt"                  "human" Henchman (Just False) 22  6.1 170
>   , Character "Larry"                 "human" Henchman (Just False) 18  5.9 180
>   , Character "Igner"                 "human" Henchman (Just False) 15  5.8 175
>   ]
>momDist = train momCorporation :: FuturamaDist

>spaceForce = 
>   [ Character "Zapp Brannigan"        "human" Manager  (Nothing)   45  6.0 230
>   , Character "Kif Kroker"            "alien" Crew     (Just True) 113 4.5 120
>   ]
>spaceDist = train spaceForce :: FuturamaDist

>robots = 
>   [ bender
>   , Character "Calculon"              "robot" Other    (Nothing)    123  6.8 650
>   , Character "The Crushinator"       "robot" Other    (Nothing)    45   8.0 4500
>   , Character "Clamps"                "robot" Henchman (Just False) 134  5.8 330
>   , Character "DonBot"                "robot" Manager  (Just False) 178  5.8 520
>   , Character "HedonismBot"           "robot" Other    (Just False) 69   4.3 1200
>   , Character "Roberto"               "robot" Other    (Just False) 77   5.9 250
>   , Character "Robot Devil"           "robot" Other    (Just False) 895 6.0 280
>   , Character "Robot Santa"           "robot" Other    (Just False) 488 6.3 950
>   ]
>robotDist = train robots :: FuturamaDist

Now we're going to take advantage of the monoid structure of our multivariate distributions.

>futuramaDist = dist4 <> momDist <> spaceDist <> robotDist

Now, let's ask some questions.
If I pick a character at random, what's the probability that they're a good guy?

ghci> plotDistribution (plotFile ("goodguy")) $ getMargin TH_isGood futuramaDist

But what if I only want to pick from those characters that are humans, or those characters that are robots?
I would have to condition on those attributes, and we can do that like:

ghci> plotDistribution (plotFile ("goodguy-human")) $ getMargin TH_isGood $ condition TH_species "human" futuramaDist
ghci> plotDistribution (plotFile ("goodguy-robot")) $ getMargin TH_isGood $ condition TH_species "robot" futuramaDist

Clearly futurama is anti-robot and thinks that robots are going to destroy the world!
If only we could figure out the average age of these evil robots, we would know how much longer we have until this distopian future.

ghci> mean $ getMargin TH_age $ condition TH_isGood (Just False) $ condition TH_species "robot" futuramaDist 
273.0769230769231

Notice that conditioning a distribution is a commutative operation.
That means we can condition in any order and still get the exact same results.

ghci> mean $ getMargin TH_age $ condition TH_species "robot" $ condition TH_isGood (Just False) futuramaDist 
273.0769230769231