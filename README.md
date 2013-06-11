#Homomorphic Learning

HLearn is a suite of libraries for interpretting machine learning models according to their algebraic structure.  Every structure has associated algorithms useful for learning.  when we show that a model is an instance of a particular structure, we get the associated algorithms "for free."

| Algebraic structure | What we get |
|:-----------|:------------|
| Monoid | parallel batch training |
| Monoid     | online training    |
| Monoid     |  fast cross-validation  |
| Abelian group | "untraining" of data points |
| Abelian group | more fast cross-validation |
| R-Module    | weighted data points |
| Vector space | fractionally weighted data points |

This interpretation of machine learning is somewhat limitting in that not all models have obvious algebraic structure.  But many important models do.  Currently implemented models include:

* **Univariate distributions**: exponential, log-normal, normal, kernel density estimator, binomial, categorical, geometric, poisson

* **Multivariate distributions**:  normal, categorical, subset of markov networks

* **Classifiers**: naive bayes, full bayes, decision stumps, decision trees, k-nearest neighbor (naive and kd-tree), perceptron, bagging, boosting (sort of)

* **Other**: markov chains, k-centers, partition

Note: not all of these are included in the latest hackage releases simply because I haven't had time to finish a major refactoring

## Example: normal distribution

Every model in HLearn is trained from a data set using the function `train`.  The type signature specifies which model we're training.

> let dataset =  [1,2,3,4,5,6]

> let dist = train dataset :: Normal Double

We can train in parallel using the higher order function `parallel`.  The GHC run time automatically takes advantage of multiple cores on your compuer.  If you have 4 cores, then run time is 4x faster.

> let dist' = parallel train dataset :: Normal Double

We can also train in online mode.  This is where you add data points to an already existing model using either the function `add1dp` or `addBatch`.

> let dist_online1 = add1dp dist 7

> let dist_online2 = addBatch dist [7,8,9,10]

Finally, once we've trained a data point, we can do all the normal operations on it we would expect.  One common operation on distributions is evaluating the probability density function.  We do this with the `pdf` function.

> pdf dist 10

For more details on why the Normal distribution has algebraic structure and what we can do with it, see the blog post [Normal distributions form a monoid and why machine learning experts should care](http://izbicki.me/blog/gausian-distributions-are-monoids).

## More documentation

There are three main sources of documentation.  First, there are a number of tutorials on my personal blog.  These provide the most detail and are geared towards the beginner.  They're probably the easiest way to get started.  Next, there are two papers about the internals of the HLearn library.  They are a good resource for understanding the theory behind why the library works.  Finally, there's the hackage documentation.  

Tutorial links:

* [Markov Networks, Monoids, and Futurama](http://izbicki.me/blog/markov-networks-monoids-and-futurama)
* [The Categorical Distribution's Algebraic Structure](http://izbicki.me/blog/the-categorical-distributions-algebraic-structure)
* [Nuclear Weapon Statistics Using Monoids, Groups, and Modules in Haskell](http://izbicki.me/blog/nuclear-weapon-statistics-using-monoids-groups-and-modules-in-haskell)
* [Gaussian distributions form a monoid](http://izbicki.me/blog/gausian-distributions-are-monoids)

Paper links:

* TFP13 - [HLearn: A Machine Learning Library for Haskell](http://izbicki.me/public/papers/tfp2013-hlearn-a-machine-learning-library-for-haskell.pdf)
* ICML13 - [Algebraic Classifiers: a generic approach to fast cross-validation, online training, and parallel training](http://izbicki.me/public/papers/icml2013-algebraic-classifiers.pdf)

Hackage links:

* [HLearn-algebra](http://hackage.haskell.org/package/HLearn-algebra)
* [HLearn-distributions](http://hackage.haskell.org/package/HLearn-distributions)
* [HLearn-classification](http://hackage.haskell.org/package/HLearn-classification)

## Future directions

HLearn is under active development.  At present, it is primarily a research tool.  This means that the interfaces may change significantly in the future (but will definitely follow the [PVP](http://www.haskell.org/haskellwiki/Package_versioning_policy)).  I'm hoping HLearn will eventually become a stable package that will make it easy to incorporate machine learning techniques into Haskell programs.

Current development is focused in two areas.  First, implementing new models and their algebraic structures.  Many unimplemented models have "trivial" algebraic structure.  But for many successful models it is unknown whether they can have interesting structure.  The second area is investigating new structures.  Many models have Functor/Applicative/Monoid structure (or in some strict sense *almost* have these structures) and I'm working on how to exploit these structures.

**Any comments / questions / pull requests are greatly appreciated!**
