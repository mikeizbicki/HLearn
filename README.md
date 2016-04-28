# HLearn

<!--![](https://travis-ci.org/mikeizbicki/HLearn.svg)-->

HLearn is a high performance machine learning library written in [Haskell](http://haskell.org).
For example, it currently has the fastest nearest neighbor implementation for arbitrary metric spaces (see [this blog post](http://izbicki.me)).

HLearn is also a research project.
The research goal is to discover the "best possible" interface for machine learning.
This involves two competing demands:
The library should be as fast as low-level libraries written in C/C++/Fortran/Assembly;
but it should be as flexible as libraries written in high level languages like Python/R/Matlab.
[Julia](http://julialang.org/) is making amazing progress in this direction,
but HLearn is more ambitious.
In particular, HLearn's goal is to be *faster* than the low level languages and *more flexible* than the high level languages.

To achieve this goal, HLearn uses a very different interface than standard learning libraries.
The H in HLearn stands for three separate concepts that are fundamental to HLearn's design:

1. The H stands for [Haskell](http://haskell.org).
Machine learning is about estimating *functions* from data,
so it makes sense that a functional programming language would be well suited for machine learning.
But Functional programming languages are not widely used in machine learning because they traditionally lack strong support for the fast numerical computations required for learning algorithms.
HLearn uses the [SubHask](http://github.com/mikeizbicki/subhask) library to get this fast numeric support in Haskell.
The two libraries are being developed in tandem with each other.
<!--Languages like Agda/Coq/Idris provide more advanced type systems,-->
<!--but their compilers lack the support for real world optimizations needed for numerical applications.-->
<!--Haskell strikes a nice balance.-->

1. The H stands for [Homomorphisms](https://en.wikipedia.org/wiki/Homomorphism).
Homomorphisms are a fundamental concept in [abstract algebra](https://en.wikipedia.org/wiki/Abstract_algebra),
and HLearn exploits the algebraic structures inherrent in learning systems.
The following table gives a brief overview of what these structures give us:

    | Structure     | What we get                           |
    |:--------------|:--------------------------------------|
    | Monoid        | parallel batch training               |
    | Monoid        | online training                       |
    | Monoid        | fast cross-validation                 |
    | Abelian group | "untraining" of data points           |
    | Abelian group | more fast cross-validation            |
    | R-Module      | weighted data points                  |
    | Vector space  | fractionally weighted data points     |
    | Functor       | fast simple preprocessing of data     |
    | Monad         | fast complex preprocessing of data    |

1. The H stands for the [History monad](https://github.com/mikeizbicki/HLearn/blob/master/src/HLearn/History.hs).
One of the most difficult tasks of developing a new learning algorithm is debugging the optimization procedure.
There has previously been essentially no work on making this debugging process easier,
and the `History` monad tries to solve this problem.
It lets you thread debugging information throughout the optimization code *without modifying the original code*.
Furthermore, there is no runtime overhead associated with this technique.

The downside of HLearn's ambition is that it currently does not implement many of the popular machine learning techniques.

## More Documentation

Due to the rapid pace of development, HLearn's documentation is sparse.
That said, the [examples](https://github.com/mikeizbicki/HLearn/tree/master/examples) folder is a good place to start.
The haddock documentation embedded within the code is decent;
but unfortunately, hackage is unable to compile the haddocks because it uses an older version of GHC.

HLearn has several academic papers:

* ICML15 - [Faster Cover Trees](http://izbicki.me/public/papers/icml2015-faster-cover-trees.pdf)
* ICML13 - [Algebraic Classifiers: a generic approach to fast cross-validation, online training, and parallel training](http://izbicki.me/public/papers/icml2013-algebraic-classifiers.pdf)
* TFP13 - [HLearn: A Machine Learning Library for Haskell](http://izbicki.me/public/papers/tfp2013-hlearn-a-machine-learning-library-for-haskell.pdf)

There are also a number of blog posts on [my personal website](http://izbicki.me).
Unfortunately, they are mostly out of date with the latest version of HLearn.
They might help you understand some of the main concepts in HLearn, but the code they use won't work at all.

* [The categorical distribution's monoid/group/module Structure](http://izbicki.me/blog/the-categorical-distributions-algebraic-structure)
* [The categorical distribution's functor/monad structure](http://izbicki.me/blog/functors-and-monads-for-analyzing-data)
* [Markov Networks, monoids, and futurama](http://izbicki.me/blog/markov-networks-monoids-and-futurama)
* [Solving NP-complete problems with HLearn, and how to write your own HomTrainer instances](http://izbicki.me/public/papers/monoids-for-approximating-np-complete-problems.pdf)
* [Nuclear weapon statistics using monoids, groups, and modules](http://izbicki.me/blog/nuclear-weapon-statistics-using-monoids-groups-and-modules-in-haskell)
* [Gaussian distributions form a monoid](http://izbicki.me/blog/gausian-distributions-are-monoids)
* [HLearn cross-validates >400x faster than Weka](http://izbicki.me/blog/hlearn-cross-validates-400x-faster-than-weka)
* [HLearn's code is shorter and clearer than Weka's](http://izbicki.me/blog/hlearns-code-is-shorter-and-clearer-than-wekas)

## Contributing

<!--If you want to contribute, I'd be happy to help you get started.-->
I'd love to have you contribute, and I'd be happy to help you get started!
Just [create an issue](https://github.com/mikeizbicki/hlearn/issues) to let me know you're interested and we can work something out.
