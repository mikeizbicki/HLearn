# bench/allknn

This folder contains scripts that run all nearest neighbor searches in a number of libraries.
For the most part, the scripts are very bare-bones.
For example, they don't even output the results.

To run the scripts, you'll obviously first need to install the libraries.
The `/install` folder in this repo contains scripts for installing all of these libraries.
With all the libraries installed, just call the `runtest.sh` script with a single parameter that is the dataset to test on.

The table below provides a brief description of the libraries compared against.

| Library           | Description               |
|-------------------|---------------------------|
| [FLANN](http://www.cs.ubc.ca/research/flann/) | The Fast Library for Approximate Nearest Neighbor queries.  This C++ library is the standard method for nearest neighbor in Matlab/Octave and the [OpenCV](http://opencv.org) computer vision toolkit.  |
| [Julia](http://julia.org) | A popular new language designed from the ground up for fast data processing.  Julia supports faster nearest neighbor queries using the [KDTrees.jl](https://github.com/JuliaGeometry/KDTrees.jl) package. |
| [Langford's cover tree](http://hunch.net/~jl/projects/cover_tree/cover_tree.html) | A reference implementation for the cover tree data structure created by John Langford.  The implementation is in C, and the data structure is widely included in C/C++ machine learning libraries. |
| [MLPack](http://mlpack.org) | A C++ library for machine learning.  MLPack was the first library to demonstrate the utility of generic programming in machine learning.  The interface for nearest neighbor queries lets you use either a cover tree or kdtree.
| [R](http://r-project.org) | A popular language for statisticians.  Nearest neighbor queries are implemented in the [FNN](https://cran.r-project.org/web/packages/FNN/index.html) package, which provides bindings to the C-based [ANN](http://www.cs.umd.edu/~mount/ANN/) library for kdtrees. |
| [scikit-learn](scikitlearn.org) | The Python machine learning toolkit.  The documentation is very beginner friendly and easy to learn.  The interface for nearest neighbor queries lets you use either a [ball tree](https://en.wikipedia.org/wiki/Ball_tree) or [kdtree](https://en.wikipedia.org/wiki/K-d_tree) to speed up the calculations.  Both data structures were written in [Cython](http://cython.org). |
| [Weka](http://weka.org) | A Java data mining tool with a popular GUI frontend.  Nearest neighbor queries in Weka are very, very slow for me and not remotely competitive with any of the libraries above. |

