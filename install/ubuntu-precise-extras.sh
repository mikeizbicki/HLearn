#!/bin/bash
cd
set -e

# installing julia
#sudo add-apt-repository -y ppa:staticfloat/juliareleases
#sudo add-apt-repository -y ppa:staticfloat/julia-deps
##sudo add-apt-repository -y 'deb http://ppa.launchpad.net/staticfloat/juliareleases/ubuntu precise main'
#sudo apt-get update
#sudo apt-get install -qq julia

# the julia package is not compatible with python's numpy due to conflicting lapack dependencies
wget https://github.com/JuliaLang/julia/releases/download/v0.3.10/julia-0.3.10_c8ceeefcc1.tar.gz
tar -xf julia-0.3.10_c8ceeefcc1.tar.gz
cd julia
make -j5
sudo make install
export PATH="/home/ubuntu/julia/julia-0.3.10/bin:$PATH"
cd

echo "Pkg.add(\"KDTrees\")" > setup.julia
julia setup.julia

# install R
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
sudo add-apt-repository -y 'deb http://streaming.stat.iastate.edu/CRAN/bin/linux/ubuntu precise/'
sudo apt-get update
sudo apt-get install -qq r-base r-base-dev

echo "install.packages(\"FNN\",repos=\"http://cran.us.r-project.org\")" > setup.R
sudo Rscript setup.R

# install scikit
sudo apt-get install -qq subversion
sudo apt-get install -qq python-dev python-pip
sudo pip install numpy
sudo pip install scipy
sudo pip install sklearn

# install flann
sudo apt-get install -qq cmake unzip make
wget http://www.cs.ubc.ca/research/flann/uploads/FLANN/flann-1.8.4-src.zip
unzip flann-1.8.4-src.zip
cd flann-1.8.4-src
mkdir build
cd build
cmake ..
make -j5
sudo make install
cd

# install mlpack
sudo apt-get install -qq libxml2-dev
sudo apt-get install -qq libboost-all-dev libboost-program-options-dev libboost-test-dev libboost-random-dev
sudo apt-get install -qq doxygen

wget http://downloads.sourceforge.net/project/boost/boost/1.58.0/boost_1_58_0.tar.gz
tar -xf boost_1_58_0.tar.gz
cd boost_1_58_0
./booststrap.sh
./b2
sudo ./b2 install
cd

wget http://sourceforge.net/projects/arma/files/armadillo-5.200.2.tar.gz
tar -xf armadillo-5.200.2.tar.gz
cd armadillo-5.200.2
cmake ..
make -j5
sudo make install
cd

http://mlpack.org/files/mlpack-1.0.12.tar.gz
tar -xf mlpack-1.0.12.tar.gz
cd mlpack-1.0.12
mkdir build
cd build
cmake ..
make -j5
sudo make install
cd

# weka
sudo apt-get install -qq openjdk-7-jdk
sudo apt-get install -qq weka
