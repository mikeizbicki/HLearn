#!/bin/bash

set -e

# travis automatically sets these variables;
# this is a hacked test to check if we're running on travis,
# if not, we need to set them manually
if [ -z "$CABALVER" ]; then

    # set environment variables
    CABALVER=1.22
    GHCVER=7.10.2
    LLVMVER=3.5

    # install git
    sudo apt-get update -qq
    sudo apt-get install -qq git

    git config --global user.name 'InstallScript'
    git config --global user.email 'installscript@izbicki.me'

    # get hlearn code
    git clone https://github.com/mikeizbicki/hlearn
    cd hlearn
    git submodule update --init --recursive subhask
fi

# install numeric deps
sudo apt-get install -qq libatlas3gf-base
sudo apt-get install -qq libblas-dev liblapack-dev

# update g++ version (required for llvm)
sudo add-apt-repository -y ppa:ubuntu-toolchain-r/test
sudo apt-get update -qq
sudo apt-get install -qq g++-4.8
sudo update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.8 90

# update llvm
wget -O - http://llvm.org/apt/llvm-snapshot.gpg.key|sudo apt-key add -
sudo add-apt-repository "deb http://llvm.org/apt/precise/ llvm-toolchain-precise main"
sudo add-apt-repository "deb http://llvm.org/apt/precise/ llvm-toolchain-precise-$LLVMVER main"
sudo apt-get update
sudo apt-get install -y llvm-$LLVMVER llvm-$LLVMVER-dev
sudo ln -s /usr/bin/opt-$LLVMVER /usr/bin/opt
sudo ln -s /usr/bin/llc-$LLVMVER /usr/bin/llc
export PATH="/usr/bin:$PATH"

# install haskell bits
sudo add-apt-repository -y ppa:hvr/ghc
sudo apt-get update -qq
sudo apt-get install -qq cabal-install-$CABALVER ghc-$GHCVER
export PATH="~/.cabal/bin:/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$PATH"

# install the local version of subhask
cd subhask
cabal update
cabal install -j4
cd ..

# install hlearn
cabal --version
echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]"
cabal install -j4 --only-dependencies --enable-tests --enable-benchmarks
cabal install || cabal install --disable-optimization

echo "Optimization: False" >> /home/travis/.cabal/config
