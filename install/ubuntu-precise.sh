#!/bin/sh

# travis automatically sets these variables;
# this is a hacked test to check if we're running on travis,
# if not, we need to set them manually
if [ -z "$CABALVER" ]; then

    # set environment variables
    CABALVER=1.22
    GHCVER=7.10.2
    LLVMVER=3.5

    # get hlearn code
    git clone https://github.com/mikeizbicki/hlearn
    cd hlearn

    # travis defines this function, so we need to define it too
    travis_retry() {
        $@
    }
fi

# install numeric deps
travis_retry sudo apt-get install -qq libblas-dev liblapack-dev

# update g++ version (required for llvm)
travis_retry sudo add-apt-repository -y ppa:ubuntu-toolchain-r/test
travis_retry sudo apt-get update -qq
travis_retry sudo apt-get install -qq g++-4.8
travis_retry sudo update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.8 90

# update llvm
travis_retry wget -O - http://llvm.org/apt/llvm-snapshot.gpg.key|sudo apt-key add -
travis_retry sudo add-apt-repository "deb http://llvm.org/apt/precise/ llvm-toolchain-precise main"
travis_retry sudo add-apt-repository "deb http://llvm.org/apt/precise/ llvm-toolchain-precise-$LLVMVER main"
travis_retry sudo apt-get update
sudo apt-get install -y llvm-$LLVMVER llvm-$LLVMVER-dev
sudo ln -s /usr/bin/opt-$LLVMVER /usr/bin/opt
export PATH="/usr/bin:$PATH"

# install haskell bits
travis_retry sudo add-apt-repository -y ppa:hvr/ghc
travis_retry sudo apt-get update -qq
travis_retry sudo apt-get install -qq cabal-install-$CABALVER ghc-$GHCVER
export PATH="~/.cabal/bin:/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$PATH"

# install the local version of subhask
cd subhask
travis_retry cabal update
travis_retry cabal install -j4
cd ..

# install hlearn
cabal --version
echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]"
cabal install -j4 --only-dependencies --enable-tests --enable-benchmarks
