#!/bin/bash

#prof="-prof"
prof=""
opt="-O2" 
sandbox="-no-user-package-db -package-db ../../.cabal-sandbox/x86_64-linux-ghc-7.8.2-packages.conf.d"
core="-ddump-simpl -dsuppress-module-prefixes -dsuppress-uniques -dsuppress-uniques -dsuppress-type-applications -dsuppress-idinfo -keep-s-file -keep-llvm-files" 

ghc -fforce-recomp $prof $opt $sandbox $core "$1" > "$1.core"
