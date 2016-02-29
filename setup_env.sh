#!/bin/bash

cabal sandbox delete
rm -rf dist externals

cabal update
cabal sandbox init --sandbox .cabal-sandbox

mkdir externals
pushd externals

git clone https://github.com/coffeecup-winner/peparser-haskell peparser
pushd peparser
cabal sandbox init --sandbox ../../.cabal-sandbox
cabal install
popd

git clone https://github.com/coffeecup-winner/hdis86
pushd hdis86
git co HEAD^ # properly update hdis86 to support udis86 1.7.2
cabal sandbox init --sandbox ../../.cabal-sandbox
cabal install
popd

popd

cabal install --only-dependencies
cabal configure
