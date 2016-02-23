#!/bin/bash

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
cabal sandbox init --sandbox ../../.cabal-sandbox
cabal install
popd

popd

cabal configure
