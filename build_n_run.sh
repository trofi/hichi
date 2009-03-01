#!/bin/sh -e

cabal configure
cabal build
cabal sdist

dist/build/hichi/hichi