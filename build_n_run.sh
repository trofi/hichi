#!/bin/sh -e

cabal configure -ftest
cabal build
cabal sdist

dist/build/test-rc4/test-rc4
dist/build/hichi/hichi
