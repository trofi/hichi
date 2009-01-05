#!/bin/sh -e

cabal configure
cabal build

dist/build/hichat/hichat