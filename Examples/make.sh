#!/bin/sh

ghc -O2 --make -rtsopts BasicConcurrent.hs
ghc -O2 --make -rtsopts ByteStringConcurrent.hs
ghc -O2 --make -rtsopts LibevConcurrent.hs

