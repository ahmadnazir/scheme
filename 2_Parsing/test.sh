#!/bin/bash

cabal exec ghc Main && ./Main "`more samples/test.txt`"
