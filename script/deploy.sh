#!/bin/bash

cabal clean \
   && cabal configure -fproduction \
   && cabal build \
   && scp -r static/combined functor:/srv/sites/mustached-octo-happiness/static \
   && scp dist/build/mustached-octo-happiness/mustached-octo-happiness \
          functor:/srv/sites/mustached-octo-happiness/binary
