#!/bin/bash

cabal haddock \
   && sed -i -e \
      's|file://[^>]*/\([^/]*\)/html|http://hackage.haskell.org/package/\1/docs|g' \
      dist/doc/html/mustached-octo-happiness/*.html \
   && scp -r dist/doc/html/mustached-octo-happiness \
         functor:/srv/work/0DUMP/mustached-octo-happiness
