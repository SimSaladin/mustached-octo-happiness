#!/bin/bash

cabal clean \
   && cabal configure -fproduction \
   && cabal build \
   && rsync -r static/ \
             functor:/srv/sites/mustached-octo-happiness/static \
   && scp -C dist/build/mustached-octo-happiness/mustached-octo-happiness \
          functor:/srv/sites/mustached-octo-happiness/binary.new \
   && ssh -t functor 'bash -c "sudo systemctl stop mustached-octo-happiness \
   && mv /srv/sites/mustached-octo-happiness/{binary,binary.old} \
   && mv /srv/sites/mustached-octo-happiness/{binary.new,binary} \
   && sudo systemctl start mustached-octo-happiness"'
          # xxx: trap for unsuccesful mv's?
