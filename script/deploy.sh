#!/bin/bash

HOST=functor
MOH_PATH=/srv/sites/mustached-octo-happiness
BIN=mustached-octo-happiness

cabal clean \
   && cabal configure -fproduction \
   && cabal build \
   && rsync -r static/ $HOST:$MOH_PATH/static \
   && scp -C dist/build/mustached-octo-happiness/$BIN-octo-happiness \
          $HOST:$MOH_PATH/binary.new \
   # attract the user, as following sudo's likely require interaction.
   && echo -en '' \
   && ssh -t functor "bash \
   -c 'sudo systemctl stop mustached-octo-happiness \
   && mv $MOH_PATH/{binary,binary.old} \
   && mv $MOH_PATH/{binary.new,binary} \
   && sudo systemctl start mustached-octo-happiness'"
          # xxx: trap for unsuccesful mv's?
          # xxx: clean up &&'s with that bash's exit-on-failed-command hook?
