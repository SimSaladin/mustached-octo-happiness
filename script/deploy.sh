#!/bin/bash

HOST=functor
NAME=mustached-octo-happiness
MOH_PATH=/srv/sites/mustached-octo-happiness

cabal clean \
   && cabal configure -fproduction \
   && cabal build \
   && rsync -r static/ $HOST:$MOH_PATH/static \
   && scp -C dist/build/$NAME/$NAME \
          $HOST:$MOH_PATH/binary.new \
   && echo -en '' \
   && ssh -t functor "bash \
   -c 'sudo systemctl stop $NAME \
   && mv $MOH_PATH/{binary,binary.old} \
   && mv $MOH_PATH/{binary.new,binary} \
   && sudo systemctl start $NAME'"
          # xxx: trap for unsuccesful mv's?
          # xxx: clean up &&'s with that bash's exit-on-failed-command hook?
