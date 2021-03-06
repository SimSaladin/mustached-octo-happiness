#!/bin/bash

cd doc || exit 1

mpost usecases.mp \
   && mpost ui.mp \
   && mpost class.mp \
   && pdflatex --shell-escape dokumentaatio \
   && pdflatex --shell-escape dokumentaatio
