#!/bin/bash

cd doc || exit 1

mpost usecases.mp \
   && mpost ui.mp \
   && mpost class.mp \
   && pdflatex dokumentaatio \
   && pdflatex dokumentaatio
