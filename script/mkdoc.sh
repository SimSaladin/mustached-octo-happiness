#!/bin/bash

cd doc || exit 1

mpost usecases.mp \
   && mpost ui.mp \
   && mpost class.mp \
   && mpost database.mp \
   && pdflatex dokumentaatio \
   && pdflatex dokumentaatio
