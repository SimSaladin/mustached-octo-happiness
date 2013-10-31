#!/bin/bash

cd doc || exit 1

mpost usecases.mp \
   && mpost ui.mp \
   && pdflatex dokumentaatio \
   && pdflatex dokumentaatio
