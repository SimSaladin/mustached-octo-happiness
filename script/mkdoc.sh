#!/bin/bash

cd doc || exit 1

mpost usecases.mp \
   && pdflatex dokumentaatio \
   && pdflatex dokumentaatio
