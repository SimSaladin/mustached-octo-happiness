#!/bin/bash

cd doc || exit 1

mpost usecases.mp \
   && pdflatex doc \
   && pdflatex doc
