#!/bin/bash

# figure to latex (exec. at doc/):
#    dot2tex -f pstricks --figonly ../db-graphs/diagrams/summary/relationships.real.compact.dot > relationships.tex

# column table exported from html view as pdf.

mkdir db-graphs

schemaspy \
   -t pgsql \
   -dp /usr/share/java/postgresql-jdbc/postgresql-9.2-1002.jdbc3.jar \
   -host localhost \
   -u mustached-octo-happiness \
   -db mustached-octo-happiness \
   -s public \
   -o db-graphs \
   -norows -hq
