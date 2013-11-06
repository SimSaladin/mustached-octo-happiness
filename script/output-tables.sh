#!/bin/bash

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
