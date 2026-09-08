#!/bin/bash
###############################################################################
# RUN SPATIAL JOINS BETWEEN THE ACTIVE FIRES FROM QUEIMADAS AND THE SPATAIL
# DIVISION
###############################################################################

SCRIPT=/home/alber/Documents/github/queimadas/inst/extdata/scripts/./fire_sp_join.R

CSV_DIR=/home/alber/Documents/data/r_packages/queimadas/csv
GPKG_FILE=/home/alber/Documents/data/r_packages/queimadas/grade_tm_util.gpkg
OUT_DIR=/home/alber/Documents/data/r_packages/queimadas/results/gpkg

parallel -j8 $SCRIPT -c {1} -g $GPKG_FILE -o $OUT_DIR ::: $(find $CSV_DIR -type f -iname "*.csv")
