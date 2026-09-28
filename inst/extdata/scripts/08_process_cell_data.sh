#!/bin/bash
###############################################################################
# PROCESS THE DATA OF EACH CELL. PLOT AND CREATE TABLES WITH REGRESSION PARAMETERS
###############################################################################

SCRIPT=/home/alber/Documents/github/queimadas/inst/extdata/scripts/./fire_by_cell.R
RDS_DIR=/home/alber/Documents/results/r_packages/queimadas/cell_ts
OUT_DIR=/home/alber/Documents/results/r_packages/queimadas/cell_ts_lm01

parallel -j24 $SCRIPT -f {1} -o $OUT_DIR ::: $(find $RDS_DIR -type f -iname "*.RDS")
