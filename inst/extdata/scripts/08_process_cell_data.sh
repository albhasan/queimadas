#!/bin/bash
###############################################################################
# PROCESS THE DATA OF EACH CELL. PLOT AND CREATE TABLES WITH REGRESSION PARAMETERS
###############################################################################

SCRIPT=/home/alber/Documents/github/queimadas/inst/extdata/scripts/./fire_by_cell.R

# Directory with the files with the time series of aggregated active fires.
RDS_DIR=/home/alber/Documents/results/r_packages/queimadas/cell_ts

# Directory for storing the results.
OUT_DIR=/home/alber/Documents/github/slides/queimadas/slides/figures_R/s08/cell_lm

# Call the script and run it in parallel.
parallel -j12 $SCRIPT -f {1} -o $OUT_DIR ::: $(find $RDS_DIR -type f -iname "*.RDS")
