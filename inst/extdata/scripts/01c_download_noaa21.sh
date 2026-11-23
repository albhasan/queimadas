#!/bin/bash
###############################################################################
# FIRMS
# https://firms.modaps.eosdis.nasa.gov
# Download VIIRS NOAA-20 as CSVs.
# alber.ipia@inpe.br
###############################################################################

# VIIRS_NOAA_21
# Data requests.
# start      end        id     datasource
# 2023-03-01 2023-12-31 855252 jpss2-viirs-c2 #NOTE: This year is incomplete!
# 2024-01-01 2024-12-31 855255 jpss2-viirs-c2
# 2025-01-01 2025-12-31 855256 jpss2-viirs-c2
# 2026-01-01 2026-11-23 855257 jpss2-viirs-c2 #NOTE: This year is incomplete!

OUT21="/home/alber/Documents/data/r_packages/queimadas/VIIRS_NOAA_21"
wget -P $OUT21 https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J2V-C2_855252.zip
wget -P $OUT21 https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J2V-C2_855255.zip
wget -P $OUT21 https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J2V-C2_855256.zip
wget -P $OUT21 https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J2V-C2_855257.zip

exit 0
