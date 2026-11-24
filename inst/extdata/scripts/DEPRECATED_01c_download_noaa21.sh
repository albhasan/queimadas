#!/bin/bash
###############################################################################
# FIRMS
# https://firms.modaps.eosdis.nasa.gov
# Download VIIRS NOAA-21 as CSVs.
# alber.ipia@inpe.br
###############################################################################

echo "DEPRECATED: This is no longer neccessary. The data is alredy present in the data base."
exit 1

# VIIRS_NOAA_21
# Data requests.
# start      end        id     datasource
# 2023-03-01 2023-12-31 855252 jpss2-viirs-c2 #NOTE: This year is incomplete!
# 2024-01-01 2024-12-31 855255 jpss2-viirs-c2
# 2025-01-01 2025-12-31 855256 jpss2-viirs-c2
# 2026-01-01 2026-11-23 855257 jpss2-viirs-c2 #NOTE: This year is incomplete!

OUT21W="/home/alber/Documents/data/r_packages/queimadas/VIIRS_NOAA_21/zip_world"
wget -P $OUT21W https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J2V-C2_855252.zip
wget -P $OUT21W https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J2V-C2_855255.zip
wget -P $OUT21W https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J2V-C2_855256.zip
# wget -P $OUT21W https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J2V-C2_855257.zip

# VIIRS_NOAA_21
# Data requests.
# start      end        id     datasource
# 2023-03-01 2023-12-31 855365 jpss2-viirs-c2 #NOTE: This year is incomplete!
# 2024-01-01 2024-12-31 855366 jpss2-viirs-c2
# 2025-01-01 2025-12-31 855367 jpss2-viirs-c2
# 2026-01-01 2026-11-23 855371 jpss2-viirs-c2 #NOTE: This year is incomplete!

OUT21B="/home/alber/Documents/data/r_packages/queimadas/VIIRS_NOAA_21/zip_brazil"
wget -P $OUT21B https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J2V-C2_855365.zip
wget -P $OUT21B https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J2V-C2_855366.zip
wget -P $OUT21B https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J2V-C2_855367.zip
#wget -P $OUT21B https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J2V-C2_855371.zip

exit 0
