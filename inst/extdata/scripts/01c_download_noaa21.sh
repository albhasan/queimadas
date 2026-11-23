#!/bin/bash
###############################################################################
# FIRMS
# https://firms.modaps.eosdis.nasa.gov
# Download VIIRS NOAA-20 as CSVs.
# alber.ipia@inpe.br
###############################################################################

# VIIRS_NOAA_20
# Data requests.
# start      end        id
# 2018-04-01 2018-12-31 855225 jpss1-viirs-c2
# 2019-01-01 2019-12-31 855227 jpss1-viirs-c2
# 2020-01-01 2020-12-31 855230 jpss1-viirs-c2
# 2021-01-01 2021-12-31 855232 jpss1-viirs-c2
# 2022-01-01 2022-12-31 855233 jpss1-viirs-c2
# 2023-01-01 2023-12-31 855234 jpss1-viirs-c2
# 2024-01-01 2024-12-31 855235 jpss1-viirs-c2
# 2025-01-01 2025-12-31 855236 jpss1-viirs-c2
# 2026-01-01 2026-11-23 855238 jpss1-viirs-c2

OUT20="/home/alber/Documents/data/VIIRS_NOAA_20"
wget -P $OUT20 https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855225.zip
wget -P $OUT20 https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855227.zip
wget -P $OUT20 https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855230.zip
wget -P $OUT20 https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855232.zip
wget -P $OUT20 https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855233.zip
wget -P $OUT20 https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855234.zip
wget -P $OUT20 https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855235.zip
wget -P $OUT20 https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855236.zip
wget -P $OUT20 https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855238.zip

# VIIRS_NOAA_21
# Data requests.
# start      end        id     datasource
# 2023-03-01 2023-12-31 855252 jpss2-viirs-c2
# 2024-01-01 2024-12-31 855255 jpss2-viirs-c2
# 2025-01-01 2025-12-31 855256 jpss2-viirs-c2
# 2026-01-01 2026-11-23 855257 jpss2-viirs-c2

OUT21="/home/alber/Documents/data/VIIRS_NOAA_21"
wget -P $OUT21 https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J2V-C2_855252.zip
wget -P $OUT21 https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J2V-C2_855255.zip
wget -P $OUT21 https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J2V-C2_855256.zip
wget -P $OUT21 https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J2V-C2_855257.zip

exit 0
