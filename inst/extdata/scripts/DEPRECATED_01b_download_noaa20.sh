#!/bin/bash
###############################################################################
# FIRMS
# https://firms.modaps.eosdis.nasa.gov
# Download VIIRS NOAA-20 as CSVs.
# alber.ipia@inpe.br
###############################################################################

echo "DEPRECATED: This is no longer neccessary. The data is alredy in the data base."
exit 1

# VIIRS_NOAA_20 WORLD.
# Data requests.
# start      end        id
# 2018-04-01 2018-12-31 855225 jpss1-viirs-c2 #NOTE: This year is incomplete!
# 2019-01-01 2019-12-31 855227 jpss1-viirs-c2
# 2020-01-01 2020-12-31 855230 jpss1-viirs-c2
# 2021-01-01 2021-12-31 855232 jpss1-viirs-c2
# 2022-01-01 2022-12-31 855233 jpss1-viirs-c2
# 2023-01-01 2023-12-31 855234 jpss1-viirs-c2
# 2024-01-01 2024-12-31 855235 jpss1-viirs-c2
# 2025-01-01 2025-12-31 855236 jpss1-viirs-c2
# 2026-01-01 2026-11-23 855238 jpss1-viirs-c2 #NOTE: This year is incomplete!

OUT20W="/home/alber/Documents/data/r_packages/queimadas/VIIRS_NOAA_20/zip_world"
wget -P $OUT20W https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855225.zip
wget -P $OUT20W https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855227.zip
wget -P $OUT20W https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855230.zip
wget -P $OUT20W https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855232.zip
wget -P $OUT20W https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855233.zip
wget -P $OUT20W https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855234.zip
wget -P $OUT20W https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855235.zip
wget -P $OUT20W https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855236.zip
# wget -P $OUT20W https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855238.zip

# VIIRS_NOAA_20 BRASIL
# Data requests.
# start      end        id
# 2018-04-01 2018-12-31 855353 jpss1-viirs-c2 #NOTE: This year is incomplete!
# 2019-01-01 2019-12-31 855354 jpss1-viirs-c2
# 2020-01-01 2020-12-31 855355 jpss1-viirs-c2
# 2021-01-01 2021-12-31 855362 jpss1-viirs-c2
# 2022-01-01 2022-12-31 855357 jpss1-viirs-c2
# 2023-01-01 2023-12-31 855358 jpss1-viirs-c2
# 2024-01-01 2024-12-31 855359 jpss1-viirs-c2
# 2025-01-01 2025-12-31 855360 jpss1-viirs-c2
# 2026-01-01 2026-11-23 855361 jpss1-viirs-c2 #NOTE: This year is incomplete!

OUT20B="/home/alber/Documents/data/r_packages/queimadas/VIIRS_NOAA_20/zip_brazil"
wget -P $OUT20B https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855353.zip
wget -P $OUT20B https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855354.zip
wget -P $OUT20B https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855355.zip
wget -P $OUT20B https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855362.zip
wget -P $OUT20B https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855357.zip
wget -P $OUT20B https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855358.zip
wget -P $OUT20B https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855359.zip
wget -P $OUT20B https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855360.zip
#wget -P $OUT20B https://firms.modaps.eosdis.nasa.gov/data/download/DL_FIRE_J1V-C2_855361.zip

exit 0
