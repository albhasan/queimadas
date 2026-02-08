library(dplyr)
library(geobr)

brstate_tb <-
  geobr::read_state() %>%
  sf::st_drop_geometry()

usethis::use_data(brstate_tb, overwrite = TRUE)
