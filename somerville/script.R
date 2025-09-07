require(tidyverse)
require(sf)

# neighborhoods <- read_csv("data/neighborhoods.csv") %>%
#   st_as_sf(
#     wkt = "Geometry",
#     crs = st_crs("EPSG:4269")
#   )

somerville_outline <- read_sf("data/towns") %>%
  filter(TOWN == "SOMERVILLE")

somerville_precincts <- read_sf("data/wardsprecincts2022") %>%
  filter(TOWN == "SOMERVILLE")

precints_outline <- ggplot() +
  geom_sf(
    data = somerville_outline
  ) +
  geom_sf(
    data = somerville_precincts
  ) +
  theme_void()

precints_outline

# TODO: add names of precincts as text
