require(tidyverse)
require(sf)

mass <- read_sf("data/towns")

somerville_outline <- filter(mass, TOWN == "SOMERVILLE")

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

lighthouses <- read_sf("data/lighthouses")

lights <- ggplot() +
  geom_sf(
    data = lighthouses
  ) + 
  geom_sf(
    data = mass
  )

lights
