require(tidyverse)
require(sf)

neighborhoods <- read_csv("data/neighborhoods.csv") %>%
  st_as_sf(
    wkt = "Geometry",
    crs = st_crs("EPSG:4269")
  )

bike_lanes <- read_csv("data/bike_lanes.csv") %>%
  st_as_sf(
    wkt = "Geometry",
    crs = st_crs("EPSG:4269")
  )

bars <- read_csv("data/bars.csv") %>%
  # filter(!grepl("WEST SENECA", `Additional Address Information`), fixed=TRUE) %>%
  # filter(`Zip Code` %in% 
  #          c(14201, 14202, 14203, 14204, 14206, 
  #            14207, 14208, 14209, 14210, 14211, 
  #            14212, 14213, 14214, 14215, 14216, 14218, 14220, 14222)
  #        ) %>%
  filter(!is.na(Georeference), Georeference != "") %>%
  select(LegalName, Georeference) %>%
  st_as_sf(
    wkt = "Georeference",
    crs = st_crs("EPSG:4269")
  )

map <- ggplot() +
  geom_sf(
    data = neighborhoods
  ) +
  geom_sf(
    data = bike_lanes,
    aes(color = `Facility`)
  ) +
  geom_sf(
    data = bars
  ) +
  geom_sf_text(
    data = neighborhoods, 
    aes(label = `Neighborhood Name`), 
    size = 2.5,
  ) +
  theme_void()

map
