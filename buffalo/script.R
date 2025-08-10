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

barIsInBuffalo <- lengths(st_intersects(bars, neighborhoods)) > 0
bars <- bars[barIsInBuffalo,]

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

intersection <- st_intersects(
    x = neighborhoods,
    y = bars
  )

neighborhoods <- mutate(
    neighborhoods,
    numBars = lengths(intersection)
  )

countmap <- ggplot() +
  geom_sf(
    data = neighborhoods,
    aes(fill = numBars)
  ) +
  scale_fill_gradient2(
    low = "#ffffe0",
    mid = "#86754d",
    high = "#aa483d",
    midpoint = 15
  )

countmap

roads <- read_csv("data/major_roads.csv") %>%
  filter(!is.na(the_geom), the_geom != "") %>%
  select(CompleteStreetName, the_geom) %>%
  st_as_sf(
    wkt = "the_geom",
    crs = st_crs("EPSG:4269")
  )

barsandroads <- ggplot() +
  geom_sf(
    data = neighborhoods,
    lwd = 0,
    aes(fill = numBars)
  ) +
  scale_fill_gradient2(
    low = "#ffffe0",
    mid = "#86754d",
    high = "#aa483d",
    midpoint = 25
  ) +
  geom_sf(
    data = roads,
    color = "#333",
    alpha = 0.6,
    lwd = 0.15
  ) +
  labs(
    title = "Buffalo's Bars"
  ) +
  theme_void()

barsandroads

heatmap <- ggplot() +
  geom_sf(
    data = neighborhoods
  ) +
  geom_sf(
    data = bars,
    stroke = .1,
    alpha = 0.3
  ) +
  stat_density_2d(
    aes(
      x = st_coordinates(bars)[,1], 
      y = st_coordinates(bars)[,2],
      fill = ..level..
    ),
    alpha = 0.2,
    geom = 'polygon',
    color = NA
  ) +
  scale_fill_gradient2(
    low = "#ffffe0",
    mid = "#86754d",
    high = "#aa483d",
    guide = "none"
    # midpoint = 100
  ) +
  theme_void()
      
heatmap
