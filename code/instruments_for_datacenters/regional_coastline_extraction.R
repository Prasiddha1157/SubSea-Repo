library(sf)
library(tidyverse)

# Importing original shapefiles

europe_coastline <- st_read(file.path("raw_data/instrument_for_datacenters/EEA_Coastline_Polygon_Shape", "EEA_Coastline_20170228.shp"))

nuts2 <- st_read(file.path("raw_data/instrument_for_datacenters/NUTS_RG_20M_2021_3035.shp", "NUTS_RG_20M_2021_3035.shp"))

nuts2 <- nuts2 %>% filter(LEVL_CODE == "2")

# Reducing the level of detail to simplify the calculus of borders
europe_coastline <- st_simplify(europe_coastline, dTolerance = 1000)
europe_coastline <- europe_coastline %>% filter(!st_is_empty(geometry))

# Considering only the borders, namely transforming into string
europe_coastline <- st_cast(europe_coastline, "MULTILINESTRING")

# Selecting regional boundaries
nuts2_boundaries <- st_cast(st_boundary(nuts2), "MULTILINESTRING")

# Since the maps of NUTS2 borders is of lower quality, it is necessary to create a buffer around the shape,
# so to intersect this area with the coastline file. The result of the interesection is only the coastline file.
boundaries_buffer <- st_buffer(nuts2_boundaries, dist = 10000)

# Perform intersection
regional_coastline <- st_intersection(boundaries_buffer, europe_coastline)

# Reducing data to one observation for NUTS2 region
regional_coastline <- regional_coastline %>%
  group_by(NUTS_ID, CNTR_CODE, NAME_LATN, NUTS_NAME) %>% 
  summarise(geometry = st_union(geometry),
            , .groups = "drop")

regional_coastline_buffer_5KM <- regional_coastline %>% mutate(
  geometry = st_buffer(geometry, dist = 5000)
  # Recall that the unit of measure for the distance is meters and corresponds to the radius
)

regional_coastline_buffer_2_5KM <- regional_coastline %>% mutate(
  geometry = st_buffer(geometry, dist = 2500)
)

regional_coastline_buffer_1KM <- regional_coastline %>% mutate(
  geometry = st_buffer(geometry, dist = 1000)
)

regional_coastline_buffer_0_5KM <- regional_coastline %>% mutate(
  geometry = st_buffer(geometry, dist = 500)
)

write_rds(regional_coastline, "work_data/instruments_for_datacenters/Regional_Coastline.Rds")
write_rds(regional_coastline_buffer_5KM, "work_data/instruments_for_datacenters/Regional_Coastline_Buffer_5KM.Rds")
write_rds(regional_coastline_buffer_2_5KM, "work_data/instruments_for_datacenters/Regional_Coastline_Buffer_2_5KM.Rds")
write_rds(regional_coastline_buffer_1KM, "work_data/instruments_for_datacenters/Regional_Coastline_Buffer_1KM.Rds")
write_rds(regional_coastline_buffer_0_5KM, "work_data/instruments_for_datacenters/Regional_Coastline_Buffer_0_5KM.Rds")

