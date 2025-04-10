# Load required libraries
library(tidyverse)
library(maps)
library(dbscan)
library(geosphere)
library(readxl)
library(sf)
library(readr)
library(giscoR)
library(magrittr)
library(dplyr)
library(stringr)
library(igraph)

# File paths
cloud_regions_path <- "work_data/datacenters_location/cloud_regions_dataset.xlsx"
local_zones_path <- "work_data/datacenters_location/local_zones_dataset.xlsx"
on_ramps_path <- "work_data/datacenters_location/on_ramps_dataset.xlsx"
world_boundaries_path <- "raw_data/geopolitical_data/world-administrative-boundaries/world-administrative-boundaries.shp"
output_path <- "work_data/datacenters_location/countries_and_datacenters.R"

# Load world boundaries
world_boundaries <- st_read(world_boundaries_path) %>%
  select(color_code, geometry) %>%
  rename(country_code = color_code)

world_boundaries <- world_boundaries %>% group_by(country_code)  %>%
  summarise(geometry = st_union(geometry)) 

world_boundaries <- world_boundaries %>% st_cast("MULTIPOLYGON")


# Cast multipolygons into individual polygons
world_boundaries_single_territories <- world_boundaries %>%
  st_cast(to = "POLYGON", group_or_split = FALSE) %>%
  group_by(country_code) %>%
  mutate(country_code_n = paste0(country_code, "_", row_number())) %>%
  ungroup() %>% select(-country_code) %>% relocate(country_code_n, .before = geometry) # Keep only ISO country codes and geometry




# Function to process each data center dataset
process_datacenter_file <- function(file_path, type_column) {
  df <- read_excel(file_path) %>%
    select(country_code) %>%
    group_by(country_code) %>%
    summarise(!!type_column := n(), .groups = "drop")  # Create a named column dynamically
  return(df)
}

# Process each dataset separately
cloud_regions <- process_datacenter_file(cloud_regions_path, "Cloud_Regions")
local_zones <- process_datacenter_file(local_zones_path, "Local_Zones")
on_ramps <- process_datacenter_file(on_ramps_path, "On_Ramps")

# Merge all data center types
datacenter_counts <- full_join(cloud_regions, local_zones, by = "country_code") %>%
  full_join(on_ramps, by = "country_code") %>%
  mutate(across(c(Cloud_Regions, Local_Zones, On_Ramps), ~ replace_na(.x, 0))) %>%
  mutate(Total_DataCenters = Cloud_Regions + Local_Zones + On_Ramps)

# Country centroids (accounting for earth's curvature)
# Transform to an equal-area CRS (EPSG: 6933 accounts for curvature)
world_boundaries_projected <- st_transform(world_boundaries, crs = 6933)  # Best for Earth's curvature

# Compute centroids in projected CRS from above
world_centroids <- world_boundaries_projected %>%
  group_by(country_code) %>%
  summarise(geometry = st_union(geometry)) %>%  # Merge multipolygons so centroid falls within land
  st_centroid() %>%  # Compute centroid in projected CRS
  st_transform(crs = 4326)  # Convert back to lat-long


# Merge Centroids with data center counts
final_dataset <- world_centroids %>%
  select(country_code, geometry) %>%
  inner_join(datacenter_counts, by = "country_code")

# Save the final dataset
saveRDS(final_dataset, output_path)
cat("Final dataset saved to:", output_path, "\n")
