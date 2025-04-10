library(sf) 
library(dplyr)
library(tidyverse)
library(readr)

# File paths - update if needed  

nuts2_path <- "raw_data/instrument_for_datacenters/NUTS_RG_20M_2021_3035.shp"

# Alternative apporach with coastline:
# nuts2 <- readRDS("work_data/instruments_for_datacenters/Regional_Coastline_Buffer_0_5KM.Rds")

sub_cables_path <- "raw_data/subsea_cables_location/sub_cables.geojson"
output_path <- "work_data/instruments_for_datacenters/subsea_cable_landing_points.rds"

# Load NUTS2 shapefile (keeping only ID + geometry) 
# Watch out! Here one must filter for NUTS-2 regions (in the dataset there are
# also other levels). In any case, it is possible change the file 
# with that with coastline, some intersection might be missing
# (we should check why)

nuts2 <- st_read(nuts2_path) %>% 
  filter(LEVL_CODE == 2) %>% 
  select(NUTS_ID, LEVL_CODE, NAME_LATN) 

# Load subsea cables 
sub_cables <- st_read(sub_cables_path)

# Make sure both datasets use the same CRS  
sub_cables <- st_transform(sub_cables, st_crs(nuts2))

# Identify where subsea cables intersect NUTS2 regions  
intersections <- st_join(nuts2, sub_cables, join = st_intersects)  

# Count the number of landing intersections per NUTS2  
landing_counts <- intersections %>%
  drop_na() %>% # Drop NA otherwise count NA as 1 (n() just counts cases, not valid observations)
  group_by(NUTS_ID) %>%
  summarise(landing_count = n(), .groups = "drop")

# Remove geometry and join back to NUTS2  
landing_counts_df <- landing_counts %>% st_drop_geometry()  

# Merge back to main nuts2 df, set NA to 0 (means no landing)  
# Now safely perform the join

nuts2 <- nuts2 %>%
  left_join(landing_counts_df, by = "NUTS_ID") %>%
  mutate(landing_count = ifelse(is.na(landing_count), 0, landing_count))

# Generate binary dummy columns: 1 if at least that many landings exist  
# max_landings <- max(nuts2$landing_count, na.rm = TRUE)  

# for (i in 1:max_landings) {
#  nuts2[[paste0("landing_point_", i)]] <- ifelse(nuts2$landing_count >= i, 1, 0)
# }

# Keep only relevant columns  
# nuts2_final <- nuts2 %>% select(NUTS_ID, starts_with("landing_point_"))  

# Save processed dataset  
#saveRDS(nuts2_final, output_path)  
saveRDS(nuts2, output_path) 
cat("Subsea cable landing points saved to:", output_path, "\n")  
