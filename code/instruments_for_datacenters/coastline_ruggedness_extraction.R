calculate_riley_rug <- function(x_loc, y_loc, depth_loc, df) {
  # Filter neighboring points within the specified range
  neighboring_area <- df %>%
    filter(
      x_order == (x_loc + 1) | x_order == (x_loc - 1),
      y_order == (y_loc + 1) | y_order == (y_loc - 1)
    )
  
  # Check if neighboring_area has enough rows
  if (nrow(neighboring_area) < 4) {
    return(NA)  # Return NA if not enough neighbors
  }
  
  # Calculate the Riley Rug value
  riley_rug_value <- neighboring_area %>%
    mutate(
      sqhdist = (depth_loc - elevation)^2
    ) %>%
    summarize(riley_rug = sqrt(sum(sqhdist))) %>%
    pull(riley_rug)  # Extract riley_rug value directly
  
  return(riley_rug_value)
}

library(terra) 
library(dplyr)
library(sf) 

# Importing coastline datasets
buffer_size <- "1KM"
dataset_name <- paste0("work_data/instruments_for_datacenters/Regional_Coastline_Buffer_", buffer_size, ".Rds")

regional_coastline <- readRDS(dataset_name)

# Importing bathymetry data
bathymetry_data_1 <- rast("~/Downloads/gebco_2024_geotiff/gebco_2024_n90.0_s0.0_w0.0_e90.0.tif")
bathymetry_data_2 <- rast("~/Downloads/gebco_2024_geotiff/gebco_2024_n90.0_s0.0_w-90.0_e0.0.tif")

bathymetry_data <- merge(bathymetry_data_1, bathymetry_data_2)

# Looping across regions
regional_coastline <- st_transform(regional_coastline, crs(bathymetry_data))
regions <- regional_coastline %>% select(NUTS_ID) %>% st_drop_geometry() %>% pull(NUTS_ID)

regional_bathymetry <- list()
regional_coastline_ruggedness <- list()

# Loop over regions
for (region in regions) {
  coastline <- regional_coastline %>% filter(NUTS_ID == region)
  coastline_vect <- vect(coastline)
  bathymetry_masked <- mask(bathymetry_data, coastline_vect)
  bathymetry_masked_df <- as.data.frame(bathymetry_masked, xy = TRUE) %>%
    rename(elevation = bathymetry_data_liguria)  # Change the name
  regional_bathymetry[[region]] <- bathymetry_masked_df
  
  # Part on Riley Ruggedness
  # Excluding regions with no coastline
  if(length(bathymetry_masked_df) == 0){
    regional_coastline_ruggedness[[region]] <- NA
    next
  }
  
  bathymetry_masked_df <- bathymetry_masked_df %>% mutate(
    x_order = dense_rank(x),
    y_order = dense_rank(y)
  )
  
  bathymetry_masked_df <- bathymetry_masked_df %>%
    mutate(
      riley_rug = pmap_dbl(
        list(x_order, y_order, elevation),
        ~ calculate_riley_rug(..1, ..2, ..3, bathymetry_masked_df)
      )
    )
  
  bathymetry_masked_df <- bathymetry_masked_df %>% na.omit() %>% 
    summarize(NUTS_ID = region,
              mean_ruggedness = mean(riley_rug,),
              median_ruggedness = median(riley_rug),
              max_ruggedness = max(riley_rug),
              min_ruggedness = min(riley_rug))

  regional_coastline_ruggedness[[region]] <- bathymetry_masked_df 
}

regional_coastline_ruggedness <- as.data.frame(do.call(rbind, regional_coastline_ruggedness))

saveRDS(regional_coastline_ruggedness, file = paste0("work_data/instruments_for_datacenters/regional_coastline_ruggedness_", buffer_size, ".Rds"))

