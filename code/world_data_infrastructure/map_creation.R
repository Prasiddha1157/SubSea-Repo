library(rvest)
library(dplyr)
library(tidygeocoder)
library(maps)
library(sf)
library(ggpubr)
library(Cairo)
library(ggpattern)
library(openxlsx)
library(ggplot2)

rm(list = ls(all.names = TRUE))

# Importing Datasets 

cloud_regions_data <- read.xlsx("work_data/datacenters_location/cloud_regions_dataset.xlsx")
cloud_regions_data <- as_tibble(cloud_regions_data)
cloud_regions_data <- cloud_regions_data %>% mutate(type = "Cloud_Region")

on_ramps_data <- read.xlsx("work_data/datacenters_location/on_ramps_dataset.xlsx")
on_ramps_data <- as_tibble(on_ramps_data)
on_ramps_data <- on_ramps_data %>% mutate(number_of_datacenters = 1,
                                          type = "On_Ramp"
                                          )
local_zones_data <- read.xlsx("work_data/datacenters_location/local_zones_dataset.xlsx")
local_zones_data <- as_tibble(local_zones_data)
local_zones_data <- local_zones_data %>% mutate(number_of_datacenters = 1,
                                          type = "Local_Zone"
)

# Merging Cloud Regions with On Ramps and Local Zones
datacenters_data <- cloud_regions_data %>% add_row(on_ramps_data) %>% add_row(local_zones_data)

datacenters_data <- datacenters_data %>%
  mutate(location = as.factor(location))

datacenters_data <- datacenters_data %>%
  mutate(block = as.factor(block))

levels(datacenters_data$block) <- c("China", "USA") 

datacenters_data <- datacenters_data %>% distinct(provider_name, region_details, .keep_all = TRUE)

city_aggregates <- datacenters_data %>%
  group_by(location, block) %>%
  summarise(
    number_of_datacenters = sum(number_of_datacenters),
    latitude = min(latitude),  
    longitude = max(longitude)
  ) %>%
  ungroup()

# Add line to save datacenters_data on xlxs

# Plotting datacenters and importing cable maps

# Importing world map
world_map <- map_data("world")

# Importing geometries on cable maps
cable_data <- st_read("raw_data/subsea_cables_location/sub_cables.geojson")

# Extracting the top 3 cities to create labels

top_cities <- city_aggregates %>% arrange(number_of_datacenters) %>% top_n(7, number_of_datacenters) %>%
  select(location, latitude, longitude) %>% mutate(location = sub(" .*", "", location), latitude = latitude -2)

# Plot

#World_Data_Infrastructure <- 
World_Data_Infrastructure_01 <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),color = "#3C3C3C", fill="#EDDDD4") +
  geom_sf(data = cable_data, aes(geometry=geometry), color = "black", size = 1, alpha= 0.6) +
  geom_point(data = city_aggregates, aes(x = longitude, y = latitude, fill = block, size = number_of_datacenters), color= "black", alpha= 0.7, shape=21) + 
  geom_label(data = top_cities, aes(x= longitude, y= latitude, label= location))+
  scale_fill_manual(values = c("USA" = "#FF5A5F", "China"= "#087E8B"))+
  scale_radius(range = c(3, 20))+
  theme_minimal() +
  labs(title = "The World Data Network",
       subtitle = "Subsea Cables and Cloud Datacenters of the main Internet players",
       fill = "Datacenters Owner's Origin",
       size = "Number of Datacenters",
       caption = paste0("Notes: data on submarine cables are updated up to year 2022, while the data cloud datacenters are updated up to year 2024. 
       The graph shows the datacenters of major cloud players, divided into two groups: Microsoft Azure, AWS, Google Cloud, IBM Cloud and Oracle Cloud (US),
       and Alibaba Cloud, Tencent Cloud and Huawei Cloud (China). The dataset contains Cloud Regions, Local Zones and On-Ramp datacenters. 
                        The total number of datacenters is: ", sum(city_aggregates$number_of_datacenters))
  ) +  #theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 50, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 40, face = "italic"),
    plot.caption = element_text(hjust = 0.5, size = 30),
    plot.caption.position = "panel",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "top", 
    legend.title = element_text(size = 20),
    legend.key.size = unit(40, "pt"),
    legend.text = element_text(size = 20),
    legend.spacing = unit(20, "pt"),              
    legend.box = "horizontal",                  
    legend.box.just = "center",
    panel.background = element_rect(fill = "#FEFEFE"),
    plot.background = element_rect(fill = "#F7F7F7")
  ) +
  scale_x_continuous(breaks = seq(-180, 180, by = 60)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 30)) + 
  guides(fill = guide_legend(override.aes = list(size=20)))

ggsave("output/world_data_infrastructure/World_Data_Infrastructure_01.pdf", World_Data_Infrastructure_01, scale=3, height=9, width=15,  device = cairo_pdf(), limitsize = FALSE)



# Another way to visualise these data is the following:

# city_aggregates_02 <- datacenters_data %>%
#   group_by(location, provider_name) %>%
#   summarise(
#     #block = ifelse(length(block) == 1, names(which.max(table(block))), "Mixed"),
#     number_of_datacenters = sum(number_of_datacenters),
#     latitude = min(latitude),  
#     longitude = max(longitude)
#   ) %>%
#   ungroup()

# 
# World_Data_Infrastructure_02 <- ggplot() +
#   geom_polygon(data = world_map, aes(x = long, y = lat, group = group)) +
#   geom_sf(data = cable_data, aes(geometry=geometry), color = "black", size = 1, alpha= 0.6) +
#   geom_point(data = city_aggregates_02, aes(x = longitude, y = latitude, fill = provider_name, size = number_of_datacenters), color= "black", alpha= 0.7, shape=21) + 
#   scale_fill_manual(values = c("Microsoft Azure" = "red", "Google Cloud"= "orange", "Amazon Web Services" = "yellow", "IBM Cloud" = "brown"  , 
#                                "Oracle Cloud" = "sienna4", "Alibaba Cloud"= "dodgerblue4", "Huawei Cloud"= "lightblue", "Tencent Cloud"= "seagreen2" ))+
#   
#   scale_radius(range = c(3, 20))+
#   theme_minimal() +
#   labs(title = "The World Data Network",
#        subtitle = "Subsea Cables and Cloud Datacenters of the main Internet players",
#        fill = "Datacenters Owner's Origin",
#        size = "Number of Datacenters",
#        caption = "Note: data on submarine cables are updated up to year 2022, while the data cloud datacenters are updated up to year 2024"
#   ) +  theme_minimal(base_size = 14) +
#   theme(
#     plot.margin = unit(c(0,40,0,40), units =  "pt"),
#     plot.title = element_text(hjust = 0.5, size = 50, face = "bold"),
#     plot.subtitle = element_text(hjust = 0.5, size = 40, face = "italic"),
#     plot.caption = element_text(hjust = 0.5, size = 30),
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     axis.title = element_blank(),
#     legend.position = "top", 
#     legend.title = element_text(size = 20),
#     legend.key.size = unit(40, "pt"),
#     legend.text = element_text(size = 20),
#     legend.spacing = unit(20, "pt"),              
#     legend.box = "horizontal",                  
#     legend.box.just = "center",
#     panel.background = element_rect(fill = "#F7F7F7")
#   ) +
#   scale_x_continuous(breaks = seq(-180, 180, by = 60)) +
#   scale_y_continuous(breaks = seq(-90, 90, by = 30)) + 
#   guides(fill = guide_legend(override.aes = list(size=20)))
# 
# ggsave("World_Data_Infrastructure_02.pdf", World_Data_Infrastructure_02, scale= 2,height=12, width= 20, device = cairo_pdf())
# 
# 
