################################################
#
##########  GLOBAL FISHING WATCH ###############
#
################################################

remotes::install_github("GlobalFishingWatch/gfwr", force = TRUE, build_vignettes = TRUE)
library(gfwr)
library(tidyverse)
library(tidyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(glue)
library(ggplot2)

# check this page for instructions: 
#https://github.com/GlobalFishingWatch/gfwr
#https://globalfishingwatch.org/data/downloadable-public-data-in-r/


### Set API

# SET API -----------------------------------------------------------------

# open R environment:
# usethis::edit_r_environ()
# and paste teh following line to the .Renviron file:
# GFW_TOKEN="eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6ImtpZEtleSJ9.eyJkYXRhIjp7Im5hbWUiOiJJbmdvLk1pbGxlcl9QaEQiLCJ1c2VySWQiOjI2NTY5LCJhcHBsaWNhdGlvbk5hbWUiOiJJbmdvLk1pbGxlcl9QaEQiLCJpZCI6NzE1LCJ0eXBlIjoidXNlci1hcHBsaWNhdGlvbiJ9LCJpYXQiOjE2ODg0Mjg0OTAsImV4cCI6MjAwMzc4ODQ5MCwiYXVkIjoiZ2Z3IiwiaXNzIjoiZ2Z3In0.Q3_GVbiz09MtGrmH7pbxurfqcRzF-LFwxNcVFviEsr_hQELj0Qt3X9kMIhWk8NAsxAM5TyT7l1ZRdCeiguGwJlI7HrO7PZ9fvhUSK-fy-9r4Ar6LnLrY3o2rtxPnZYRrSqMzYvFJLGBKip6Bpv4su2EmEmPMlmhWO4l0CpJ9tRAHjNkujKHhqgfbWAuiUT8sryerzfr_7oIHobxcZCyj-ogH-TeRUaySHvlHqC8xw-wJxx2HOcIJVxQVIQvpK0nV3Xwq24NqbRg3kcMbv35Uz_7CqtbbB8RbztGsfH2-YSC1LLYmKUFJAa_AtAFP3C3qcsZfjN5JLP4CiF0MnHRHqJuUM1X7T0tcHw6iR_f2Zyi9_yqkDvTNCuLJ4Tu_kYG-5ExeaANNHT8i8WFUzWZLx4I5DR6ri6R6JxcSqF6oqi2EK1RetTBmCgVi6z-wt7sjLbryEYzY9dF06wD4alVol_Ya8Cta37nM-oojGZtafGh2TJ4_27gMQETjK6ZUz7QQ
# "

# kall the ID we created:
key <- gfw_auth()
#or this if above does not work:
#key <- Sys.getenv("GFW_TOKEN")




# Get Shipping Traffic Data -----------------------------------------------


get_event(event_type = 'ENCOUNTER',
          start_date = "2020-01-01",
          end_date = "2020-01-02",
          key = key
)





# Shiiping Traffic Maps ---------------------------------------------------





# Map theme with dark background
map_theme <- ggplot2::theme_minimal() + 
  ggplot2::theme(
    panel.border = element_blank(), 
    legend.position = "bottom", legend.box = "vertical", 
    legend.key.height = unit(3, "mm"), 
    legend.key.width = unit(20, "mm"),
    legend.text = element_text(color = "#848b9b", size = 8), 
    legend.title = element_text(face = "bold", color = "#363c4c", size = 8, hjust = 0.5), 
    plot.title = element_text(face = "bold", color = "#363c4c", size = 10), 
    plot.subtitle = element_text(color = "#363c4c", size = 10), 
    axis.title = element_blank(), 
    axis.text = element_text(color = "#848b9b", size = 6)
  )

# Palette for fishing activity
map_effort_light <- c("#ffffff", "#eeff00", "#3b9088","#0c276c")


start_date <- '2021-01-01'
end_date <- '2021-03-31'



regions <- get_regions()


# Use get_region_id function to get EEZ code for Italy
AUS_EEZ_code <- get_region_id(region_name = "AUS", region_source = "EEZ", key = key)

AUS_EEZ_code <- AUS_EEZ_code %>%  dplyr::filter(label == "Australia")


# Download data for the AUS EEZ
AUS_eez_fish_df <- get_raster(
  spatial_resolution = "HIGH",
  temporal_resolution = "MONTHLY",
  group_by = "GEARTYPE",
  start_date = start_date,
  end_date = end_date,
  region = AUS_EEZ_code$id,
  region_source = "EEZ"
)

AUS_eez_fish_df


# organise by grid cell

eez_fish_all_df <- AUS_eez_fish_df %>% 
  group_by(Lat, Lon) %>% 
  summarize(fishing_hours = sum(`Apparent Fishing Hours`, na.rm = T))


eez_fish_all_df %>% 
  filter(fishing_hours >= 1) %>% 
  ggplot() +
  geom_raster(aes(x = Lon,
                  y = Lat,
                  fill = fishing_hours)) +
  geom_sf(data = ne_countries(returnclass = "sf", scale = "medium")) +
  coord_sf(xlim = c(min(eez_fish_all_df$Lon),max(eez_fish_all_df$Lon)),
           ylim = c(min(eez_fish_all_df$Lat),max(eez_fish_all_df$Lat))) +
  scale_fill_gradientn(
    trans = 'log10',
    colors = map_effort_light, 
    na.value = NA,
    labels = scales::comma) +
  labs(title = "Apparent fishing hours in the Italian EEZ",
       subtitle = glue::glue("{start_date} to {end_date}"),
       fill = "Fishing hours") +
  map_theme



# custom area

my_shp <- tibble(
  lon = c(136,136,160,160,136),
  lat = c(-30,-5,-5,-30,-30)
) %>% 
  sf::st_as_sf(
    coords = c('lon','lat'),
    crs = "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  ) %>% 
  summarize(geometry = st_combine(geometry)) %>% 
  st_cast("POLYGON")

ggplot() +
  geom_sf(data = ne_countries(returnclass = "sf", scale = "small")) +
  geom_sf(
    data = my_shp, 
    fill = NA,
    color = 'red') +
  map_theme


my_shp_bbox <- st_bbox(my_shp)


my_raster_df <- get_raster(
  spatial_resolution = "LOW",
  temporal_resolution = "YEARLY",
  group_by = "GEARTYPE",
  start_date = start_date,
  end_date = end_date,
  region = my_shp,
  region_source = "USER_SHAPEFILE"
)


my_port_events_df <- get_event(event_type = "PORT_VISIT",
                               confidences = 4,
                               vessel_types = "FISHING",
                               start_date = start_date,
                               end_date = end_date,
                               region = my_shp,
                               region_source = "USER_SHAPEFILE") 

my_loitering_events_df <- get_event(event_type = "LOITERING",
                                    vessel_types = "CARRIER",
                                    start_date = start_date,
                                    end_date = end_date,
                                    region = my_shp,
                                    region_source = "USER_SHAPEFILE") 




my_raster_all_df <- my_raster_df %>% 
  group_by(Lat, Lon) %>% 
  summarize(fishing_hours = sum(`Apparent Fishing Hours`, na.rm = T))



# combine events

my_events_sf <- my_port_events_df %>% 
  select(id, lon, lat, type) %>% 
  bind_rows(
    my_loitering_events_df %>% 
      select(id, lon, lat, type)
  ) %>% 
  sf::st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
  select(id, type, geometry)


my_raster_all_df %>% 
  filter(fishing_hours > 1) %>% 
  ggplot() +
  geom_raster(aes(x = Lon,
                  y = Lat,
                  fill = fishing_hours)) +
  geom_sf(data = my_events_sf,
          aes(color = type),
          alpha = 0.7) +
  geom_sf(data = ne_countries(returnclass = 'sf', scale = 'medium')) +
  coord_sf(xlim = my_shp_bbox[c(1,3)],
           ylim = my_shp_bbox[c(2,4)]) +
  scale_fill_gradientn(
    transform = 'log10',
    colors = map_effort_light, 
    na.value = NA) +
  labs(
    title = 'Fishing hours, loitering events, and port visits',
    subtitle = glue("{start_date} to {end_date}"),
    fill = 'Fishing hours',
    color = 'Event type'
  ) +
  map_theme





