################################################
#
##########  GLOBAL FISHING WATCH ###############
#
################################################

#remotes::install_github("GlobalFishingWatch/gfwr", force = TRUE, build_vignettes = TRUE)

# new develop branch that includes new vessel presence API
# remotes::install_github("GlobalFishingWatch/gfwr@major_release",
#                         dependencies = TRUE, build_vignettes = TRUE)

library(gfwr)
library(tidyverse)
library(tidyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(glue)
library(ggplot2)


# SET API -----------------------------------------------------------------

# open R environment:
# usethis::edit_r_environ()
#and paste teh following line to the .Renviron file:
GFW_TOKEN="eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6ImtpZEtleSJ9.eyJkYXRhIjp7Im5hbWUiOiJJbmdvLk1pbGxlcl9QaERfMjAyNSIsInVzZXJJZCI6MjY1NjksImFwcGxpY2F0aW9uTmFtZSI6IkluZ28uTWlsbGVyX1BoRF8yMDI1IiwiaWQiOjI1OTAsInR5cGUiOiJ1c2VyLWFwcGxpY2F0aW9uIn0sImlhdCI6MTc0NTQ1NDk5OCwiZXhwIjoyMDYwODE0OTk4LCJhdWQiOiJnZnciLCJpc3MiOiJnZncifQ.iBBWF91YudweM-9BCIeRDJEgzTNw0n26FRM6Vh5Do2RgTl9s1qONGRVtVn105b9-bOJASXw95yaWH4H9bRHiVmEIUP_zLFB8dikBK9dWWnsHxgze_E4xGoEExnklQ_TuUF5gMYnmJh0GIXE5XNAFyA8MEXS7_7l6_Mtspy9PhiWzkcW-cBARxkcmZ1vdOwFBLTEBfIEUp_0ouN6JfC129Mk2J7mBrhlSyeys888eQW_I9f_kI9ARTITdV-BQ1jT46Wl37Io0LtX-RAfsJDkN4fiQLhKty7Uqd-yGSjN0sWW3md4VXWhXXWW5Xf47Hy3Nbs4bvjEywJyZx99DEYXfcik9I05KavcvF9Nsg-IZYNIaocFXtimZR3ANt2VWUziM009aNpzkXcXZyWYurDIdoEx_t1f_Dm_BPn775Wlu1jJjncFJEwc1q4sZTppSCf6arulDHKPcg7jINC513ERvKPjVjvXBy8WfTNwcNcvr1Smpt0reJXmvTle6MlUHHtxE"

# kall the ID we created:
key <- gfw_auth()
#or this if above does not work:
#key <- Sys.getenv("GFW_TOKEN")






# set target area ---------------------------------------------------------




my_shp <- tibble(
  lon = c(130,130,180,180,130),
  lat = c(-50, 20,20,-50,-50)
) |> 
  sf::st_as_sf(
    coords = c('lon','lat'),
    # crs = "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
    crs = "EPSG:4326"
  ) |> 
  summarize(geometry = st_combine(geometry)) %>% 
  st_cast("POLYGON")



# my_shp <- tibble(
#   lon = c(-180,-180,180,180,-180),
#   lat = c(-80, 80,80,-80,-80)
# ) %>% 
#   sf::st_as_sf(
#     coords = c('lon','lat'),
#     crs = "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
#   ) %>% 
#   summarize(geometry = st_combine(geometry)) %>% 
#   st_cast("POLYGON")





# Get AIS Presence data  --------------------------------------------------

## large vessels, over 4 knots; all combined 

my_filter <- paste(
  "vessel_type IN ('carrier','cargo','passenger','bunker')",
  "AND speed IN ('4-6','6-10','10-15','15-25','>25')",   
  sep = " "
)

my_filter
input_filter <-  my_filter

# start <- "2018-01-01"
# end <- "2018-12-31"
# ais_p_2018 <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                       temporal_resolution = "MONTHLY",
#                                       #group_by = "FLAGANDGEARTYPE",
#                                       filter_by = input_filter,
#                                       start_date = start,
#                                       end_date = end,
#                                       region = my_shp,
#                                       region_source = "USER_SHAPEFILE",
#                                       key = key,
#                                       print_request = TRUE)
# 
# 
# 
# start <- "2019-01-01"
# end <- "2019-12-31"
# ais_p_2019 <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                       temporal_resolution = "MONTHLY",
#                                       #group_by = "FLAGANDGEARTYPE",
#                                       filter_by = input_filter,
#                                       start_date = start,
#                                       end_date = end,
#                                       region = my_shp,
#                                       region_source = "USER_SHAPEFILE",
#                                       key = key,
#                                       print_request = TRUE)
# 
# start <- "2020-01-01"
# end <- "2020-12-31"
# ais_p_2020 <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                       temporal_resolution = "MONTHLY",
#                                       #group_by = "FLAGANDGEARTYPE",
#                                       filter_by = input_filter,
#                                       start_date = start,
#                                       end_date = end,
#                                       region = my_shp,
#                                       region_source = "USER_SHAPEFILE",
#                                       key = key,
#                                       print_request = TRUE)
# 
# start <- "2021-01-01"
# end <- "2021-12-31"
# ais_p_2021 <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                       temporal_resolution = "MONTHLY",
#                                       #group_by = "FLAGANDGEARTYPE",
#                                       filter_by = input_filter,
#                                       start_date = start,
#                                       end_date = end,
#                                       region = my_shp,
#                                       region_source = "USER_SHAPEFILE",
#                                       key = key,
#                                       print_request = TRUE)
# 
# 
# start <- "2022-01-01"
# end <- "2022-12-31"
# ais_p_2022 <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                       temporal_resolution = "MONTHLY",
#                                       #group_by = "FLAGANDGEARTYPE",
#                                       filter_by = input_filter,
#                                       start_date = start,
#                                       end_date = end,
#                                       region = my_shp,
#                                       region_source = "USER_SHAPEFILE",
#                                       key = key,
#                                       print_request = TRUE)
# 
# start <- "2023-01-01"
# end <- "2023-12-31"
# ais_p_2023 <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                       temporal_resolution = "MONTHLY",
#                                       #group_by = "FLAGANDGEARTYPE",
#                                       filter_by = input_filter,
#                                       start_date = start,
#                                       end_date = end,
#                                       region = my_shp,
#                                       region_source = "USER_SHAPEFILE",
#                                       key = key,
#                                       print_request = TRUE)
# 
# start <- "2024-01-01"
# end <- "2024-12-31"
# ais_p_2024 <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                       temporal_resolution = "MONTHLY",
#                                       #group_by = "FLAGANDGEARTYPE",
#                                       filter_by = input_filter,
#                                       start_date = start,
#                                       end_date = end,
#                                       region = my_shp,
#                                       region_source = "USER_SHAPEFILE",
#                                       key = key,
#                                       print_request = TRUE)
# 
# start <- "2025-01-01"
# end <- "2025-08-31"
# ais_p_2025 <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                       temporal_resolution = "MONTHLY",
#                                       #group_by = "FLAGANDGEARTYPE",
#                                       filter_by = input_filter,
#                                       start_date = start,
#                                       end_date = end,
#                                       region = my_shp,
#                                       region_source = "USER_SHAPEFILE",
#                                       key = key,
#                                       print_request = TRUE)
# 
# 
# 
# ais_p_all <- dplyr::bind_rows(
#   ais_p_2018,
#   ais_p_2019,
#   ais_p_2020,
#   ais_p_2021,
#   ais_p_2022,
#   ais_p_2023,
#   ais_p_2024,
#   ais_p_2025
# )
# 
# ais_p_all
# 
# 



# Summary stats  ----------------------------------------------------------





# mean_month_gfw <- ais_p_all |>
#   dplyr::mutate(
#     date = base::as.Date(paste0(`Time Range`, "-01")),
#     days_in_month = lubridate::days_in_month(date)
#   ) |>
#   dplyr::rename(
#     unique_vessels = `Vessel IDs`,
#     total_hours    = `Vessel Presence Hours`,
#     lat = Lat,
#     lon = Lon
#   ) |>
#   dplyr::mutate(
#     hours_per_day = total_hours / days_in_month
#   ) |>
#   dplyr::group_by(lon, lat) |>
#   dplyr::summarise(
#     mean_hours_per_day = base::mean(hours_per_day, na.rm = TRUE),
#     mean_hours = mean_hours_per_day * 30,
#     mean_vessels = base::mean(unique_vessels, na.rm = TRUE),
#     median_vessels = stats::median(unique_vessels, na.rm = TRUE),
#     min_vessels = base::min(unique_vessels, na.rm = TRUE),
#     max_vessels = base::max(unique_vessels, na.rm = TRUE),
#     sd_vessels = stats::sd(unique_vessels, na.rm = TRUE),
#     p90_hours_per_day = stats::quantile(hours_per_day, 0.9, na.rm = TRUE),
#     n_months = dplyr::n(),
#     n_years = dplyr::n_distinct(lubridate::year(date)),
#     .groups = "drop"
#   )















# Plot Traffic Density ----------------------------------------------------




# dark theme 
map_theme_dark <- theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "black", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.text = element_text(color = "black", size = 6),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.key.height = unit(3, "mm"),
    legend.key.width = unit(20, "mm"),
    legend.text = element_text(color = "black", size = 8),
    legend.title = element_text(face = "bold", color = "black", size = 8, hjust = 0.5),
    plot.title = element_text(face = "bold", color = "black", size = 10),
    plot.subtitle = element_text(color = "black", size = 10)
  )

map_effort_dark <- c("#132b43", "#56B1F7", "#FFE873", "#FF5C5C")








mean_month_gfw <- readRDS("data/processed/GFW_SWP_AIS_pres_2018_2025_bigVessels_allSpeeds.rds")

str(mean_month_gfw)


start <- "2018-06-01"
end <- "2025-06-31"
input_dt <- mean_month_gfw
# AUS_region <- c(xmin=135, xmax = 160, ymin = -30, ymax = -5)
AUS_region <- c(xmin=140, xmax = 170, ymin = -40, ymax = 0)

p1 <- ggplot2::ggplot() +
  # grey layer for 1–2 vessels
  # ggplot2::geom_tile(
  #   data = input_dt |> dplyr::filter(mean_vessels %in% c(1, 2)),
  #   mapping = ggplot2::aes(x = lon, y = lat),
  #   fill = "grey5"
  # ) +
  # gradient for >2 vessels
  ggplot2::geom_tile(
    data = input_dt|> dplyr::filter(mean_vessels >= 1),
    mapping = ggplot2::aes(x = lon, y = lat, fill = mean_vessels)
  ) +
  ggplot2::geom_sf(
    data = rnaturalearth::ne_countries(returnclass = "sf", scale = 10),
    color = "grey20", fill = "grey20"
  ) +
  ggplot2::coord_sf(
    xlim = c(AUS_region["xmin"], AUS_region["xmax"]),
    ylim = c(AUS_region["ymin"], AUS_region["ymax"]),
    expand = FALSE
  ) +
  ggplot2::scale_fill_gradientn(
    trans = "log10",
    colors = cmocean::cmocean("thermal")(300),
    na.value = NA,
    labels = scales::comma,
    limits = c(1, max(input_dt$mean_vessels, na.rm = TRUE))
  ) +
  ggplot2::labs(
    title = "Mean Monthly Vessel Density (GFW AIS Presence Data)",
    subtitle = glue::glue("{start} to {end}"),
    fill = "Mean Vessel Density"
  ) +
  map_theme_dark

p1



# print(mean_month_gfw)
# saveRDS(mean_month_gfw, "data/processed/GFW_SWP_AIS_pres_2018_2025_bigVessels_allSpeeds.rds")


# High Risk scenario ------------------------------------------------------


## large vessels, over 10 knots; all combined 

# my_filter_high <- paste(
#   "vessel_type IN ('carrier','cargo','passenger','bunker')",
#   "AND speed IN ('10-15','15-25','>25')",   
#   sep = " "
# )
# 
# my_filter_high
# 
# input_filter <-  my_filter_high
# 
# start <- "2018-06-01"
# end <- "2025-06-31"
# ais_p_2018_h <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                      temporal_resolution = "MONTHLY",
#                                      #group_by = "FLAGANDGEARTYPE",
#                                      filter_by = input_filter,
#                                      start_date = start,
#                                      end_date = end,
#                                      region = my_shp,
#                                      region_source = "USER_SHAPEFILE",
#                                      key = key,
#                                      print_request = TRUE)
# 
# 
# 
# start <- "2019-01-01"
# end <- "2019-12-31"
# ais_p_2019_h <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                      temporal_resolution = "MONTHLY",
#                                      #group_by = "FLAGANDGEARTYPE",
#                                      filter_by = input_filter,
#                                      start_date = start,
#                                      end_date = end,
#                                      region = my_shp,
#                                      region_source = "USER_SHAPEFILE",
#                                      key = key,
#                                      print_request = TRUE)
# 
# start <- "2020-01-01"
# end <- "2020-12-31"
# ais_p_2020_h <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                      temporal_resolution = "MONTHLY",
#                                      #group_by = "FLAGANDGEARTYPE",
#                                      filter_by = input_filter,
#                                      start_date = start,
#                                      end_date = end,
#                                      region = my_shp,
#                                      region_source = "USER_SHAPEFILE",
#                                      key = key,
#                                      print_request = TRUE)
# 
# start <- "2021-01-01"
# end <- "2021-12-31"
# ais_p_2021_h <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                      temporal_resolution = "MONTHLY",
#                                      #group_by = "FLAGANDGEARTYPE",
#                                      filter_by = input_filter,
#                                      start_date = start,
#                                      end_date = end,
#                                      region = my_shp,
#                                      region_source = "USER_SHAPEFILE",
#                                      key = key,
#                                      print_request = TRUE)
# 
# 
# start <- "2022-01-01"
# end <- "2022-12-31"
# ais_p_2022_h <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                      temporal_resolution = "MONTHLY",
#                                      #group_by = "FLAGANDGEARTYPE",
#                                      filter_by = input_filter,
#                                      start_date = start,
#                                      end_date = end,
#                                      region = my_shp,
#                                      region_source = "USER_SHAPEFILE",
#                                      key = key,
#                                      print_request = TRUE)
# 
# start <- "2023-01-01"
# end <- "2023-12-31"
# ais_p_2023_h <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                      temporal_resolution = "MONTHLY",
#                                      #group_by = "FLAGANDGEARTYPE",
#                                      filter_by = input_filter,
#                                      start_date = start,
#                                      end_date = end,
#                                      region = my_shp,
#                                      region_source = "USER_SHAPEFILE",
#                                      key = key,
#                                      print_request = TRUE)
# 
# start <- "2024-01-01"
# end <- "2024-12-31"
# ais_p_2024_h <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                      temporal_resolution = "MONTHLY",
#                                      #group_by = "FLAGANDGEARTYPE",
#                                      filter_by = input_filter,
#                                      start_date = start,
#                                      end_date = end,
#                                      region = my_shp,
#                                      region_source = "USER_SHAPEFILE",
#                                      key = key,
#                                      print_request = TRUE)
# 
# start <- "2025-01-01"
# end <- "2025-08-31"
# ais_p_2025_h <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                      temporal_resolution = "MONTHLY",
#                                      #group_by = "FLAGANDGEARTYPE",
#                                      filter_by = input_filter,
#                                      start_date = start,
#                                      end_date = end,
#                                      region = my_shp,
#                                      region_source = "USER_SHAPEFILE",
#                                      key = key,
#                                      print_request = TRUE)
# 
# 
# 
# ais_p_high <- dplyr::bind_rows(
#   ais_p_2018_h,
#   ais_p_2019_h,
#   ais_p_2020_h,
#   ais_p_2021_h,
#   ais_p_2022_h,
#   ais_p_2023_h,
#   ais_p_2024_h,
#   ais_p_2025_h
# )
# 
# ais_p_high
# 
# 
# mean_month_gfw_h <- ais_p_high |>
#   dplyr::mutate(
#     date = base::as.Date(paste0(`Time Range`, "-01")),
#     days_in_month = lubridate::days_in_month(date)
#   ) |>
#   dplyr::rename(
#     unique_vessels = `Vessel IDs`,
#     total_hours    = `Vessel Presence Hours`,
#     lat = Lat,
#     lon = Lon
#   ) |>
#   dplyr::mutate(
#     hours_per_day = total_hours / days_in_month
#   ) |>
#   dplyr::group_by(lon, lat) |>
#   dplyr::summarise(
#     mean_hours_per_day = base::mean(hours_per_day, na.rm = TRUE),
#     mean_hours = mean_hours_per_day * 30,
#     mean_vessels = base::mean(unique_vessels, na.rm = TRUE),
#     median_vessels = stats::median(unique_vessels, na.rm = TRUE),
#     min_vessels = base::min(unique_vessels, na.rm = TRUE),
#     max_vessels = base::max(unique_vessels, na.rm = TRUE),
#     sd_vessels = stats::sd(unique_vessels, na.rm = TRUE),
#     p90_hours_per_day = stats::quantile(hours_per_day, 0.9, na.rm = TRUE),
#     n_months = dplyr::n(),
#     n_years = dplyr::n_distinct(lubridate::year(date)),
#     .groups = "drop"
#   )




mean_month_gfw_h <- readRDS("data/processed/GFW_SWP_AIS_pres_2018_2025_bigVessels_Speeds10+knots.rds")
start <- "2018-06-01"
end <- "2025-06-31"
input_dt <- mean_month_gfw_h
# AUS_region <- c(xmin=135, xmax = 160, ymin = -30, ymax = -5)
AUS_region <- c(xmin=140, xmax = 170, ymin = -40, ymax = 0)

p_high <- ggplot2::ggplot() +
  # grey layer for 1–2 vessels
  # ggplot2::geom_tile(
  #   data = mean_month_gfw |> dplyr::filter(mean_vessels %in% c(1, 2)),
  #   mapping = ggplot2::aes(x = lon, y = lat),
  #   fill = "grey5"
  # ) +
  # gradient for >2 vessels
  ggplot2::geom_tile(
    data = input_dt |> dplyr::filter(mean_vessels >= 1),
    mapping = ggplot2::aes(x = lon, y = lat, fill = mean_vessels)
  ) +
  ggplot2::geom_sf(
    data = rnaturalearth::ne_countries(returnclass = "sf", scale = 10),
    color = "grey20", fill = "grey20"
  ) +
  ggplot2::coord_sf(
    xlim = c(AUS_region["xmin"], AUS_region["xmax"]),
    ylim = c(AUS_region["ymin"], AUS_region["ymax"]),
    expand = FALSE
  ) +
  ggplot2::scale_fill_gradientn(
    trans = "log10",
    colors = cmocean::cmocean("thermal")(300),
    na.value = NA,
    labels = scales::comma,
    limits = c(1, max(input_dt$mean_vessels, na.rm = TRUE))
  ) +
  ggplot2::labs(
    title = "Mean Monthly Vessel Density >10 knots (GFW AIS Presence Data)",
    subtitle = glue::glue("{start} to {end}"),
    fill = "Mean Vessel Density"
  ) +
  map_theme_dark

p_high
 

library(patchwork)
p1 + p_high


saveRDS(mean_month_gfw_h, "data/processed/GFW_SWP_AIS_pres_2018_2025_bigVessels_Speeds10+knots.rds")


# Low Risk scenario ------------------------------------------------------


## large vessels, over 4 but below 10 knots; all combined 

# my_filter_low <- paste(
#   "vessel_type IN ('carrier','cargo','passenger','bunker')",
#   "AND speed IN ('4-6','6-10')",   
#   sep = " "
# )
# 
# my_filter_low
# 
# input_filter <-  my_filter_low
# input_filter
# 
# start <- "2018-01-01"
# end <- "2018-12-31"
# ais_p_2018_l <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                        temporal_resolution = "MONTHLY",
#                                        #group_by = "FLAGANDGEARTYPE",
#                                        filter_by = input_filter,
#                                        start_date = start,
#                                        end_date = end,
#                                        region = my_shp,
#                                        region_source = "USER_SHAPEFILE",
#                                        key = key,
#                                        print_request = TRUE)
# 
# 
# 
# start <- "2019-01-01"
# end <- "2019-12-31"
# ais_p_2019_l <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                        temporal_resolution = "MONTHLY",
#                                        #group_by = "FLAGANDGEARTYPE",
#                                        filter_by = input_filter,
#                                        start_date = start,
#                                        end_date = end,
#                                        region = my_shp,
#                                        region_source = "USER_SHAPEFILE",
#                                        key = key,
#                                        print_request = TRUE)
# 
# start <- "2020-01-01"
# end <- "2020-12-31"
# ais_p_2020_l <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                        temporal_resolution = "MONTHLY",
#                                        #group_by = "FLAGANDGEARTYPE",
#                                        filter_by = input_filter,
#                                        start_date = start,
#                                        end_date = end,
#                                        region = my_shp,
#                                        region_source = "USER_SHAPEFILE",
#                                        key = key,
#                                        print_request = TRUE)
# 
# start <- "2021-01-01"
# end <- "2021-12-31"
# ais_p_2021_l <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                        temporal_resolution = "MONTHLY",
#                                        #group_by = "FLAGANDGEARTYPE",
#                                        filter_by = input_filter,
#                                        start_date = start,
#                                        end_date = end,
#                                        region = my_shp,
#                                        region_source = "USER_SHAPEFILE",
#                                        key = key,
#                                        print_request = TRUE)
# 
# 
# start <- "2022-01-01"
# end <- "2022-12-31"
# ais_p_2022_l <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                        temporal_resolution = "MONTHLY",
#                                        #group_by = "FLAGANDGEARTYPE",
#                                        filter_by = input_filter,
#                                        start_date = start,
#                                        end_date = end,
#                                        region = my_shp,
#                                        region_source = "USER_SHAPEFILE",
#                                        key = key,
#                                        print_request = TRUE)
# 
# start <- "2023-01-01"
# end <- "2023-12-31"
# ais_p_2023_l <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                        temporal_resolution = "MONTHLY",
#                                        #group_by = "FLAGANDGEARTYPE",
#                                        filter_by = input_filter,
#                                        start_date = start,
#                                        end_date = end,
#                                        region = my_shp,
#                                        region_source = "USER_SHAPEFILE",
#                                        key = key,
#                                        print_request = TRUE)
# 
# start <- "2024-01-01"
# end <- "2024-12-31"
# ais_p_2024_l <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                        temporal_resolution = "MONTHLY",
#                                        #group_by = "FLAGANDGEARTYPE",
#                                        filter_by = input_filter,
#                                        start_date = start,
#                                        end_date = end,
#                                        region = my_shp,
#                                        region_source = "USER_SHAPEFILE",
#                                        key = key,
#                                        print_request = TRUE)
# 
# start <- "2025-01-01"
# end <- "2025-08-31"
# ais_p_2025_l <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
#                                        temporal_resolution = "MONTHLY",
#                                        #group_by = "FLAGANDGEARTYPE",
#                                        filter_by = input_filter,
#                                        start_date = start,
#                                        end_date = end,
#                                        region = my_shp,
#                                        region_source = "USER_SHAPEFILE",
#                                        key = key,
#                                        print_request = TRUE)
# 
# 
# 
# ais_p_low <- dplyr::bind_rows(
#   ais_p_2018_l,
#   ais_p_2019_l,
#   ais_p_2020_l,
#   ais_p_2021_l,
#   ais_p_2022_l,
#   ais_p_2023_l,
#   ais_p_2024_l,
#   ais_p_2025_l
# )
# 
# ais_p_low
# 
# 
# mean_month_gfw_l <- ais_p_low |>
#   dplyr::mutate(
#     date = base::as.Date(paste0(`Time Range`, "-01")),
#     days_in_month = lubridate::days_in_month(date)
#   ) |>
#   dplyr::rename(
#     unique_vessels = `Vessel IDs`,
#     total_hours    = `Vessel Presence Hours`,
#     lat = Lat,
#     lon = Lon
#   ) |>
#   dplyr::mutate(
#     hours_per_day = total_hours / days_in_month
#   ) |>
#   dplyr::group_by(lon, lat) |>
#   dplyr::summarise(
#     mean_hours_per_day = base::mean(hours_per_day, na.rm = TRUE),
#     mean_hours = mean_hours_per_day * 30,
#     mean_vessels = base::mean(unique_vessels, na.rm = TRUE),
#     median_vessels = stats::median(unique_vessels, na.rm = TRUE),
#     min_vessels = base::min(unique_vessels, na.rm = TRUE),
#     max_vessels = base::max(unique_vessels, na.rm = TRUE),
#     sd_vessels = stats::sd(unique_vessels, na.rm = TRUE),
#     p90_hours_per_day = stats::quantile(hours_per_day, 0.9, na.rm = TRUE),
#     n_months = dplyr::n(),
#     n_years = dplyr::n_distinct(lubridate::year(date)),
#     .groups = "drop"
#   )


mean_month_gfw_l <- readRDS("data/processed/GFW_SWP_AIS_pres_2018_2025_bigVessels_Speeds-10knots.rds")

start <- "2018-06-01"
end <- "2025-06-31"
input_dt <- mean_month_gfw_l
# AUS_region <- c(xmin=135, xmax = 160, ymin = -30, ymax = -5)
AUS_region <- c(xmin=140, xmax = 170, ymin = -40, ymax = 0)

p_low <- ggplot2::ggplot() +
  # grey layer for 1–2 vessels
  # ggplot2::geom_tile(
  #   data = mean_month_gfw |> dplyr::filter(mean_vessels %in% c(1, 2)),
  #   mapping = ggplot2::aes(x = lon, y = lat),
  #   fill = "grey5"
  # ) +
  # gradient for >2 vessels
  ggplot2::geom_tile(
    data = mean_month_gfw |> dplyr::filter(mean_vessels >= 1),
    mapping = ggplot2::aes(x = lon, y = lat, fill = mean_vessels)
  ) +
  ggplot2::geom_sf(
    data = rnaturalearth::ne_countries(returnclass = "sf", scale = 10),
    color = "grey20", fill = "grey20"
  ) +
  ggplot2::coord_sf(
    xlim = c(AUS_region["xmin"], AUS_region["xmax"]),
    ylim = c(AUS_region["ymin"], AUS_region["ymax"]),
    expand = FALSE
  ) +
  ggplot2::scale_fill_gradientn(
    trans = "log10",
    colors = cmocean::cmocean("thermal")(300),
    na.value = NA,
    labels = scales::comma,
    limits = c(1, max(input_dt$mean_vessels, na.rm = TRUE))
  ) +
  ggplot2::labs(
    title = "Mean Monthly Vessel Density <10 knots (GFW AIS Presence Data)",
    subtitle = glue::glue("{start} to {end}"),
    fill = "Mean Vessel Density"
  ) +
  map_theme_dark

p_low


library(patchwork)
p1 + p_high + p_low

saveRDS(mean_month_gfw_l, "data/processed/GFW_SWP_AIS_pres_2018_2025_bigVessels_Speeds-10knots.rds")


## save a s rasters

# Raster: mean vessel count
r_vessels_all <- terra::rast(mean_month_gfw[, c("lon", "lat", "mean_vessels")], type = "xyz")
r_vessels_all
terra::crs(r_vessels_all) <- "EPSG:4326"
terra::plot(r_vessels_all, range = c(2, 350))

r_vessels_fast <- terra::rast(mean_month_gfw_h[, c("lon", "lat", "mean_vessels")], type = "xyz")
terra::crs(r_vessels_fast) <- "EPSG:4326"

r_vessels_slow <- terra::rast(mean_month_gfw_l[, c("lon", "lat", "mean_vessels")], type = "xyz")
terra::crs(r_vessels_slow) <- "EPSG:4326"

terra::writeRaster(r_vessels_all, "data/processed/GFW_SWP_AIS_pres_2018_2025_bigVessels.tif", overwrite = TRUE)
terra::writeRaster(r_vessels_fast, "data/processed/GFW_SWP_AIS_pres_2018_2025_bigVessels_Speed10+knots.tif", overwrite = TRUE)
terra::writeRaster(r_vessels_slow, "data/processed/GFW_SWP_AIS_pres_2018_2025_bigVessels_Speed-10knots.tif", overwrite = TRUE)

