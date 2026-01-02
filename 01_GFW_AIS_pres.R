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


citation("gfwr")


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

start <- "2018-01-01"
end <- "2018-12-31"
ais_p_2018 <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
                                      temporal_resolution = "MONTHLY",
                                      # group_by = "VESSEL_ID",
                                      filter_by = input_filter,
                                      start_date = start,
                                      end_date = end,
                                      region = my_shp,
                                      region_source = "USER_SHAPEFILE",
                                      key = key,
                                      print_request = TRUE)


ais_p_2018


start <- "2019-01-01"
end <- "2019-12-31"
ais_p_2019 <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
                                      temporal_resolution = "MONTHLY",
                                      #group_by = "FLAGANDGEARTYPE",
                                      filter_by = input_filter,
                                      start_date = start,
                                      end_date = end,
                                      region = my_shp,
                                      region_source = "USER_SHAPEFILE",
                                      key = key,
                                      print_request = TRUE)

start <- "2020-01-01"
end <- "2020-12-31"
ais_p_2020 <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
                                      temporal_resolution = "MONTHLY",
                                      #group_by = "FLAGANDGEARTYPE",
                                      filter_by = input_filter,
                                      start_date = start,
                                      end_date = end,
                                      region = my_shp,
                                      region_source = "USER_SHAPEFILE",
                                      key = key,
                                      print_request = TRUE)

start <- "2021-01-01"
end <- "2021-12-31"
ais_p_2021 <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
                                      temporal_resolution = "MONTHLY",
                                      #group_by = "FLAGANDGEARTYPE",
                                      filter_by = input_filter,
                                      start_date = start,
                                      end_date = end,
                                      region = my_shp,
                                      region_source = "USER_SHAPEFILE",
                                      key = key,
                                      print_request = TRUE)


start <- "2022-01-01"
end <- "2022-12-31"
ais_p_2022 <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
                                      temporal_resolution = "MONTHLY",
                                      #group_by = "FLAGANDGEARTYPE",
                                      filter_by = input_filter,
                                      start_date = start,
                                      end_date = end,
                                      region = my_shp,
                                      region_source = "USER_SHAPEFILE",
                                      key = key,
                                      print_request = TRUE)

start <- "2023-01-01"
end <- "2023-12-31"
ais_p_2023 <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
                                      temporal_resolution = "MONTHLY",
                                      #group_by = "FLAGANDGEARTYPE",
                                      filter_by = input_filter,
                                      start_date = start,
                                      end_date = end,
                                      region = my_shp,
                                      region_source = "USER_SHAPEFILE",
                                      key = key,
                                      print_request = TRUE)

start <- "2024-01-01"
end <- "2024-12-31"
ais_p_2024 <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
                                      temporal_resolution = "MONTHLY",
                                      #group_by = "FLAGANDGEARTYPE",
                                      filter_by = input_filter,
                                      start_date = start,
                                      end_date = end,
                                      region = my_shp,
                                      region_source = "USER_SHAPEFILE",
                                      key = key,
                                      print_request = TRUE)

start <- "2025-01-01"
end <- "2025-08-31"
ais_p_2025 <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
                                      temporal_resolution = "MONTHLY",
                                      #group_by = "FLAGANDGEARTYPE",
                                      filter_by = input_filter,
                                      start_date = start,
                                      end_date = end,
                                      region = my_shp,
                                      region_source = "USER_SHAPEFILE",
                                      key = key,
                                      print_request = TRUE)



ais_p_all <- dplyr::bind_rows(
  ais_p_2018,
  ais_p_2019,
  ais_p_2020,
  ais_p_2021,
  ais_p_2022,
  ais_p_2023,
  ais_p_2024,
  ais_p_2025
)

ais_p_all


saveRDS(ais_p_all, "data/processed/GFW_SWP_AIS_pres_2018_2025_big_vessels_4knots+.rds")


# Summary stats  ----------------------------------------------------------





mean_month_gfw <- ais_p_all |>
  dplyr::mutate(
    date = base::as.Date(paste0(`Time Range`, "-01")),
    days_in_month = lubridate::days_in_month(date)
  ) |>
  dplyr::rename(
    unique_vessels = `Vessel IDs`,
    total_hours    = `Vessel Presence Hours`,
    lat = Lat,
    lon = Lon
  ) |>
  dplyr::mutate(
    hours_per_day = total_hours / days_in_month
  ) |>
  dplyr::group_by(lon, lat) |>
  dplyr::summarise(
    mean_hours_per_day = base::mean(hours_per_day, na.rm = TRUE),
    mean_hours = mean_hours_per_day * 30,
    mean_vessels = base::mean(unique_vessels, na.rm = TRUE),
    median_vessels = stats::median(unique_vessels, na.rm = TRUE),
    min_vessels = base::min(unique_vessels, na.rm = TRUE),
    max_vessels = base::max(unique_vessels, na.rm = TRUE),
    sd_vessels = stats::sd(unique_vessels, na.rm = TRUE),
    q90_hours_per_day = stats::quantile(hours_per_day, 0.9, na.rm = TRUE),
    n_months = dplyr::n(),
    n_years = dplyr::n_distinct(lubridate::year(date)),
    .groups = "drop"
  )


mean_month_gfw












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

input_dt <- mean_month_gfw


ggplot2::ggplot() +
  ggplot2::geom_tile(
    data = input_dt|> dplyr::filter(mean_vessels >= 1),
    mapping = ggplot2::aes(x = lon, y = lat, fill = mean_vessels)
  ) +
  # ggplot2::geom_sf(
  #   data = rnaturalearth::ne_countries(returnclass = "sf", scale = 10),
  #   color = "grey20", fill = "grey20"
  # ) +
  ggplot2::coord_sf(
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






start <- "2018-06-01"
end <- "2025-06-31"

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






# Stats -------------------------------------------------------------------

# Step 5: Calculate monthly mean per cell

ais_p_all

ais_p_all <- ais_p_all |>
  dplyr::mutate(
    month = as.integer(stringr::str_sub(`Time Range`, 6, 7)),
    year = as.integer(stringr::str_sub(`Time Range`, 1, 4))
  )

mean_summary <- ais_p_all |> 
  dplyr::group_by(Lon, Lat) |> 
  dplyr::summarise(
    min_vessels = min(`Vessel IDs`, na.rm = TRUE),
    mean_vessels = mean(`Vessel IDs`, na.rm = TRUE),
    max_vessels = max(`Vessel IDs`, na.rm = TRUE),
    min_hours = min(`Vessel Presence Hours`, na.rm = TRUE),
    mean_hours = mean(`Vessel Presence Hours`, na.rm = TRUE),
    max_hours = max(`Vessel Presence Hours`, na.rm = TRUE),
    .groups = "drop"
  )

mean_summary

glimpse(mean_summary)


monthly_summary <- ais_p_all |> 
  dplyr::group_by(month, Lon, Lat) |> 
  dplyr::summarise(
    min_vessels = min(`Vessel IDs`, na.rm = TRUE),
    mean_vessels = mean(`Vessel IDs`, na.rm = TRUE),
    max_vessels = max(`Vessel IDs`, na.rm = TRUE),
    min_hours = min(`Vessel Presence Hours`, na.rm = TRUE),
    mean_hours = mean(`Vessel Presence Hours`, na.rm = TRUE),
    max_hours = max(`Vessel Presence Hours`, na.rm = TRUE),
    .groups = "drop"
  )

monthly_summary

# check for monthly variations in shipping traffic in vicinity of Wreck Bay (NOTE: this is stats per grid average grid cell!!!)

ais_p_all |> 
  dplyr::filter(Lat >= -17, Lat <= -12, 
                Lon >= 142.7, Lon <= 146.7) |>
  dplyr::group_by(month) |>
  rstatix::get_summary_stats(`Vessel IDs`, type = "common")




#overall mean ship density around Wreck Bay
ais_p_all |> 
  dplyr::filter(Lat >= -17, Lat <= -12, 
                Lon >= 142.7, Lon <= 146.7) |>
  rstatix::get_summary_stats(`Vessel IDs`, type = "common")



area_month <- ais_p_all |>
  dplyr::filter(Lat >= -17, Lat <= -12,
                Lon >= 142.7, Lon <= 146.7) |>
  dplyr::group_by(year, month) |>
  dplyr::summarise(
    total_presence_hours = sum(`Vessel Presence Hours`, na.rm = TRUE),
    .groups = "drop"
  )

area_month

area_month |>
  rstatix::get_summary_stats(total_presence_hours, type = "common")





#check for statisitacl significance 

library(rstatix)

ais_p_all |>
  dplyr::filter(Lat >= -17, Lat <= -12,
                Lon >= 142.7, Lon <= 146.7) |> 
  dplyr::mutate(month = factor(month)) |>
  rstatix::kruskal_test(`Vessel IDs` ~ month) |>
  rstatix::add_significance()

ais_p_all |>
  dplyr::filter(month %in% c(10, 11, 12, 1, 2, 3)) |> 
  dplyr::filter(Lat >= -17, Lat <= -12,
                Lon >= 142.7, Lon <= 146.7) |> 
  dplyr::mutate(month = factor(month)) |>
  rstatix::pairwise_wilcox_test(
    formula = `Vessel IDs` ~ month,
    p.adjust.method = "BH"  # Benjamini-Hochberg correction
  )




start <- ais_p_all |>  dplyr::arrange(year, month) |>  dplyr::slice(1) |>  dplyr::mutate(date = paste(year)) |>  pull(date)
end <- ais_p_all |>  dplyr::arrange(desc(year), desc(month)) |>  dplyr::slice(1) |>  dplyr::mutate(date = paste(year)) |>  pull(date)


# vessel hour density
AUS_region <- c(xmin=140, xmax = 170, ymin = -40, ymax = 0)
ggplot(data = mean_summary) +
  geom_tile(aes(x = Lon, y = Lat, fill = mean_hours)) +
  geom_sf(data = rnaturalearth::ne_countries(returnclass = "sf", scale = 10),
          color = "grey20", fill = "grey20") +
  coord_sf(xlim = c(AUS_region["xmin"], AUS_region["xmax"]),
           ylim = c(AUS_region["ymin"], AUS_region["ymax"])) +
  scale_fill_gradientn(
    trans = 'log10',
    colors = cmocean::cmocean("thermal")(300),
    na.value = NA,
    labels = scales::comma,
    limits = c(1, max(mean_summary$mean_hours, na.rm = TRUE))
  ) +
  labs(
    title = "Mean Monthly Vessel Hours (GFW AIS Presence Data)",
    subtitle = glue::glue("{start} to {end}"),
    fill = "Hours"
  ) +
  map_theme_dark

# vessel desnity
AUS_region <- c(xmin=140, xmax = 170, ymin = -40, ymax = 0)
ggplot(data = mean_summary) +
  geom_tile(aes(x = Lon, y = Lat, fill = mean_vessels)) +
  geom_sf(data = rnaturalearth::ne_countries(returnclass = "sf", scale = 10),
          color = "grey20", fill = "grey20") +
  coord_sf(xlim = c(AUS_region["xmin"], AUS_region["xmax"]),
           ylim = c(AUS_region["ymin"], AUS_region["ymax"]),
           expand = FALSE) +
  scale_fill_gradientn(
    trans = 'log10',
    colors = cmocean::cmocean("thermal")(300),
    na.value = NA,
    labels = scales::comma,
    limits = c(1, max(mean_summary$mean_vessels, na.rm = TRUE))
  ) +
  labs(
    title = "Mean Monthly Vessel Density (GFW AIS Presence Data)",
    subtitle = glue::glue("{start} to {end}"),
    fill = "Count"
  ) +
  map_theme_dark




# Raster: mean vessel count
mean_summary
r_vessels <- terra::rast(mean_summary[, c("Lon", "Lat", "mean_vessels")], type = "xyz")
crs(r_vessels) <- "EPSG:4326"
r_vessels
writeRaster(r_vessels, "GFW_Shipping_MeanVessels_AUS_2018-2025.tif", overwrite = TRUE)

# Raster: mean vessel hours
r_hours <- terra::rast(mean_summary[, c("Lon", "Lat", "mean_hours")], type = "xyz")
crs(r_hours) <- "EPSG:4326"
writeRaster(r_hours, "GFW_Shipping_MeanHours_AUS_2018-2025.tif", overwrite = TRUE)

r_vessels



# Stats on high traffic cells (shippign lanes) ----------------------------


cell_res  <- 0.1  # your grid resolution


study_lat_range <- c(-40, 0)
study_lon_range <- c(140, 170)


lane_grid_all <- ais_p_all |>
  dplyr::filter(
    Lat >= study_lat_range[1], Lat <= study_lat_range[2],
    Lon >= study_lon_range[1], Lon <= study_lon_range[2]
  ) |>
  dplyr::group_by(Lat, Lon) |>
  dplyr::summarise(
    mean_presence_hours = base::mean(`Vessel Presence Hours`, na.rm = TRUE),
    mean_vessel_ids     = base::mean(`Vessel IDs`, na.rm = TRUE),
    .groups = "drop"
  )

q90_thr_all <- stats::quantile(lane_grid_all$mean_vessel_ids, probs = 0.90, na.rm = TRUE)

lane_grid_all <- lane_grid_all |>
  dplyr::mutate(lane_q90 = mean_vessel_ids >= q90_thr_all)

lane_cells_all <- lane_grid_all |>
  dplyr::filter(lane_q90) |>
  dplyr::select(Lat, Lon)


ggplot2::ggplot(lane_grid_all, ggplot2::aes(x = Lon, y = Lat)) +
  ggplot2::geom_tile(
    ggplot2::aes(fill = lane_q90),
    width = cell_res, height = cell_res
  ) +
  ggplot2::coord_equal(expand = FALSE) +
  ggplot2::labs(
    title = "Binary mask: top 10% traffic cells (Q90)",
    fill  = "Lane cell"
  ) +
  ggplot2::theme_bw()


lane_monthly_cellstats <- ais_p_all |>
  dplyr::filter(
    Lat >= study_lat_range[1], Lat <= study_lat_range[2],
    Lon >= study_lon_range[1], Lon <= study_lon_range[2]
  ) |>
  dplyr::inner_join(lane_cells_all, by = c("Lat", "Lon")) |>
  dplyr::group_by(year, month) |>
  dplyr::summarise(
    mean_vessels_per_cell = base::mean(`Vessel IDs`, na.rm = TRUE),
    sd_vessels_per_cell   = stats::sd(`Vessel IDs`, na.rm = TRUE),
    mean_presence_hours   = base::mean(`Vessel Presence Hours`, na.rm = TRUE),
    sd_presence_hours     = stats::sd(`Vessel Presence Hours`, na.rm = TRUE),
    n_cells               = dplyr::n(),
    .groups = "drop"
  )


lane_monthly_cellstats


lane_monthly_cellstats |>
  rstatix::get_summary_stats(mean_vessels_per_cell, type = "common")


lane_climatology <- lane_monthly_cellstats |>
  dplyr::group_by(month) |>
  rstatix::get_summary_stats(mean_vessels_per_cell, type = "common")

lane_climatology



# significant differences acrooss months?
lane_monthly_cellstats |>
  # dplyr::filter(month %in% c(10, 11, 12, 1, 2, 3)) |>
  dplyr::mutate(month = base::factor(month, levels = 1:12)) |>
  rstatix::kruskal_test(mean_vessels_per_cell ~ month)


lane_monthly_cellstats |>
  dplyr::mutate(month = base::factor(month, levels = 1:12)) |>
  rstatix::pairwise_wilcox_test(
    formula = mean_vessels_per_cell ~ month,
    p.adjust.method = "BH"
  )

lane_monthly_cellstats |>
  dplyr::filter(month %in% c(10, 11, 12, 1, 2, 3)) |>
  dplyr::mutate(month = factor(month, levels = c(10, 11, 12, 1, 2, 3))) |>
  rstatix::pairwise_wilcox_test(
    formula = mean_vessels_per_cell ~ month,
    p.adjust.method = "BH"
  )




## Wreck Bay region

# Far North
roi_lat_range <- c(-17, -11)
roi_lon_range <- c(142.7, 146.5)

# PNG
roi_lat_range <- c(-13.05, -2.95)
roi_lon_range <- c(143.5, 156.5)




lane_cells_roi <- lane_cells_all |>
  dplyr::filter(
    Lat >= roi_lat_range[1], Lat <= roi_lat_range[2],
    Lon >= roi_lon_range[1], Lon <= roi_lon_range[2]
  )


ggplot2::ggplot(
  lane_grid_all |>
    dplyr::filter(
      Lat >= roi_lat_range[1], Lat <= roi_lat_range[2],
      Lon >= roi_lon_range[1], Lon <= roi_lon_range[2]
    ),
  ggplot2::aes(x = Lon, y = Lat)
) +
  ggplot2::geom_tile(
    ggplot2::aes(fill = lane_q90),
    width = cell_res, height = cell_res
  ) +
  ggplot2::coord_equal(expand = FALSE) +
  ggplot2::labs(
    title = "Binary mask: top 10% traffic cells (Q90) — ROI",
    fill  = "Lane cell"
  ) +
  ggplot2::theme_bw()





lane_monthly_cellstats <- ais_p_all |>
  dplyr::filter(
    Lat >= roi_lat_range[1], Lat <= roi_lat_range[2],
    Lon >= roi_lon_range[1], Lon <= roi_lon_range[2]
  ) |>
  dplyr::inner_join(lane_cells_roi, by = c("Lat", "Lon")) |>
  dplyr::group_by(year, month) |>
  dplyr::summarise(
    mean_vessels_per_cell = base::mean(`Vessel IDs`, na.rm = TRUE),
    sd_vessels_per_cell   = stats::sd(`Vessel IDs`, na.rm = TRUE),
    mean_presence_hours   = base::mean(`Vessel Presence Hours`, na.rm = TRUE),
    sd_presence_hours     = stats::sd(`Vessel Presence Hours`, na.rm = TRUE),
    n_cells               = dplyr::n(),
    .groups = "drop"
  )


lane_monthly_cellstats


lane_monthly_cellstats |>
  rstatix::get_summary_stats(mean_vessels_per_cell, type = "common")


lane_climatology <- lane_monthly_cellstats |>
  dplyr::group_by(month) |>
  rstatix::get_summary_stats(mean_vessels_per_cell, type = "common")

lane_climatology






lane_monthly_cellstats |>
  # dplyr::filter(month %in% c(10, 11, 12, 1, 2, 3)) |>
  dplyr::mutate(month = base::factor(month, levels = 1:12)) |>
  rstatix::kruskal_test(mean_vessels_per_cell ~ month)


lane_monthly_cellstats |>
  dplyr::mutate(month = base::factor(month, levels = 1:12)) |>
  rstatix::pairwise_wilcox_test(
    formula = mean_vessels_per_cell ~ month,
    p.adjust.method = "BH"
  )

lane_monthly_cellstats |>
  dplyr::filter(month %in% c(10, 11, 12, 1, 2, 3)) |>
  dplyr::mutate(month = factor(month, levels = c(10, 11, 12, 1, 2, 3))) |>
  rstatix::pairwise_wilcox_test(
    formula = mean_vessels_per_cell ~ month,
    p.adjust.method = "BH"
  )





# obverall stats on max values as well

ais_cell_mean <- ais_p_all |>
  dplyr::group_by(Lat, Lon) |>
  dplyr::summarise(
    mean_vessels = base::mean(`Vessel IDs`, na.rm = TRUE),
    .groups = "drop"
  )

# Range / max of the cell means (this should match ships_gfw max)
ais_cell_mean |>
  dplyr::summarise(
    min_mean = min(mean_vessels, na.rm = TRUE),
    max_mean = max(mean_vessels, na.rm = TRUE),
    mean_mean = mean(mean_vessels, na.rm = TRUE),
    sd_mean = sd(mean_vessels, na.rm = TRUE)
  )

# Compare directly to the raster stats
terra::global(ships_gfw, "range", na.rm = TRUE)




# 3) (Optional) This is NOT what ships_gfw is, but useful to understand hotspots:
#    maximum vessels observed in each cell in any month
ais_cell_max <- ais_p_all |>
  dplyr::group_by(Lat, Lon) |>
  dplyr::summarise(
    max_vessels_any_month = max(`Vessel IDs`, na.rm = TRUE),
    .groups = "drop"
  )


ais_cell_max

ais_cell_max |>
  dplyr::summarise(
    max_of_cell_max = max(max_vessels_any_month, na.rm = TRUE)
  )


max_hits <- ais_p_all |>
  dplyr::filter(!is.na(`Vessel IDs`)) |>
  dplyr::slice_max(order_by = `Vessel IDs`, n = 1, with_ties = TRUE) |>
  dplyr::arrange(`Time Range`, dplyr::desc(`Vessel IDs`))

max_hits


top_cell <- ais_p_all |>
  dplyr::group_by(Lat, Lon) |>
  dplyr::summarise(mean_vessels = base::mean(`Vessel IDs`, na.rm = TRUE), .groups = "drop") |>
  dplyr::slice_max(mean_vessels, n = 1, with_ties = TRUE) |>
  dplyr::select(Lat, Lon)

# 2) Within that cell, find the month-year with the maximum Vessel IDs
top_cell_peak_month <- ais_p_all |>
  dplyr::semi_join(top_cell, by = c("Lat", "Lon")) |>
  dplyr::slice_max(`Vessel IDs`, n = 10, with_ties = TRUE) |>
  dplyr::arrange(`Time Range`)

top_cell_peak_month



