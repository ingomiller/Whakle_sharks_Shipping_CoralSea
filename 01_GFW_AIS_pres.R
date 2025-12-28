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
    p90_hours_per_day = stats::quantile(hours_per_day, 0.9, na.rm = TRUE),
    n_months = dplyr::n(),
    n_years = dplyr::n_distinct(lubridate::year(date)),
    .groups = "drop"
  )















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






# Stats -------------------------------------------------------------------

# Step 5: Calculate monthly mean per cell

ais_p_all

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


monthly_summary <- all_merged |> 
  dplyr::group_by(month, lon, lat) |> 
  dplyr::summarise(
    min_vessels = min(unique_vessels, na.rm = TRUE),
    mean_vessels = mean(unique_vessels, na.rm = TRUE),
    max_vessels = max(unique_vessels, na.rm = TRUE),
    min_hours = min(total_hours, na.rm = TRUE),
    mean_hours = mean(total_hours, na.rm = TRUE),
    max_hours = max(total_hours, na.rm = TRUE),
    .groups = "drop"
  )

monthly_summary

# check for monthly variations in shipping traffic in vicinity of Wreck Bay

all_merged |> 
  dplyr::filter(lat >= -17, lat <= -12, 
                lon >= 143, lon <= 146) |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    min_vessels = min(unique_vessels, na.rm = TRUE),
    mean_vessels = mean(unique_vessels, na.rm = TRUE),
    max_vessels = max(unique_vessels, na.rm = TRUE),
    .groups = "drop"
  )

#check for statisitacl significance 

library(rstatix)

all_merged |>
  dplyr::filter(lat >= -17, lat <= -12,
                lon >= 142.7, lon <= 146.5) |> 
  dplyr::mutate(month = factor(month)) |>
  rstatix::kruskal_test(unique_vessels ~ month) |>
  rstatix::add_significance()

all_merged |>
  dplyr::filter(month %in% c(10, 11, 12, 1, 2, 3)) |> 
  dplyr::filter(lat >= -17, lat <= -12,
                lon >= 143, lon <= 146) |> 
  dplyr::mutate(month = factor(month)) |>
  rstatix::pairwise_wilcox_test(
    formula = unique_vessels ~ month,
    p.adjust.method = "BH"  # Benjamini-Hochberg correction
  )




start <- all_merged %>% dplyr::arrange(year, month) %>% dplyr::slice(1) %>% dplyr::mutate(date = paste(year)) %>% pull(date)
end <- all_merged %>% dplyr::arrange(desc(year), desc(month)) %>% dplyr::slice(1) %>% dplyr::mutate(date = paste(year)) %>% pull(date)


# vessel hour density
AUS_region <- c(xmin=135, xmax = 160, ymin = -30, ymax = -5)
ggplot(data = mean_summary) +
  geom_tile(aes(x = lon, y = lat, fill = mean_hours)) +
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
    title = "Mean Monthly Vessel Hours (AMSA Data)",
    subtitle = glue::glue("{start} to {end}"),
    fill = "Hours"
  ) +
  map_theme_dark

# vessel desnity
AUS_region <- c(xmin=135, xmax = 160, ymin = -30, ymax = -5)
AUS_region <- c(xmin=135, xmax = 180, ymin = -45, ymax = 5)
ggplot(data = mean_summary) +
  geom_tile(aes(x = lon, y = lat, fill = mean_vessels)) +
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
    title = "Mean Monthly Vessel Density (AMSA Data)",
    subtitle = glue::glue("{start} to {end}"),
    fill = "Count"
  ) +
  map_theme_dark




# Raster: mean vessel count
r_vessels <- terra::rast(mean_summary[, c("lon", "lat", "mean_vessels")], type = "xyz")
crs(r_vessels) <- "EPSG:4326"
writeRaster(r_vessels, "Shipping_MeanVessels_AUS_2018-2025.tif", overwrite = TRUE)

# Raster: mean vessel hours
r_hours <- terra::rast(mean_summary[, c("lon", "lat", "mean_hours")], type = "xyz")
crs(r_hours) <- "EPSG:4326"
writeRaster(r_hours, "Shipping_MeanHours_AUS_2018-2025.tif", overwrite = TRUE)





# are restricted info and stats  ------------------------------------------

process_shapefile_target_area <- function(shp_path, vessel_exclude_patterns, out_dir,
                                          lat_range = c(-17, -12), lon_range = c(142.7, 146.7)) {
  tryCatch({
    file_name <- tools::file_path_sans_ext(basename(shp_path))
    out_file <- file.path(out_dir, paste0(file_name, "_target_processed.csv"))
    if (file.exists(out_file)) {
      message("Skipping (already processed): ", out_file)
      return(out_file)
    }
    
    sf_obj <- sf::st_read(shp_path, quiet = TRUE)
    
    if ("TMESTAMP" %in% names(sf_obj) && !"TIMESTAMP" %in% names(sf_obj)) {
      names(sf_obj)[names(sf_obj) == "TMESTAMP"] <- "TIMESTAMP"
    }
    
    df <- sf::st_read(shp_path, quiet = TRUE) |> as.data.frame()
    
    timestamp_col <- grep("^t[ia]{1,2}m[e]?s?t?a?m?p?$", names(df), ignore.case = TRUE, value = TRUE)
    if (length(timestamp_col) == 1) {
      names(df)[names(df) == timestamp_col] <- "TIMESTAMP"
    } else {
      stop("No valid TIMESTAMP column found in ", basename(shp_path))
    }
    
    df <- df |>
      dplyr::filter(!grepl(paste(vessel_exclude_patterns, collapse = "|"), TYPE, ignore.case = TRUE)) |>
      dplyr::filter(LENGTH >= 30)
    
    df$TIMESTAMP <- suppressWarnings(lubridate::dmy_hms(df$TIMESTAMP, tz = "UTC"))
    df <- dplyr::arrange(df, CRAFT_ID, TIMESTAMP)
    
    df <- df |>
      dplyr::mutate(
        time_diff = as.numeric(difftime(lead(TIMESTAMP), TIMESTAMP, units = "hours")),
        dist_km = geosphere::distHaversine(cbind(LON, LAT), cbind(lead(LON), lead(LAT))) / 1000,
        time_diff = ifelse(time_diff < 0 | time_diff > 6 | dist_km > 30, NA, time_diff)
      )
    
    # Filter to target area
    df <- df |>
      dplyr::filter(LAT >= lat_range[1], LAT <= lat_range[2],
                    LON >= lon_range[1], LON <= lon_range[2])
    
    # Summarise over the *entire area* (not grid cells)
    summary_df <- df |> 
      dplyr::summarise(
        unique_vessels = dplyr::n_distinct(CRAFT_ID),
        total_hours = sum(time_diff, na.rm = TRUE)
      )
    
    # Add metadata
    year <- as.integer(stringr::str_extract(file_name, "20\\d{2}"))
    month <- as.integer(stringr::str_extract(file_name, "(?<!\\d)(0[1-9]|1[0-2])(?!\\d)"))
    summary_df$year <- year
    summary_df$month <- month
    summary_df$source_file <- basename(shp_path)
    
    readr::write_csv(summary_df, out_file)
    message("Processed and saved: ", out_file)
    return(out_file)
    
  }, error = function(e) {
    message("Failed: ", basename(shp_path), " - ", e$message)
    return(NULL)
  })
}


output_folder <- "target_area_monthly"
dir.create(output_folder, showWarnings = FALSE)

results_paths <- purrr::map(
  shp_files,
  ~ process_shapefile_target_area(.x, vessel_exclude_patterns, output_folder)
) |> purrr::compact()



monthly_transits <- readr::read_csv(
  list.files("target_area_monthly", full.names = TRUE, pattern = "_target_processed.csv$")
) |> 
  dplyr::arrange(year, month)


monthly_transits

# Step 1: Compute the mean vessel count for each month across all years
monthly_means <- monthly_transits |>
  dplyr::group_by(year, month) |>
  dplyr::summarise(
    mean_vessels = mean(unique_vessels, na.rm = TRUE),
    .groups = "drop"
  )


monthly_transits |>
  dplyr::mutate(month = factor(month)) |>
  rstatix::kruskal_test(unique_vessels ~ month) |>
  rstatix::add_significance()

monthly_transits |>
  dplyr::mutate(month = factor(month)) |>
  rstatix::pairwise_wilcox_test(
    formula = unique_vessels ~ month,
    p.adjust.method = "BH"  
  ) |> 
  print(n=100)


monthly_transits |>
  dplyr::filter(month %in% c(10, 11, 12, 1, 2, 3)) |> 
  dplyr::mutate(month = factor(month)) |>
  rstatix::kruskal_test(unique_vessels ~ month) |>
  rstatix::add_significance()

monthly_transits |>
  dplyr::filter(month %in% c(10, 11, 12, 1, 2, 3)) |> 
  dplyr::mutate(month = factor(month)) |>
  rstatix::pairwise_wilcox_test(
    formula = unique_vessels ~ month,
    p.adjust.method = "BH" 
  )


monthly_transits |>
  dplyr::filter(month %in% c(10, 11, 12, 1, 2, 3)) |> 
  dplyr::group_by(month) |>
  rstatix::get_summary_stats(unique_vessels, type = "common")

monthly_transits |>
  dplyr::filter(month %in% c(10, 11, 12, 1, 2, 3)) |> 
  rstatix::get_summary_stats(unique_vessels, type = "common")

monthly_transits |>
  dplyr::filter(month %in% c(11, 12)) |> 
  dplyr::group_by(month) |>
  rstatix::get_summary_stats(unique_vessels, type = "common")

monthly_transits |>
  rstatix::get_summary_stats(unique_vessels, type = "common")











