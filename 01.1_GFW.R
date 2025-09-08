
################################################
#
##########  GLOBAL FISHING WATCH ###############
#
################################################

#remotes::install_github("GlobalFishingWatch/gfwr", force = TRUE, build_vignettes = TRUE)

# new develop branch that includes new vessel presence API
remotes::install_github("GlobalFishingWatch/gfwr@major_release",
                        dependencies = TRUE, build_vignettes = TRUE)

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
usethis::edit_r_environ()
#and paste teh following line to the .Renviron file:
GFW_TOKEN="eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6ImtpZEtleSJ9.eyJkYXRhIjp7Im5hbWUiOiJJbmdvLk1pbGxlcl9QaERfMjAyNSIsInVzZXJJZCI6MjY1NjksImFwcGxpY2F0aW9uTmFtZSI6IkluZ28uTWlsbGVyX1BoRF8yMDI1IiwiaWQiOjI1OTAsInR5cGUiOiJ1c2VyLWFwcGxpY2F0aW9uIn0sImlhdCI6MTc0NTQ1NDk5OCwiZXhwIjoyMDYwODE0OTk4LCJhdWQiOiJnZnciLCJpc3MiOiJnZncifQ.iBBWF91YudweM-9BCIeRDJEgzTNw0n26FRM6Vh5Do2RgTl9s1qONGRVtVn105b9-bOJASXw95yaWH4H9bRHiVmEIUP_zLFB8dikBK9dWWnsHxgze_E4xGoEExnklQ_TuUF5gMYnmJh0GIXE5XNAFyA8MEXS7_7l6_Mtspy9PhiWzkcW-cBARxkcmZ1vdOwFBLTEBfIEUp_0ouN6JfC129Mk2J7mBrhlSyeys888eQW_I9f_kI9ARTITdV-BQ1jT46Wl37Io0LtX-RAfsJDkN4fiQLhKty7Uqd-yGSjN0sWW3md4VXWhXXWW5Xf47Hy3Nbs4bvjEywJyZx99DEYXfcik9I05KavcvF9Nsg-IZYNIaocFXtimZR3ANt2VWUziM009aNpzkXcXZyWYurDIdoEx_t1f_Dm_BPn775Wlu1jJjncFJEwc1q4sZTppSCf6arulDHKPcg7jINC513ERvKPjVjvXBy8WfTNwcNcvr1Smpt0reJXmvTle6MlUHHtxE"

# kall the ID we created:
key <- gfw_auth()
#or this if above does not work:
#key <- Sys.getenv("GFW_TOKEN")


my_shp <- tibble(
  lon = c(130,130,180,180,130),
  lat = c(-50, 20,20,-50,-50)
) %>% 
  sf::st_as_sf(
    coords = c('lon','lat'),
    crs = "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  ) %>% 
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


start <- "2018-01-01"
end <- "2018-12-31"



my_filter <- paste(
  "vessel_type IN ('carrier','cargo','passenger','fishing')",
  "AND speed IN ('4-6','6-10','10-15','15-25', '>25')",   
  sep = " "
)

my_filter

ais_p_2018 <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
                 temporal_resolution = "MONTHLY",
                 #group_by = "FLAGANDGEARTYPE",
                 filter_by = my_filter,
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
                                     filter_by = my_filter,
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
                                     filter_by = my_filter,
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
                                     filter_by = my_filter,
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
                                     filter_by = my_filter,
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
                                     filter_by = my_filter,
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
                                     filter_by = my_filter,
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
                                     filter_by = my_filter,
                                     start_date = start,
                                     end_date = end,
                                     region = my_shp,
                                     region_source = "USER_SHAPEFILE",
                                     key = key,
                                     print_request = TRUE)


str(ais_p_2025)

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



glimpse(mean_month_gfw)


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





start <- "2018-01-01"
end <- "2025-08-31"
AUS_region <- c(xmin=135, xmax = 160, ymin = -30, ymax = -5)
AUS_region <- c(xmin=135, xmax = 180, ymin = -45, ymax = 10)
ggplot(data = mean_month_gfw) +
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
    limits = c(1, max(mean_month_gfw$mean_vessels, na.rm = TRUE))
  ) +
  labs(
    title = "Mean Monthly Vessel Density (GFW AIS Presence Data)",
    subtitle = glue::glue("{start} to {end}"),
    fill = "Count"
  ) +
  map_theme_dark


p <- ggplot2::ggplot() +
  # grey layer for 1–2 vessels
  ggplot2::geom_tile(
    data = mean_month_gfw |> dplyr::filter(mean_vessels %in% c(1, 2)),
    mapping = ggplot2::aes(x = lon, y = lat),
    fill = "grey10"
  ) +
  # gradient for >2 vessels
  ggplot2::geom_tile(
    data = mean_month_gfw |> dplyr::filter(mean_vessels > 2),
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
    limits = c(3, max(mean_month_gfw$mean_vessels, na.rm = TRUE))
  ) +
  ggplot2::labs(
    title = "Mean Monthly Vessel Density (GFW AIS Presence Data)",
    subtitle = glue::glue("{start} to {end}"),
    fill = "Count"
  ) +
  map_theme_dark

p





## without fishing

my_filter <- paste(
  "vessel_type IN ('carrier','cargo','passenger', 'bunker')",
  "AND speed IN ('4-6','6-10','10-15','15-25', '>25')",   
  sep = " "
)

my_filter

start <- "2018-01-01"
end <- "2018-12-31"
ais_p_2018f <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
                                     temporal_resolution = "MONTHLY",
                                     #group_by = "FLAGANDGEARTYPE",
                                     filter_by = my_filter,
                                     start_date = start,
                                     end_date = end,
                                     region = my_shp,
                                     region_source = "USER_SHAPEFILE",
                                     key = key,
                                     print_request = TRUE)



start <- "2019-01-01"
end <- "2019-12-31"
ais_p_2019f <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
                                     temporal_resolution = "MONTHLY",
                                     #group_by = "FLAGANDGEARTYPE",
                                     filter_by = my_filter,
                                     start_date = start,
                                     end_date = end,
                                     region = my_shp,
                                     region_source = "USER_SHAPEFILE",
                                     key = key,
                                     print_request = TRUE)

start <- "2020-01-01"
end <- "2020-12-31"
ais_p_2020f <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
                                     temporal_resolution = "MONTHLY",
                                     #group_by = "FLAGANDGEARTYPE",
                                     filter_by = my_filter,
                                     start_date = start,
                                     end_date = end,
                                     region = my_shp,
                                     region_source = "USER_SHAPEFILE",
                                     key = key,
                                     print_request = TRUE)

start <- "2021-01-01"
end <- "2021-12-31"
ais_p_2021f <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
                                     temporal_resolution = "MONTHLY",
                                     #group_by = "FLAGANDGEARTYPE",
                                     filter_by = my_filter,
                                     start_date = start,
                                     end_date = end,
                                     region = my_shp,
                                     region_source = "USER_SHAPEFILE",
                                     key = key,
                                     print_request = TRUE)


start <- "2022-01-01"
end <- "2022-12-31"
ais_p_2022f <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
                                     temporal_resolution = "MONTHLY",
                                     #group_by = "FLAGANDGEARTYPE",
                                     filter_by = my_filter,
                                     start_date = start,
                                     end_date = end,
                                     region = my_shp,
                                     region_source = "USER_SHAPEFILE",
                                     key = key,
                                     print_request = TRUE)

start <- "2023-01-01"
end <- "2023-12-31"
ais_p_2023f <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
                                     temporal_resolution = "MONTHLY",
                                     #group_by = "FLAGANDGEARTYPE",
                                     filter_by = my_filter,
                                     start_date = start,
                                     end_date = end,
                                     region = my_shp,
                                     region_source = "USER_SHAPEFILE",
                                     key = key,
                                     print_request = TRUE)

start <- "2024-01-01"
end <- "2024-12-31"
ais_p_2024f <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
                                     temporal_resolution = "MONTHLY",
                                     #group_by = "FLAGANDGEARTYPE",
                                     filter_by = my_filter,
                                     start_date = start,
                                     end_date = end,
                                     region = my_shp,
                                     region_source = "USER_SHAPEFILE",
                                     key = key,
                                     print_request = TRUE)

start <- "2025-01-01"
end <- "2025-08-31"
ais_p_2025f <- gfwr::gfw_ais_presence(spatial_resolution = "LOW",
                                     temporal_resolution = "MONTHLY",
                                     #group_by = "FLAGANDGEARTYPE",
                                     filter_by = my_filter,
                                     start_date = start,
                                     end_date = end,
                                     region = my_shp,
                                     region_source = "USER_SHAPEFILE",
                                     key = key,
                                     print_request = TRUE)



ais_p_all_f <- dplyr::bind_rows(
  ais_p_2018f,
  ais_p_2019f,
  ais_p_2020f,
  ais_p_2021f,
  ais_p_2022f,
  ais_p_2023f,
  ais_p_2024f,
  ais_p_2025f
)

ais_p_all_f


mean_month_gfw_f <- ais_p_all_f |>
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


start <- "2018-01-01"
end <- "2025-08-31"
# AUS_region <- c(xmin=135, xmax = 160, ymin = -30, ymax = -5)
AUS_region <- c(xmin=135, xmax = 180, ymin = -45, ymax = 10)
ggplot(data = mean_month_gfw_f) +
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
    limits = c(1, max(mean_month_gfw_f$mean_vessels, na.rm = TRUE))
  ) +
  labs(
    title = "Mean Monthly Vessel Density (GFW AIS Presence Data)",
    subtitle = glue::glue("{start} to {end}"),
    fill = "Count"
  ) +
  map_theme_dark



p <- ggplot2::ggplot() +
  # grey layer for 1–2 vessels
  ggplot2::geom_tile(
    data = mean_month_gfw_f |> dplyr::filter(mean_vessels %in% c(1, 2)),
    mapping = ggplot2::aes(x = lon, y = lat),
    fill = "grey5"
  ) +
  # gradient for >2 vessels
  ggplot2::geom_tile(
    data = mean_month_gfw_f |> dplyr::filter(mean_vessels > 2),
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
    limits = c(3, max(mean_month_gfw_f$mean_vessels, na.rm = TRUE))
  ) +
  ggplot2::labs(
    title = "Mean Monthly Vessel Density (GFW AIS Presence Data)",
    subtitle = glue::glue("{start} to {end}"),
    fill = "Count"
  ) +
  map_theme_dark

p




# # Get Shipping Traffic Data -----------------------------------------------
# 
# 
# get_event(event_type = 'ENCOUNTER',
#           start_date = "2020-01-01",
#           end_date = "2020-01-02",
#           key = key
# )
# 
# 
# 
# 
# 
# # Shiiping Traffic Maps ---------------------------------------------------
# 




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


start_date <- '2023-01-01'
end_date <- '2023-02-01'


ais_p_2018

month_sel <- "2023-01"   # change to any value in `ais_p$`Time Range``

# ---- 2) summarise to one value per cell (sum of vessels across flags) ----
cell_counts <- ais_p_2018 |>
  #dplyr::filter(`Time Range` == month_sel) |>
  dplyr::group_by(Lon, Lat) |>
  dplyr::summarise(vessels = sum(`Vessel IDs`, na.rm = TRUE), .groups = "drop")

# ---- 3) points -> raster (0.1° global grid, WGS84) ----
pts <- cell_counts |>
  terra::vect(geom = c("Lon", "Lat"), crs = "EPSG:4326")

r_tmpl <- terra::rast(xmin = 130, xmax = 180, ymin = -40, ymax = 10, crs = "EPSG:4326")
terra::res(r_tmpl) <- 0.1

r_ais <- terra::rasterize(
  x = pts, y = r_tmpl, field = "vessels",
  fun = "sum", background = NA
)

# (optional) drop empty margins for nicer plotting
r_ais_trim <- terra::trim(r_ais)

# ---- 4) plot ----
# Base terra
r_ais |>
  terra::plot(main = paste0("Vessels per 0.1° cell"))

# ggplot2 + tidyterra
ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = r_ais) +
  ggplot2::scale_fill_viridis_c(name = "Vessels / cell", trans = "sqrt") +
  ggplot2::coord_sf(expand = FALSE) +
  #ggplot2::labs(title = paste0("Vessels per 0.1° cell — ", month_sel)) +
  ggplot2::theme_bw()




ais_p_2018 %>% 
  filter(`Vessel IDs` >= 1) %>% 
  ggplot() +
  geom_raster(aes(x = Lon,
                  y = Lat,
                  fill = `Vessel IDs`)) +
  geom_sf(data = ne_countries(returnclass = "sf", scale = "medium")) +
  coord_sf() +
  coord_sf(xlim = c(min(ais_p_2018$Lon),max(ais_p_2018$Lon)),
           ylim = c(min(ais_p_2018$Lat),max(ais_p_2018$Lat))) +
  scale_fill_gradientn(
    trans = 'log10',
    colors = map_effort_light, 
    na.value = NA,
    labels = scales::comma) +
  labs(title = "Apparent fishing hours in the Italian EEZ",
       subtitle = glue::glue("{start_date} to {end_date}"),
       fill = "Vessel Density") +
  map_theme







# regions <- get_regions()
# 
# 
# # Use get_region_id function to get EEZ code for Italy
# AUS_EEZ_code <- get_region_id(region_name = "AUS", region_source = "EEZ", key = key)
# 
# AUS_EEZ_code <- AUS_EEZ_code %>%  dplyr::filter(label == "Australia")
# 
# 
# # Download data for the AUS EEZ
# AUS_eez_fish_df <- get_raster(
#   spatial_resolution = "HIGH",
#   temporal_resolution = "MONTHLY",
#   group_by = "GEARTYPE",
#   start_date = start_date,
#   end_date = end_date,
#   region = AUS_EEZ_code$id,
#   region_source = "EEZ"
# )
# 
# AUS_eez_fish_df
# 

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



# Shipping Traffic Maps ---------------------------------------------------


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

panel.background = element_rect(fill="lightblue", color = NA) #makes water blue

# Palette for fishing activity
map_effort_light <- c("#ffffff", "#eeff00", "#3b9088","#0c276c")
map_color <- c("#ffffff","#3b9088","#0c276c" )

# Date
start_date <- '2021-01-01'
end_date <- '2021-01-31'


regions <- get_regions()
regions

# Use get_region_id function to get EEZ code for Australia
AUS_EEZ_code <- get_region_id(region_name = "AUS", region_source = "EEZ", key = key)

AUS_EEZ_code <- AUS_EEZ_code %>%  dplyr::filter(label == "Australia")

AUS_EEZ_code
# Download data for the AUS EEZ
AUS_eez_fish_df <- get_raster(
  spatial_resolution = "HIGH",
  temporal_resolution = "HOURLY",
  group_by = "VESSEL_ID",
  #group_by = "GEARTYPE",
  #filter_by = "vessel_id",
  start_date = start_date,
  end_date = end_date,
  region = AUS_EEZ_code$id,
  region_source = "EEZ"
)

AUS_eez_fish_df
glimpse(AUS_eez_fish_df)
unique(AUS_eez_fish_df$`Vessel Type`)
unique(AUS_eez_fish_df$`Gear Type`)

gear_per_vessel <- AUS_eez_fish_df |> 
  dplyr::group_by(`Vessel Type`, `Gear Type`) |> 
  dplyr::summarise(count = n(), .groups = 'drop') |> 
  dplyr::arrange(desc(count))

gear_per_vessel


# organise by grid cell

eez_fish_all_df <- AUS_eez_fish_df %>% 
  dplyr::group_by(Lat, Lon) %>% 
  dplyr::summarise(fishing_hours = sum(`Apparent Fishing Hours`, na.rm = T),
                   Vessel_count = sum(`Vessel IDs`, na.rm = T))


eez_fish_all_df

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


eez_fish_all_df %>% 
  filter(Vessel_count >= 1) %>% 
  ggplot() +
  geom_raster(aes(x = Lon,
                  y = Lat,
                  fill = Vessel_count)) +
  geom_sf(data = ne_countries(returnclass = "sf", scale = 10)) +
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
  geom_sf(data = ne_countries(returnclass = "sf", scale = 10)) +
  geom_sf(
    data = my_shp, 
    fill = NA,
    color = 'red') +
  map_theme


my_shp_bbox <- st_bbox(my_shp)


my_raster_df <- get_raster(
  spatial_resolution = "LOW",
  temporal_resolution = "HOURLY",
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

str(my_events_sf)


my_raster_all_df %>% 
  filter(fishing_hours > 1) %>% 
  ggplot() +
  geom_raster(aes(x = Lon,
                  y = Lat,
                  fill = fishing_hours)) +
  geom_sf(data = my_events_sf,
          aes(color = eventType),
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

## AMSA DATA ------------------------------------------------

# downloaded Jan 2021 data from AMSA

AMSA_Shipping_shp <- st_read("/Users/ingo/Library/CloudStorage/OneDrive-JamesCookUniversity/02_PhD/06_Chapters/DataChapters/Chapter4_Whalesharks_Risks/Shipping/AIS_AMSA_data/Vessel Traffic Data September 2023/cts_srr_09_2023_pt/cts_srr_09_2023_pt.shp")

AMSA_df <- as.data.frame(AMSA_Shipping_shp)


glimpse(AMSA_df)


# Grid
AMSA_df$lon_bin <- cut(AMSA_df$LON, breaks = seq(min(AMSA_df$LON), max(AMSA_df$LON), by = 0.1))
AMSA_df$lat_bin <- cut(AMSA_df$LAT, breaks = seq(min(AMSA_df$LAT), max(AMSA_df$LAT), by = 0.1))

# Count
count <-AMSA_df %>%
group_by(lon_bin, lat_bin) %>%
summarise(count = n())


# convert to numeric and back to lat/lon

count$lon_bin <- as.numeric(count$lon_bin) * 0.1 + min(AMSA_df$LON)
count$lat_bin <- as.numeric(count$lat_bin) * 0.1 + min(AMSA_df$LAT)

# Maps
# Full World

ggplot(data = count) +
geom_raster(aes(x = lon_bin,
                y = lat_bin,
                fill = count)) +
geom_sf(data = ne_countries(returnclass = "sf", scale = 10)) +
coord_sf(xlim = range(count$lon_bin),ylim = range(count$lat_bin)) +
scale_fill_gradientn(
  trans = 'log10',
  colors = map_effort_light, 
  na.value = NA,
  labels = scales::comma) +
labs(title = "Fishing Traffic Density in the Australian EEZ",
     subtitle = glue::glue("{start_date} to {end_date}"),
     fill = "Count") +
map_theme

# Specific Region
AUS_region <- c(xmin=135, xmax = 160, ymin = -30, ymax = -5)
ggplot(data = count) +
  geom_raster(aes(x = lon_bin,
                  y = lat_bin,
                  fill = count)) +
  geom_sf(data = ne_countries(returnclass = "sf", scale = "medium")) +
  coord_sf(xlim = c(AUS_region["xmin"], AUS_region["xmax"]),
           ylim = c(AUS_region["ymin"], AUS_region["ymax"])) +
  scale_fill_gradientn(
    trans = 'log10',
    colors = map_effort_light, 
    na.value = NA,
    labels = scales::comma,
    limits = c(1, 474)) +
  labs(title = "Shipping Traffic Density in the Australian EEZ",
       subtitle = glue::glue("{start_date} to {end_date}"),
       fill = "Count") +
  map_theme



WB_region <- c(xmin=142, xmax = 145, ymin = -12.5, ymax = -10.5)
ggplot(data = count) +
  geom_raster(aes(x = lon_bin,
                  y = lat_bin,
                  fill = count)) +
  geom_sf(data = ne_countries(returnclass = "sf", scale = "medium")) +
  coord_sf(xlim = c(WB_region["xmin"], WB_region["xmax"]),
           ylim = c(WB_region["ymin"], WB_region["ymax"])) +
  scale_fill_gradientn(
    trans = 'log10',
    colors = map_effort_light, 
    na.value = NA,
    labels = scales::comma,
    limits = c(1, 474)) +
  labs(title = "Shipping Traffic Density in the Australian EEZ",
       subtitle = glue::glue("{start_date} to {end_date}"),
       fill = "Count") +
  map_theme




## GFW Data Mapping -----------------------------------------

# Grid
AUS_eez_fish_df$lon_bin <- cut(AUS_eez_fish_df$Lon, breaks = seq(min(AUS_eez_fish_df$Lon), max(AUS_eez_fish_df$Lon), by = 0.1))
AUS_eez_fish_df$lat_bin <- cut(AUS_eez_fish_df$Lat, breaks = seq(min(AUS_eez_fish_df$Lat), max(AUS_eez_fish_df$Lat), by = 0.1))

# Count
GFW_count <-AUS_eez_fish_df %>%
  group_by(lon_bin, lat_bin) %>%
  summarise(count = n())


# convert to numeric and back to lat/lon
GFW_count <- GFW_count %>%
  mutate(
    lon = as.numeric(lon_bin) * 0.1 + min(AUS_eez_fish_df$Lon),
    lat = as.numeric(lat_bin) * 0.1 + min(AUS_eez_fish_df$Lat)
  )
  

# Maps
# Specific Region
AUS_region <- c(xmin=135, xmax = 160, ymin = -30, ymax = -5)
ggplot(data = GFW_count) +
  geom_raster(aes(x = lon,
                  y = lat,
                  fill = count)) +
  geom_sf(data = ne_countries(returnclass = "sf", scale = "medium")) +
  coord_sf(xlim = c(AUS_region["xmin"], AUS_region["xmax"]),
           ylim = c(AUS_region["ymin"], AUS_region["ymax"])) +
  scale_fill_gradientn( 
    trans = 'log10', 
    colors = map_effort_light, 
    na.value = NA, limits = c(1, 169),
    breaks = c(1, 169),
    labels = c("Low", "High"))+ 
    labs(title = "GFW Shipping Traffic Density in the Australian EEZ", 
         subtitle = glue::glue("{start_date} to {end_date}"), 
         fill = "Shipping Density (Count)") + 
  map_theme
    
    
