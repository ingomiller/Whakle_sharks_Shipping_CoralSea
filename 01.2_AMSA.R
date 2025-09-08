


library(sf)
library(dplyr)
library(purrr)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyr)
library(stringr)
library(cmocean)
library(terra)


# Define base folder
base_dir <- "/Users/ingo/Library/CloudStorage/OneDrive-JamesCookUniversity/02_PhD/06_Chapters/DataChapters/Chapter4_Whalesharks_Risks/AIS_AMSA_data"






AMSA_Shipping_shp <- sf::st_read("/Volumes/Ingo_PhD/PhD_Data_Analysis/Shipping/AIS_AMSA_data/cts_srr_09_2022_pt.shp")

AMSA_df<- as.data.frame(AMSA_Shipping_shp)


glimpse(AMSA_df)
unique(AMSA_df$TYPE)
unique(AMSA_df$SUBTYPE)
unique(AMSA_df$LENGTH)

head(AMSA_df)

# exlucee. certain vessel typoes:
vessel_exclude_patterns <- c(
  "Pleasure",
  "Sailing",
  "Pilot", 
  "Port tender", 
  "Diving", 
  "Dredging", 
  "Local"
)

AMSA_df_filtered <- AMSA_df |> 
  dplyr::filter(!grepl(paste(vessel_exclude_patterns, collapse = "|"), TYPE)) |> 
  dplyr::filter(LENGTH >= 30)

str(AMSA_df_filtered)





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



start_date <- '2023-09-01'
end_date <- '2023-09-30'




AMSA_df <- AMSA_df_filtered

# Grid by 0AMSA_df_filtered# Grid by 0.1 degree
AMSA_df$lon_bin <- cut(AMSA_df$LON, breaks = seq(floor(min(AMSA_df$LON)), ceiling(max(AMSA_df$LON)), by = 0.1))
AMSA_df$lat_bin <- cut(AMSA_df$LAT, breaks = seq(floor(min(AMSA_df$LAT)), ceiling(max(AMSA_df$LAT)), by = 0.1))

# Count unique vessel IDs per grid cell
count <- AMSA_df |> 
  dplyr::group_by(lon_bin, lat_bin) |> 
  dplyr::summarise(unique_vessels_count = n_distinct(CRAFT_ID), .groups = "drop")

count

# Convert factor bins to midpoint numeric coordinates
extract_bin_mid <- function(bin_factor) {
  sapply(as.character(bin_factor), function(b) {
    edges <- as.numeric(gsub("\\[|\\]|\\(|\\)", "", strsplit(b, ",")[[1]]))
    mean(edges)
  })
}

count$lon_bin <- extract_bin_mid(count$lon_bin)
count$lat_bin <- extract_bin_mid(count$lat_bin)


count








# # Grid
# AMSA_df$lon_bin <- cut(AMSA_df$LON, breaks = seq(min(AMSA_df$LON), max(AMSA_df$LON), by = 0.1))
# AMSA_df$lat_bin <- cut(AMSA_df$LAT, breaks = seq(min(AMSA_df$LAT), max(AMSA_df$LAT), by = 0.1))
# 
# # Count
# count <-AMSA_df %>%
#   group_by(lon_bin, lat_bin) %>%
#   summarise(count = n())
# 
# 
# # convert to numeric and back to lat/lon
# 
# count$lon_bin <- as.numeric(count$lon_bin) * 0.1 + min(AMSA_df$LON)
# count$lat_bin <- as.numeric(count$lat_bin) * 0.1 + min(AMSA_df$LAT)
# 
# count


# Maps
# Full World
max(count$unique_vessels_count)


ggplot(data = count) +
  geom_raster(aes(x = lon_bin,
                  y = lat_bin,
                  fill = unique_vessels_count)) +
  geom_sf(data = ne_countries(returnclass = "sf", scale = 10), color = "grey20", fill = "grey20") +
  coord_sf(xlim = range(count$lon_bin),ylim = range(count$lat_bin)) +
  scale_fill_gradientn(
    trans = 'log10',
    colors = map_effort_dark, 
    na.value = NA,
    labels = scales::comma) +
  labs(title = "Fishing Traffic Density in the Australian EEZ",
       subtitle = glue::glue("{start_date} to {end_date}"),
       fill = "Count") +
  map_theme_dark

# QLD region
AUS_region <- c(xmin=135, xmax = 160, ymin = -30, ymax = -5)
ggplot(data = count) +
  geom_raster(aes(x = lon_bin,
                  y = lat_bin,
                  fill = unique_vessels_count)) +
  geom_sf(data = ne_countries(returnclass = "sf", scale = 10), color = "grey20", fill = "grey20") +
  coord_sf(xlim = c(AUS_region["xmin"], AUS_region["xmax"]),
           ylim = c(AUS_region["ymin"], AUS_region["ymax"])) +
  scale_fill_gradientn(
    trans = 'log10',
    colors = map_effort_dark, 
    na.value = NA,
    labels = scales::comma,
    limits = c(1, max(count$unique_vessels_count))) +
  labs(title = "Shipping Traffic Density in the Australian EEZ",
       subtitle = glue::glue("{start_date} to {end_date}"),
       fill = "Count") +
  map_theme_dark



FN_region <- c(xmin=142, xmax = 148, ymin = -20, ymax = -10.5)
ggplot(data = count) +
  geom_raster(aes(x = lon_bin,
                  y = lat_bin,
                  fill = unique_vessels_count)) +
  geom_sf(data = ne_countries(returnclass = "sf", scale = 10), color = "grey20", fill = "grey20") +
  coord_sf(xlim = c(FN_region["xmin"], FN_region["xmax"]),
           ylim = c(FN_region["ymin"], FN_region["ymax"])) +
  scale_fill_gradientn(
    trans = 'log10',
    colors = map_effort_dark, 
    na.value = NA,
    labels = scales::comma,
    limits = c(1, max(count$unique_vessels_count))) +
  labs(title = "Shipping Traffic Density in the Australian EEZ",
       subtitle = glue::glue("{start_date} to {end_date}"),
       fill = "Count") +
  map_theme_dark




# MEAN MONTHLY VESSEL TRAFFIC DATA  ---------------------------------------

base_dir <- "/Volumes/Ingo_PhD/PhD_Data_Analysis/Shipping/AIS_AMSA_data"

shp_files <- list.files(base_dir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)

vessel_exclude_patterns <- c("Pleasure", 
                             "Sailing", 
                             "Pilot", 
                             "Port tender", 
                             "Diving", 
                             "Dredging", 
                             "Local")

grid_resolution <- 0.1


start <- 'Jan 2018'
end <- 'Jul 2025'

# Function to process a single shapefile
process_shapefile <- function(shp_path) {
  df <- sf::st_read(shp_path, quiet = TRUE) |> 
    as.data.frame() #|>
    # dplyr::filter(!grepl(paste(vessel_exclude_patterns, collapse = "|"), TYPE, ignore.case = TRUE)) |>
    # dplyr::filter(LENGTH >= 30)
  
  # Bin coordinates
  df$lon_bin <- cut(df$LON, breaks = seq(floor(min(df$LON)), ceiling(max(df$LON)), by = grid_resolution))
  df$lat_bin <- cut(df$LAT, breaks = seq(floor(min(df$LAT)), ceiling(max(df$LAT)), by = grid_resolution))
  
  # Count unique vessels per grid
  count <- df |> 
    dplyr::group_by(lon_bin, lat_bin) |>
    dplyr::summarise(unique_vessels = dplyr::n_distinct(CRAFT_ID), .groups = "drop")
  
  # Extract numeric bin centers
  extract_bin_mid <- function(bin_factor) {
    sapply(as.character(bin_factor), function(b) {
      edges <- as.numeric(gsub("\\[|\\]|\\(|\\)", "", strsplit(b, ",")[[1]]))
      mean(edges)
    })
  }
  
  count$lon <- extract_bin_mid(count$lon_bin)
  count$lat <- extract_bin_mid(count$lat_bin)
  
  # Add ID to merge later
  count <- count |> 
    dplyr::select(lon, lat, unique_vessels) |>
    dplyr::mutate(id = paste(lon, lat, sep = "_"))
  
  return(count)
}





# Process all shapefiles
density_list <- purrr::map(shp_files, process_shapefile)


# Keep only id + value per month
density_list_named <- purrr::imap(density_list, ~ .x |> dplyr::select(id, unique_vessels) |> 
                                    dplyr::rename(!!paste0("vessels_", .y) := unique_vessels))

# Reduce cleanly by id only
all_counts <- purrr::reduce(density_list_named, dplyr::full_join, by = "id")
all_counts


# Add back lon/lat only once
lon_lat_df <- density_list[[1]] |> dplyr::select(id, lon, lat)
all_counts <- dplyr::left_join(lon_lat_df, all_counts, by = "id")
str(all_counts)

# Replace NAs with 0 for mean calculation
monthly_matrix <- all_counts |> 
  dplyr::select(starts_with("vessels_")) |> 
  dplyr::mutate(dplyr::across(everything(), ~ tidyr::replace_na(.x, 0)))
monthly_matrix



# Compute mean across all months
all_counts$mean_unique_vessels <- rowMeans(monthly_matrix)
glimpse(all_counts)
str(all_counts)

# Final output
mean_density <- all_counts |> 
  dplyr::select(lon, lat, mean_unique_vessels)

print(mean_density)

max(mean_density$mean_unique_vessels)
min(mean_density$mean_unique_vessels)

# QLD region
AUS_region <- c(xmin=135, xmax = 160, ymin = -30, ymax = -5)
ggplot(data = mean_density) +
  geom_raster(aes(x = lon,
                  y = lat,
                  fill = mean_unique_vessels)) +
  geom_sf(data = ne_countries(returnclass = "sf", scale = 10), color = "grey20", fill = "grey20") +
  coord_sf(xlim = c(AUS_region["xmin"], AUS_region["xmax"]),
           ylim = c(AUS_region["ymin"], AUS_region["ymax"])) +
  scale_fill_gradientn(
    trans = 'log10',
    #colors = map_effort_dark,
    colors = cmocean::cmocean("thermal")(300),
    na.value = NA,
    labels = scales::comma,
    limits = c(1, max(mean_density$mean_unique_vessels))) +
    #limits = c(min(mean_density$mean_unique_vessels), max(mean_density$mean_unique_vessels))) +
  
  labs(title = "Mean Shipping Traffic Density in the Australian SAR Zone",
       subtitle = glue::glue("{start} to {end}"),
       fill = "Count") +
  map_theme_dark


FN_region <- c(xmin=142, xmax = 148, ymin = -20, ymax = -10.5)
ggplot(data = mean_density) +
  geom_raster(aes(x = lon,
                  y = lat,
                  fill = mean_unique_vessels)) +
  geom_sf(data = ne_countries(returnclass = "sf", scale = 10), color = "grey20", fill = "grey20") +
  coord_sf(xlim = c(FN_region["xmin"], FN_region["xmax"]),
           ylim = c(FN_region["ymin"], FN_region["ymax"])) +
  scale_fill_gradientn(
    trans = 'log10',
    #colors = map_effort_dark,
    colors = cmocean::cmocean("thermal")(300),
    na.value = NA,
    labels = scales::comma,
    limits = c(1, max(mean_density$mean_unique_vessels))) +
  #limits = c(min(mean_density$mean_unique_vessels), max(mean_density$mean_unique_vessels))) +
  
  labs(title = "Mean Shipping Traffic Density in the Australian SAR Zone",
       subtitle = glue::glue("{start} to {end}"),
       fill = "Count") +
  map_theme_dark

# Create raster from point data
r <- terra::rast(mean_density, type = "xyz")

# Set CRS (WGS84)
terra::crs(r) <- "EPSG:4326"


terra::plot(r)


# Write to GeoTIFF
terra::writeRaster(r, "Shipping_Density_AUS_2018-2025.tif", overwrite = TRUE)







############## all files at once with hours by grid anbd vessel count density 


process_shapefile <- function(shp_path, vessel_exclude_patterns, out_dir, grid_resolution = 0.1) {
  tryCatch({
    file_name <- tools::file_path_sans_ext(basename(shp_path))
    out_file <- file.path(out_dir, paste0(file_name, "_processed.csv"))
    if (file.exists(out_file)) {
      message("Skipping (already processed): ", out_file)
      return(out_file)
    }
    
    # Read shapefile
    sf_obj <- sf::st_read(shp_path, quiet = TRUE)
    
    # Fix column typo before converting
    if ("TMESTAMP" %in% names(sf_obj) && !"TIMESTAMP" %in% names(sf_obj)) {
      names(sf_obj)[names(sf_obj) == "TMESTAMP"] <- "TIMESTAMP"
    }
    
    # Read, convert, fix malformed timestamp column
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
    
    
    # Parse timestamp and sort
    parsed_times <- suppressWarnings(lubridate::dmy_hms(df$TIMESTAMP, tz = "UTC"))
    if (all(is.na(parsed_times))) {
      stop("All TIMESTAMP entries failed to parse in ", basename(shp_path))
    }
    df$TIMESTAMP <- parsed_times
    bad_times <- df$TIMESTAMP[is.na(parsed_times)]
    print(head(bad_times, 5))  # Optional logging
    
    df <- dplyr::arrange(df, CRAFT_ID, TIMESTAMP)
    
    # Calculate time and distance between fixes
    df <- df |>
      dplyr::mutate(
        time_diff = as.numeric(difftime(lead(TIMESTAMP), TIMESTAMP, units = "hours")),
        dist_km = geosphere::distHaversine(cbind(LON, LAT), cbind(lead(LON), lead(LAT))) / 1000,
        time_diff = ifelse(time_diff < 0 | time_diff > 6 | dist_km > 30, NA, time_diff)
      )
    
    # Grid binning
    df$lon_bin <- cut(df$LON, breaks = seq(floor(min(df$LON)), ceiling(max(df$LON)), by = grid_resolution))
    df$lat_bin <- cut(df$LAT, breaks = seq(floor(min(df$LAT)), ceiling(max(df$LAT)), by = grid_resolution))
    
    extract_mid <- function(bin) {
      sapply(as.character(bin), function(b) {
        rng <- as.numeric(strsplit(gsub("\\[|\\]|\\(|\\)", "", b), ",")[[1]])
        mean(rng)
      })
    }
    
    df <- df |>
      dplyr::mutate(
        lon = extract_mid(lon_bin),
        lat = extract_mid(lat_bin)
      ) |>
      dplyr::filter(!is.na(lon), !is.na(lat))
    
    summary_df <- df |> 
      dplyr::group_by(lon, lat) |> 
      dplyr::summarise(
        unique_vessels = dplyr::n_distinct(CRAFT_ID),
        total_hours = sum(time_diff, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Save CSV
    readr::write_csv(summary_df, out_file)
    message("Processed and saved: ", out_file)
    return(out_file)
    
  }, error = function(e) {
    message("Failed: ", basename(shp_path), " - ", e$message)
    return(NULL)
  })
}






output_folder <- "processed_grids"  # Make sure it exists
dir.create(output_folder, showWarnings = FALSE)

results_paths <- purrr::map(
  shp_files,
  ~ process_shapefile(.x, vessel_exclude_patterns, output_folder)
) |> purrr::compact()


# merge all files:
merge_csvs <- function(folder_path = "processed_grids") {
  # Step 1: List all processed CSVs
  csv_files <- list.files(folder_path, pattern = "_processed\\.csv$", full.names = TRUE)
  
  # Step 2–4: Read, parse metadata, and merge
  all_data <- purrr::map_dfr(csv_files, function(file) {
    df <- readr::read_csv(file, show_col_types = FALSE)
    
    # Extract month and year from filename
    filename <- basename(file)
    # Extract year (first 4-digit number starting with 20)
    year <- str_extract(filename, "20\\d{2}")
    
    # Extract month (first standalone 2-digit number between 01 and 12)
    month <- str_extract(filename, "(?<!\\d)(0[1-9]|1[0-2])(?!\\d)")
    
    df <- df %>%
      dplyr::mutate(year = as.integer(year),
                    month = as.integer(month),
                    year_month = paste0(year, "-", month),
                    source_file = filename)
  })
  
  return(all_data)
}

# Merge all
all_merged <- merge_csvs("processed_grids")

all_merged |> 
  dplyr::arrange(year, month) |> 
  tail()

glimpse(all_merged)
tail(all_merged)

# Step 5: Calculate monthly mean per cell (optional)
mean_summary <- all_merged |> 
  dplyr::group_by(lon, lat) |> 
  dplyr::summarise(
    min_vessels = min(unique_vessels, na.rm = TRUE),
    mean_vessels = mean(unique_vessels, na.rm = TRUE),
    max_vessels = max(unique_vessels, na.rm = TRUE),
    min_hours = min(total_hours, na.rm = TRUE),
    mean_hours = mean(total_hours, na.rm = TRUE),
    max_hours = max(total_hours, na.rm = TRUE),
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
ggplot(data = monthly_mean_summary) +
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
  rstatix::get_summary_stats(unique_vessels, type = "common")

