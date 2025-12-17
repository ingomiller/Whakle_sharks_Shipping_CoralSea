library(terra)
library(tidyverse)
library(tidyterra)
library(ggplot2)
library(ggspatial)
library(sf)
library(basemaps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(grid)
library(patchwork)



# Import Data  ------------------------------------------------------------



# ships_amsa <- terra::rast( "Shipping_MeanVessels_AUS_2018-2025.tif")
ships_gfw <- terra::rast("data/processed/GFW_SWP_AIS_pres_2018_2025_bigVessels.tif")
ships_gfw_fast <- terra::rast("data/processed/GFW_SWP_AIS_pres_2018_2025_bigVessels_Speed10+knots.tif")
ships_gfw_slow <- terra::rast("data/processed/GFW_SWP_AIS_pres_2018_2025_bigVessels_Speed-10knots.tif")



# ships_amsa
ships_gfw
ships_gfw_fast
ships_gfw_slow



mean_SDM <- terra::rast("/Volumes/Ingo_PhD/PhD_Data_Analysis/PhD_WhaleSharks_SDMs_Enviro_Layers/Chapter2/SDM_Outputs_Rev/SDM_whalesharks_Tracks_mp_crwPA_Ensemble_mean_climate.tif")


seasons_SDM <- terra::rast("/Volumes/Ingo_PhD/PhD_Data_Analysis/PhD_WhaleSharks_SDMs_Enviro_Layers/Chapter2/SDM_Outputs_Rev/SDM_whalesharks_Tracks_mp_crwPA_Ensemble_seasonal2_mean_climate.tif")

quartals_SDM <- terra::rast("/Volumes/Ingo_PhD/PhD_Data_Analysis/PhD_WhaleSharks_SDMs_Enviro_Layers/Chapter2/SDM_Outputs_Rev/SDM_whalesharks_Tracks_mp_crwPA_Ensemble_seasonal4_quartals_mean_climate.tif")


mean_SDM
seasons_SDM
quartals_SDM

# Extract monsoon layer
sdm_monsoon <- seasons_SDM$Monsoon_Season_Ensemble_Tracking
sdm_monsoon



sdm_peak <- quartals_SDM$`Oct - Dec`

sdm_peak
plot(sdm_peak, range = c(0, 1))
plot(sdm_monsoon, range = c(0, 1))
plot(mean_SDM, range = c(0, 1))


# terra::plot(ships_amsa)
terra::plot(ships_gfw)
print(ships_gfw)
# Optional: align extents/resolution
#check alignment
terra::origin(ships_gfw)
terra::origin(mean_SDM)
terra::res(ships_gfw); terra::res(mean_SDM)


ships_crop <- terra::crop(ships_gfw, mean_SDM, snap = "out")
ships_crop
terra::plot(ships_crop)

# resample the sdm layer to ship raster to keep ships crisp 
sdm_monsoon_res <- terra::resample(sdm_monsoon, ships_crop, method = "bilinear")
sdm_monsoon_res
plot(sdm_monsoon_res)

mean_SDM_res <- terra::resample(mean_SDM, ships_crop, method = "bilinear")
mean_SDM_res
plot(mean_SDM_res)

sdm_peak_res <- terra::resample(sdm_peak, ships_crop, method = "bilinear")
sdm_peak_res
plot(sdm_peak_res)

ships_gfw
sdm_monsoon

# Maps --------------------------------------------------------------------


# Set CZU (collision zone use value)
czu_value <- 0.55 # as per Womersley et al. 2022 | I think this is not needed when using habitat suitability


# rescale ship density to 0-1
qs <- terra::global(
  ships_crop,
  fun = function(x) stats::quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)) |> 
  base::as.data.frame()

vmin_q <- base::as.numeric(qs[1, 1])
vmax_q <- base::as.numeric(qs[1, 2])

ships_q01_qnorm <- terra::app(
  ships_crop,
  fun = function(x) (x - vmin_q) / (vmax_q - vmin_q)
) |>
  terra::clamp(lower = 0, upper = 1, values = TRUE)

terra::global(ships_q01_qnorm, "range", na.rm = TRUE)

plot(ships_q01_qnorm, range = c(0, 1))


# Compute SCI
sci_raster <- mean_SDM_res * ships_q01_qnorm #* czu_value
sci_raster

terra::plot(sci_raster)



# Compute SCI fro monsoon seasosn 
sci_monsoon_raster <- sdm_monsoon_res * ships_q01_qnorm #* czu_value
sci_monsoon_raster

terra::plot(sci_monsoon_raster, range = c(0, 1))



# Compute SCI fro peak season Q4 (Oct - Dec)
sci_q4_raster <- sdm_peak_res * ships_q01_qnorm #* czu_value
sci_q4_raster

terra::plot(sci_q4_raster, range = c(0, 1))

# Save
writeRaster(sci_raster, "SCI_mean_current_climate.tif", overwrite = TRUE)
writeRaster(sci_monsoon_raster, "SCI_monsson_current_climate.tif", overwrite = TRUE)
writeRaster(sci_q4_raster, "SCI_Q4_current_climate.tif", overwrite = TRUE)


# Compute mean SCI value across all non-NA cells
mean_sci <- terra::global(sci_raster, fun = "mean", na.rm = TRUE)
mean_sci

mean_monsson_sci <- terra::global(sci_monsoon_raster, fun = "mean", na.rm = TRUE)
mean_monsson_sci

mean_q4_sci <- terra::global(sci_q4_raster, fun = "mean", na.rm = TRUE)
mean_q4_sci



### SCI in high suitability areas

# Mask to show only cells > 0.5
high_suitability <- classify(sdm_monsoon_res, rcl = matrix(c(-Inf, 0.5, NA), ncol = 3, byrow = TRUE))

# Plot high suitability areas only
plot(high_suitability, main = "High Suitability Areas (Suitability > 0.5)", col = "darkgreen", legend = FALSE)

plot(high_suitability)
# Compute SCI only in high-suitability areas
sci_high <- high_suitability * ships_q01_qnorm
plot(sci_high)


# Save
writeRaster(sci_high, "SCI_monsson_current_climate_high-suitability.tif", overwrite = TRUE)

# Mean SCI (only in >0.5 regions)
mean_sci_high <- global(sci_high, "mean", na.rm = TRUE)
print(mean_sci_high)










# Mappoing ----------------------------------------------------------------

WB_areas <- sf::st_read("/Users/ingo/Library/CloudStorage/OneDrive-JamesCookUniversity/02_PhD/06_Chapters/DataChapters/Chapter1_WhaleSharks_Aggreagtion_WB_Characteristics/DATA_Analysis/04_WS_Tracks/PKDE_WreckBay/PKDE_WreckBay.shp") 

str(WB_areas)


WB_25 <- WB_areas |> 
  dplyr::filter(stringr::str_detect(name, "25% est")) |> 
  dplyr::mutate(Cnstll_ = "Australia_WreckBay_PKDE25")

WB_95 <- WB_areas |> 
  dplyr::filter(stringr::str_detect(name, "95% est")) |> 
  dplyr::mutate(Cnstll_ = "Australia_WreckBay_PKDE95")

WB_95

WB_95_high <- WB_areas |> 
  dplyr::filter(stringr::str_detect(name, "95% high")) |> 
  dplyr::mutate(Cnstll_ = "Australia_WreckBay_PKDE95_high")


GBR_zone <- sf::st_read("/Users/ingo/Library/CloudStorage/OneDrive-JamesCookUniversity/02_PhD/06_Chapters/DataChapters/Chapter2_WhaleSharks_Mantas/Data_Analysis/R_workfolder/GBR_Zones/Great_Barrier_Reef_Marine_Park_Boundary.shp") |> 
  sf::st_transform(4326)


GBR_features <- sf::st_read("/Users/ingo/Library/CloudStorage/OneDrive-JamesCookUniversity/02_PhD/06_Chapters/DataChapters/Chapter2_WhaleSharks_Mantas/Data_Analysis/R_workfolder/GBR_Zones/Great_Barrier_Reef_Features.shp")

GBR_features


SRR_zone <- sf::st_read("/Volumes/Ingo_PhD/PhD_Data_Analysis/Shipping/AMSA_AUSTRALIAN_SRR/amsa_aust_srr_pl.shp") |> sf::st_transform(4326)




library(terra)
library(rasterVis)

AUS_region <- c(xmin=135, xmax = 160, ymin = -30, ymax = -5)
max_values <- terra::app(sci_monsoon_raster, fun = "max", na.rm = TRUE)
high <- global(max_values, fun = "max", na.rm = TRUE) |> as.numeric()
min_values <- terra::app(sci_monsoon_raster, fun = "min", na.rm = TRUE)
low <- global(max_values, fun = "min", na.rm = TRUE) |> as.numeric()
high = 1



cities <- data.frame(Loc = c("Cooktown", "Cairns", "Abbot Point", "Hay Point", "Gladstone", "Brisbane", "Sydney"),
                     Group = c("Town",   "Town",     "Town",       "Town",     "Town",       "Town",     "Town"),
                     # Season = c("Monsoon Season (Nov - Apr)"),
                     # lyr = 1,
                     lat = c(-15.4758, -16.918246, -19.8993, -21.2928, -23.8416, -27.4705, -33.911609),
                     lon = c(145.2471, 145.771359, 148.0813,  149.2559, 151.2498, 153.026, 151.186715)) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) 


cities2 <- data.frame(Loc = c( "Port\nMoresby"),
                      lat = c(  -9.4790),
                      lon = c( 147.1494)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) 

WB <- data.frame(Loc = c( "Wreck\nBay"),
                 lat = c(  -12.132504),
                 lon = c( 143.893818)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) 




# Create the inset map of Australia
# Get the world countries data from Natural Earth
world <- ne_countries(scale = 10, returnclass = "sf")

AUS_PNG <- world |> filter(name == "Australia" | name == "Papua New Guinea" | name == "Indonesia")
plot(AUS_PNG$geometry)

# Define the extent of the study area 
ext <- c(xmin = 140, xmax = 170, ymin = -40, ymax = 0)

# Create the inset map of Australia + Papua New Guinea and add the study area rectangle
AUS_map <- ggplot() +
  geom_sf(data = AUS_PNG, fill = "lightgray", colour = NA, size = 0.1) +
  coord_sf(xlim = c(112, 172), ylim = c(-45, 2), expand = FALSE) +
  geom_rect(aes(xmin = ext["xmin"], xmax = ext["xmax"], ymin = ext["ymin"], ymax = ext["ymax"]), 
            fill = NA, color = "black", 
            linetype = "solid", size = 0.5) +  # Study area rectangle
  theme_void() +  
  theme(panel.border = element_blank(),
        panel.background = element_blank())  

AUS_map

world_map <- ggplot() +
  geom_sf(data = world$geometry, fill = "lightgray", colour = NA, size = 0.1) +
  # coord_sf(xlim = c(-180, 180), ylim = c(-50, 50), expand = FALSE) +
  geom_rect(aes(xmin = ext["xmin"], xmax = ext["xmax"], ymin = ext["ymin"], ymax = ext["ymax"]), 
            fill = NA, color = "black", 
            linetype = "solid", size = 0.5) +  # Study area rectangle
  theme_void() +  
  theme(panel.border = element_blank(),
        panel.background = element_blank())  

plot(world_map)


ext <- setNames(ext, c("xmin", "xmax", "ymin", "ymax"))

# Build polygon coordinates (lon, lat)
coords <- matrix(
  c(
    ext["xmin"], ext["ymin"],
    ext["xmax"], ext["ymin"],
    ext["xmax"], ext["ymax"],
    ext["xmin"], ext["ymax"],
    ext["xmin"], ext["ymin"]  # close the polygon
  ),
  ncol = 2,
  byrow = TRUE
)

study_area <- sf::st_sfc(
  sf::st_polygon(list(coords)),
  crs = 4326
)

states_au <- rnaturalearth::ne_states(
  country     = "Australia",
  returnclass = "sf"
) |>
  sf::st_transform(4326)

states_world <- rnaturalearth::ne_states(returnclass = "sf") |>
  sf::st_transform(4326)

world_map_globe <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data   = world$geometry,
    fill   = "lightgray",
    colour = NA,
    linewidth = 0.1
  ) +
  
  ggplot2::geom_sf(
    data      = states_au,
    fill      = NA,
    colour    = "white",  # border colour
    linewidth = 0.1        # border thickness
  ) +
  
  ggplot2::geom_sf(
    data      = study_area,
    fill      = NA,
    colour    = "black",
    linewidth = 0.5
  ) +
  
  ggplot2::coord_sf(
    crs = "+proj=ortho +lat_0=-15 +lon_0=135"  # centre of the globe
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(
    panel.border      = ggplot2::element_blank(),
    panel.background  = ggplot2::element_blank()
  )

world_map_globe

# crs_globe <- "+proj=ortho +lat_0=-25 +lon_0=140 +datum=WGS84 +units=m +no_defs"
# 
# # 2. Transform to globe projection
# world_globe <- sf::st_transform(world, crs_globe)
# study_globe <- sf::st_transform(study_area, crs_globe)
# 
# # 3. Bounding box and circle
# bb <- sf::st_bbox(world_globe)
# 
# cx <- (bb["xmin"] + bb["xmax"]) / 2
# cy <- (bb["ymin"] + bb["ymax"]) / 2
# r  <- max(bb["xmax"] - bb["xmin"], bb["ymax"] - bb["ymin"]) / 2
# # r  <- r * 0.98
# 
# theta <- seq(0, 2 * pi, length.out = 361)
# 
# circle_df <- data.frame(
#   x = cx + r * cos(theta),
#   y = cy + r * sin(theta)
# )
# 
# # 4. Plot globe + study area + circular frame
# world_map_globe <- ggplot2::ggplot() +
#   ggplot2::geom_sf(
#     data   = world_globe,
#     fill   = "grey80",
#     colour = NA
#   ) +
#   ggplot2::geom_sf(
#     data      = study_globe,
#     fill      = NA,
#     colour    = "black",
#     linewidth = 0.5
#   ) +
# 
#   ggplot2::geom_sf(
#     data      = states_au,
#     fill      = NA,
#     colour    = "white",  # border colour
#     linewidth = 0.1        # border thickness
#   ) +
# 
#   ggplot2::geom_path(
#     data        = circle_df,
#     mapping     = ggplot2::aes(x = x, y = y),
#     inherit.aes = FALSE,
#     linewidth   = 0.4
#   ) +
#   ggplot2::coord_sf(
#     xlim   = range(circle_df$x),
#     ylim   = range(circle_df$y),
#     expand = FALSE
#   ) +
#   ggplot2::theme_void()
# 
# world_map_globe

cm_ocean_palette <- cmocean::cmocean(name = "balance", start = 0, end = 1)(100)



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


P1 <- ggplot2::ggplot() +
  # basemaps::basemap_gglayer(ext_bbox, map_service = "esri", map_type = "world_ocean_reference") +
  # scale_fill_identity() + 
  
  # tidyterra::geom_spatraster(data = sci_monsoon_raster) +
  tidyterra::geom_spatraster(data = sci_q4_raster) +
  # tidyterra::geom_spatraster(data = sci_high, na.rm = TRUE) +
  
  geom_sf(data = world$geometry, fill = "grey50", color = "grey50", linewidth = 0.2) +
  
  geom_sf(data = SRR_zone$geometry, fill = NA, color = "dodgerblue", linewidth = 0.2) +
  
  geom_sf(data = cities,
          shape = 21,
          colour = "black", 
          fill = "yellow", 
          alpha = 0.5, 
          size = 1.5,
          show.legend = FALSE) +
  
  
  ggsflabel::geom_sf_text_repel(data = cities,
                                colour = "black", 
                                aes(label = Loc), 
                                nudge_x = -2, 
                                nudge_y = 1, 
                                size = 2.5, 
                                force = 1,
                                force_pull = 10,
                                seed = 10) +
  
  
  geom_sf(data = cities2,
          shape = 21,
          colour = "black", 
          fill = "yellow", 
          alpha = 0.5, 
          size = 1.5,
          show.legend = FALSE) +
  
  
  ggsflabel::geom_sf_text_repel(data = cities2,
                                colour = "black", 
                                aes(label = Loc), 
                                nudge_x = 0.5, 
                                nudge_y = 0, 
                                size = 2.5, 
                                force = 1,
                                force_pull = 10,
                                seed = 12) +
  
  
  geom_sf(data = WB,
          shape = 21,,
          colour = "white",
          #fill = "yellow",
          alpha = 1,
          size = 2,
          show.legend = FALSE) +
  
  ggsflabel::geom_sf_text_repel(data = WB,
                                colour = "white",
                                aes(label = Loc),
                                nudge_x = 2,
                                nudge_y = -0.5,
                                size = 2.5,
                                #fontface = "bold",
                                force = 1,
                                force_pull = 10,
                                seed = 15) +
  
  scale_fill_gradientn(
    #colors = viridisLite::plasma(256),   # or use the manual one above
    #trans = 'log10',
    colors = cmocean::cmocean(name = "matter", start = 0.1, end = 0.9)(300),
    #colors = map_effort_dark,
    limits = c(0.01, high),
    breaks = c(0, high),
    labels = c("Low", "High"),
    #labels = scales::comma,
    na.value = "grey10",
    guide = guide_colorbar(frame.colour = "black", ticks.colour = NA)
  ) +
  
  labs(fill = "SCI", x = "Longitude", y = "Latitude", title = "") +
  
  
  
  
  scale_y_continuous(limits = c(-40, -0), breaks = seq(-35, -5, by = 10), expand = c(0,0)) +
  scale_x_continuous(limits = c(140, 170), breaks = seq(145, 165, by = 10),expand = c(0,0)) +
  
  # ggspatial::annotation_scale(location = "bl", 
  #                  width_hint = 0.25,
  #                  pad_x = unit(.5, "cm"),
  #                  pad_y = unit(.5, "cm")) +
  
  ggspatial::annotation_north_arrow(location = "bl",
                                    which_north = "true", 
                                    height = unit(1, "cm"),
                                    width = unit(1, "cm"),
                                    pad_x = unit(4, "cm"),
                                    pad_y = unit(0.25, "cm"),
                                    style =  north_arrow_fancy_orienteering) +
  
  guides(alpha = "none") +
  
  # Add the expanded inset map as an annotation
  annotation_custom(
    grob = ggplotGrob(AUS_map),  # Convert the inset map to a grob
    xmin = 141, xmax = 147, ymin = -35, ymax = -27  # Adjust position & size of inset map
  ) +
  
  # Add manual text at specific coordinates
  annotate("text", x = 141, y = -26, label = "AUS", color = "white", size = 3, fontface = "bold") +
  annotate("text", x = 144, y = -6, label = "PNG", color = "white", size = 3, fontface = "bold") +
  annotate("text", x = 152, y = -15, label = "Coral Sea", color = "white", size = 3, fontface = "italic") +
  annotate("text", x = 154, y = -8.5, label = "Solomon\nSea", color = "white", size = 3, fontface = "italic")+
  annotate("text", x = 139, y = -14, label = "Gulf of\nCarpentaria", color = "white", size = 3, fontface = "italic")+
  annotate("text", x = 145, y = -9, label = "Gulf of\nPapua", color = "white", size = 3, fontface = "italic")+
  annotate("text", x = 142.5, y = -10, label = "Torres Str.", color = "white", size = 2, fontface = "italic")+ 
  
  
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    legend.key.width = rel(0.75),
    legend.key.height = rel(1.5),
    legend.title = element_text(size=8, face = "bold"),
    legend.text = element_text(size =8),
    axis.text = element_text(size = 8),
    #axis.title = element_text(size = 10),
    axis.title = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
    #panel.background = element_rect(fill = "black", colour = NA),
    panel.grid = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")) 


P1
 



P2 <- ggplot2::ggplot() +
  # basemaps::basemap_gglayer(ext_bbox, map_service = "esri", map_type = "world_ocean_reference") +
  # scale_fill_identity() + 
  
  #tidyterra::geom_spatraster(data = sci_monsoon_raster) +
  tidyterra::geom_spatraster(data = sci_monsoon_raster, na.rm = TRUE) +
  
  geom_sf(data = world$geometry, fill = "grey50", color = "grey50", linewidth = 0.2) +
  
  geom_sf(data = SRR_zone$geometry, fill = NA, color = "dodgerblue", linewidth = 0.2) +
  
  geom_sf(data = cities,
          shape = 21,
          colour = "black", 
          fill = "yellow", 
          alpha = 0.5, 
          size = 1.5,
          show.legend = FALSE) +
  coord_sf(xlim = c(140, 170), ylim = c(-40, 0), expand = FALSE) + 
  
  ggsflabel::geom_sf_text_repel(data = cities,
                                colour = "black", 
                                aes(label = Loc), 
                                nudge_x = -2, 
                                nudge_y = 1, 
                                size = 2.5, 
                                force = 1,
                                force_pull = 10,
                                seed = 10) +
  
  
  geom_sf(data = cities2,
          shape = 21,
          colour = "black", 
          fill = "yellow", 
          alpha = 0.5, 
          size = 1.5,
          show.legend = FALSE) +
  
  
  ggsflabel::geom_sf_text_repel(data = cities2,
                                colour = "black", 
                                aes(label = Loc), 
                                nudge_x = 0.5, 
                                nudge_y = 0, 
                                size = 2.5, 
                                force = 1,
                                force_pull = 10,
                                seed = 12) +
  
  
  geom_sf(data = WB,
          shape = 21,,
          colour = "white",
          #fill = "yellow",
          alpha = 1,
          size = 2,
          show.legend = FALSE) +
  
  ggsflabel::geom_sf_text_repel(data = WB,
                                colour = "white",
                                aes(label = Loc),
                                nudge_x = 2,
                                nudge_y = -0.5,
                                size = 2.5,
                                #fontface = "bold",
                                force = 1,
                                force_pull = 10,
                                seed = 15) +
  
  scale_fill_gradientn(
    #colors = viridisLite::plasma(256),   # or use the manual one above
    #trans = 'log10',
    colors = cmocean::cmocean(name = "matter", start = 0, end = 1)(300),
    #colors = map_effort_dark,
    limits = c(0, high),
    breaks = c(0, high),
    #labels = c("Low", "High"),
    #labels = scales::comma,
    na.value = "grey10",
    guide = guide_colorbar(frame.colour = "black", ticks.colour = NA)
  ) +
  
  labs(fill = "SCI", x = "Longitude", y = "Latitude", title = "") +
  
  
  
  
  scale_y_continuous(limits = c(-40, -0), breaks = seq(-35, -5, by = 10), expand = c(0,0)) +
  scale_x_continuous(limits = c(140, 170), breaks = seq(145, 165, by = 10),expand = c(0,0)) +
  
  # ggspatial::annotation_scale(location = "bl", 
  #                  width_hint = 0.25,
  #                  pad_x = unit(.5, "cm"),
  #                  pad_y = unit(.5, "cm")) +
  
  ggspatial::annotation_north_arrow(location = "bl",
                                    which_north = "true", 
                                    height = unit(1, "cm"),
                                    width = unit(1, "cm"),
                                    pad_x = unit(4, "cm"),
                                    pad_y = unit(0.25, "cm"),
                                    style =  north_arrow_fancy_orienteering) +
  
  guides(alpha = "none") +
  
  # Add the expanded inset map as an annotation
  annotation_custom(
    grob = ggplotGrob(AUS_map),  # Convert the inset map to a grob
    xmin = 137, xmax = 145, ymin = -30, ymax = -22  # Adjust position & size of inset map
  ) +
  
  # Add manual text at specific coordinates
  annotate("text", x = 141, y = -26, label = "AUS", color = "white", size = 3, fontface = "bold") +
  annotate("text", x = 144, y = -6, label = "PNG", color = "white", size = 3, fontface = "bold") +
  annotate("text", x = 152, y = -15, label = "Coral Sea", color = "white", size = 3, fontface = "italic") +
  annotate("text", x = 154, y = -8.5, label = "Solomon\nSea", color = "white", size = 3, fontface = "italic")+
  annotate("text", x = 139, y = -14, label = "Gulf of\nCarpentaria", color = "white", size = 3, fontface = "italic")+
  annotate("text", x = 145, y = -9, label = "Gulf of\nPapua", color = "white", size = 3, fontface = "italic")+
  annotate("text", x = 142.5, y = -10, label = "Torres Str.", color = "white", size = 2, fontface = "italic")+ 
  
  
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    legend.key.width = rel(0.75),
    legend.key.height = rel(1.5),
    legend.title = element_text(size=8, face = "bold"),
    legend.text = element_text(size =8),
    axis.text = element_text(size = 8),
    #axis.title = element_text(size = 10),
    axis.title = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
    #panel.background = element_rect(fill = "black", colour = NA),
    panel.grid = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")) 


P2

plot(sdm_monsoon)


WB_95_high_df <- WB_95_high |>
  sf::st_cast("LINESTRING") |>
  sf::st_coordinates() |>
  dplyr::as_tibble() |>
  dplyr::rename(x = X, y = Y) |>
  dplyr::mutate(label = "95% CI") |>
  dplyr::group_by(L1)  # group by individual lines

WB_95_high_df_latlon <- WB_95_high |>
  sf::st_transform(4326) |>
  sf::st_coordinates() |>
  as.data.frame() |>
  dplyr::rename(x = X, y = Y) |>
  dplyr::mutate(label = "95% CI", group = 1)


WB_95_high_df
WB_95_high_df_latlon

GBR_zone_shifted <- GBR_zone
sf::st_geometry(GBR_zone_shifted) <- sf::st_geometry(GBR_zone) + c(-0.05, -0.05)
sf::st_crs(GBR_zone_shifted) <- 4326 




max_values <- terra::app(sci_monsoon_raster, fun = "max", na.rm = TRUE)
high <- global(max_values, fun = "max", na.rm = TRUE) |> as.numeric()
min_values <- terra::app(sci_high, fun = "min", na.rm = TRUE)
low <- global(min_values, fun = "min", na.rm = TRUE) |> as.numeric()
high = 50

low = 0
high = 70

sci_monsoon_raster
plot(sci_monsoon_raster, range = c(5, 130))



max_values <- terra::app(sci_monsoon_raster, fun = "max", na.rm = TRUE)
high <- round(global(max_values, fun = "max", na.rm = TRUE), 2) |> as.numeric() 
high = 1

na_col <- cmocean::cmocean("ice", direction = 1)(300)[[1]]

P_sci_fnq <- ggplot2::ggplot() +
  # tidyterra::geom_spatraster(data = sci_q4_raster) +
  tidyterra::geom_spatraster(data = sci_monsoon_raster) +
  # tidyterra::geom_spatraster(data = sci_raster) +
  # tidyterra::geom_spatraster(data = sci_high, na.rm = TRUE) +
  # geom_sf(data = GBR_zone_shifted, fill = NA, color = "green", alpha = 1, linewidth = 0.5, linetype = "dashed") +
  geom_sf(data = world$geometry, fill = "grey20", color = "black", linewidth = 0.15) +
  
  # geom_sf(data = SRR_zone$geometry, fill = NA, color = "green", linewidth = 0.5) +
  
  geom_sf(data = WB_95,
          aes(linetype = factor("Home Range", levels = c("Home Range", "95% CI"))),
          fill = NA, color = "yellow", alpha = 1, linewidth = 0.35, show.legend = TRUE) +

  geom_sf(data = WB_95_high,
          aes(linetype = factor("95% CI", levels = c("Home Range", "95% CI"))),
          fill = NA, color = "yellow", alpha = 1, linewidth = 0.35, show.legend = TRUE) +

  geomtextpath::geom_textpath(
    data = WB_95_high_df_latlon,
    aes(x = x, y = y, label = label, group = L1),
    size = 2,
    color = "yellow",
    linewidth = 0.5,
    linetype = "blank",
    vjust = 0,
    text_smoothing = 1
  ) +

  
  geom_sf(data = GBR_features |> dplyr::filter(FEAT_NAME == "Reef"), fill = NA, color = "grey50", alpha = 1, linewidth = 0.05) +

  geom_sf(data = GBR_zone, fill = NA, color = "grey70", alpha = 1, linewidth = 0.25, linetype = "longdash") +
  
  
  geom_sf(data = cities,
          shape = 21,
          colour = "black", 
          fill = "yellow", 
          alpha = 1, 
          size = 1,
          show.legend = FALSE) +
  
  coord_sf(xlim = c(142, 147), ylim = c(-18, -10), expand = FALSE) + 
  
  ggsflabel::geom_sf_text_repel(data = cities,
                                colour = "white", 
                                aes(label = Loc), 
                                nudge_x = -0.75, 
                                nudge_y = 0.25, 
                                size = 2.5, 
                                force = 1,
                                force_pull = 10,
                                segment.colour = "black",  
                                segment.size   = 0.3,       
                                segment.linetype = "solid",
                                seed = 10) +

  
  ggsflabel::geom_sf_text_repel(data = WB,
                                colour = "white",
                                aes(label = Loc),
                                nudge_x = 0.8,
                                nudge_y = -0.6,
                                size = 2.5,
                                fontface = "bold",
                                force = 1,
                                force_pull = 10,
                                segment.colour = "white",  
                                segment.size   = 0.3,       
                                segment.linetype = "solid",
                                seed = 15) +
  
  scale_fill_gradientn(
    colours = cmocean::cmocean(name = "ice", direction = 1, start = 0, end = 1)(300),
    limits  = c(0, high),
    breaks  = c(0, high),
    labels = c("low", "high"),
    na.value = na_col
  ) +
  guides(
    alpha = "none",
    linetype = guide_legend(
      override.aes = list(
        fill  = c(NA, NA),
        color = c("yellow", "yellow")
      )
    ),
    fill = guide_colorbar(
      order          = 1,
      frame.colour   = NA,
      ticks.colour   = NA,
      frame.linewidth = 0.3,
      tick.linewidth  = 0.3,
      barheight       = grid::unit(2.25, "cm"),  # try 0.4–0.8
      barwidth        = grid::unit(0.25, "cm")  # thin bar
    )
  ) +
  
  labs(fill = "SCI", x = "Longitude", y = "Latitude", title = "") +
  
  scale_linetype_manual(
    name   = "Whale Shark\nConstellation\nSpace Use",
    values = c("Home Range" = "solid", "95% CI" = "dashed"),
    guide = guide_legend(
      order = 2,
      override.aes = list(
        colour   = "yellow",
        linewidth = 0.1,   # thin legend lines
        fill     = NA
      )
    )
  ) +
  
  scale_y_continuous(limits = c(-18, -10), breaks = seq(-16, -12, by = 2), expand = c(0,0)) +
  scale_x_continuous(limits = c(142, 147), breaks = seq(143, 146, by = 2),expand = c(0,0)) +
  
  ggspatial::annotation_scale(location = "bl",
                              pad_x = unit(2.25, "cm"),
                              pad_y = unit(0.5, "cm"),
                              style = "ticks",
                              line_width = 0.2,
                              text_cex  = 0.4,
                              # plot_unit = "km",
                              width_hint = 0.25,
                              line_col = "white",
                              text_col = "white"
                              ) +
  
  # ggspatial::annotation_north_arrow(location = "bl",
  #                                   which_north = "true", 
  #                                   height = unit(1, "cm"),
  #                                   width = unit(1, "cm"),
  #                                   pad_x = unit(0.5, "cm"),
  #                                   pad_y = unit(0.25, "cm"),
  #                                   style =  north_arrow_fancy_orienteering) +
  
  # guides(alpha = "none",
  #        linetype = guide_legend(
  #          override.aes = list(
  #            fill = c(NA, NA),
  #            color = c("blue1", "blue1")
  #          )
  #        ),
  #        fill = guide_colorbar(order = 1)) +
  
  annotate("text", x = 142.5, y = -10.4, label = "Torres Str.", color = "white", size = 2, fontface = "italic")+ 
  
  theme_bw() +
  theme(
    panel.grid.major = element_blank(), # eliminate major grids
    panel.grid.minor = element_blank(), # eliminate minor grids
    # text = element_text(family="serif"),
    legend.key.height = grid::unit(0.5, "cm"),  # smaller boxes vertically
    legend.key.width  = grid::unit(0.5,  "cm"),  # narrower horizontally
    legend.spacing.y  = grid::unit(0.5, "cm"),  # less vertical space between items
    legend.position = c(0.05, 0.05),     # inside: (x, y) in [0,1]
    legend.justification = c("left", "bottom"),
    legend.direction = "vertical",
    legend.title = element_text(size=7, face = "bold", color = "white"),
    legend.text = element_text(size = 6, colour = "white"),
    axis.ticks.length = grid::unit(-1, "mm"),  # a bit shorter
    # width of tick marks in mm
    axis.ticks = element_line(linewidth = .25, colour = "black"),
    
    axis.text.x = ggplot2::element_text(
      size   = 5,
      colour = "black",
      margin = grid::unit(c(-3, 0, 0, 0), "mm")  # t, r, b, l  (pull up)
    ),
    axis.text.y = ggplot2::element_text(
      size   = 5,
      angle  = 270,
      colour = "black",
      hjust  = 0.5,          
      vjust  = 0.5,     
      margin = grid::unit(c(0, -3, 0, 0), "mm")  # pull right into panel
    ),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = NA, colour = NA),
    #axis.title = element_text(size = 10),
    axis.title = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.5),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin = unit(c(0, 0, 0.2, 0), "cm")
  )


P_sci_fnq

ggsave("SCI_FN_GBR.png", plot = P_sci_fnq, path ="Output_Figs", scale =1, width = 6.5, height = 10, units = "cm", dpi = 400)

ggsave("SCI_FN_GBR.pdf", plot = P_sci_fnq, path ="Output_Figs", scale =1, width = 6.5, height = 10, units = "cm", dpi = 400, device = "pdf")

ggsave("SCI_FN_GBR.svg", plot = P_sci_fnq, path ="Output_Figs", scale =1, width = 6.5, height = 10, units = "cm", dpi = 400)




P_sci_png <- ggplot2::ggplot() +
  # tidyterra::geom_spatraster(data = sci_q4_raster) +
  tidyterra::geom_spatraster(data = sci_monsoon_raster) +
  # tidyterra::geom_spatraster(data = sci_raster) +
  # tidyterra::geom_spatraster(data = sci_high, na.rm = TRUE) +
  # geom_sf(data = GBR_zone_shifted, fill = NA, color = "green", alpha = 1, linewidth = 0.5, linetype = "dashed") +
  geom_sf(data = world$geometry, fill = "grey20", color = "black", linewidth = 0.15) +
  
  
  geom_sf(data = cities2,
          shape = 21,
          colour = "black", 
          fill = "yellow", 
          alpha = 0.5, 
          size = 2,
          show.legend = FALSE) +
  
  ggsflabel::geom_sf_text_repel(data = cities2,
                                colour = "white", 
                                aes(label = Loc), 
                                nudge_x = 0.75, 
                                nudge_y = 0, 
                                size = 2, 
                                force = 1,
                                force_pull = 10,
                                segment.colour = "black",  
                                segment.size   = 0.15,       
                                segment.linetype = "solid",
                                seed = 12) +
  

  coord_sf(
    xlim   = c(143.5, 156.5),
    ylim   = c(-13.05, -2.95),  # tiny buffer beyond the data area
    expand = FALSE
  ) +
  

  scale_fill_gradientn(
    colours = cmocean::cmocean(name = "ice", direction = 1, start = 0, end = 1)(300),
    limits  = c(0, high),
    breaks  = c(0, high),
    labels = c("low", "high"),
    na.value = na_col
  ) +
  guides(
    alpha = "none",
    # linetype = guide_legend(
    #   override.aes = list(
    #     fill  = c(NA, NA),
    #     color = c("blue1", "blue1")
    #   )
    # ),
    fill = guide_colorbar(
      order          = 1,
      frame.colour   = NA,
      ticks.colour   = NA,
      frame.linewidth = 0.3,
      tick.linewidth  = 0.3,
      barheight       = grid::unit(2, "cm"),  # try 0.4–0.8
      barwidth        = grid::unit(0.2, "cm")  # thin bar
    )
  ) +
  
  labs(fill = "SCI", x = "Longitude", y = "Latitude", title = "") +
  
  # scale_linetype_manual(
  #   name   = "Whale Shark\nConstellation\nSpace Use",
  #   values = c("Home Range" = "solid", "95% CI" = "dashed"),
  #   guide = guide_legend(
  #     order = 2,
  #     override.aes = list(
  #       colour   = "blue1",
  #       linewidth = 0.1,   # thin legend lines
  #       fill     = NA
  #     )
  #   )
  # ) +
  
  scale_y_continuous(
    breaks = seq(-12, -4, by = 3),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    breaks = seq(147, 155, by = 5),
    expand = c(0, 0)
  ) +
  
  ggspatial::annotation_scale(location = "bl",
                              pad_x = unit(1.5, "cm"),
                              pad_y = unit(0.75, "cm"),
                              style = "ticks",
                              line_width = 0.5,
                              text_cex  = 0.5,
                              # plot_unit = "km",
                              width_hint = 0.25,
                              line_col = "white",
                              text_col = "white"
  ) +

  # ggspatial::annotation_north_arrow(location = "bl",
  #                                   which_north = "true", 
  #                                   height = unit(1, "cm"),
  #                                   width = unit(1, "cm"),
  #                                   pad_x = unit(0.5, "cm"),
  #                                   pad_y = unit(0.25, "cm"),
  #                                   style =  north_arrow_fancy_orienteering) +
  
  # guides(alpha = "none",
  #        linetype = guide_legend(
  #          override.aes = list(
  #            fill = c(NA, NA),
  #            color = c("blue1", "blue1")
  #          )
  #        ),
  #        fill = guide_colorbar(order = 1)) +
  
  annotate("text", x = 152, y = -6.5, label = "Solomon Sea", color = "white", size = 2.5, fontface = "italic")+
  annotate("text", x = 145, y = -8.5, label = "Gulf of\nPapua", color = "white", size = 2.5, fontface = "italic")+
  
  theme_bw() +
  theme(
    panel.grid.major = element_blank(), # eliminate major grids
    panel.grid.minor = element_blank(), # eliminate minor grids
    # text = element_text(family="serif"),
    legend.key.height = grid::unit(0.5, "cm"),  # smaller boxes vertically
    legend.key.width  = grid::unit(0.5,  "cm"),  # narrower horizontally
    legend.spacing.y  = grid::unit(0.5, "cm"),  # less vertical space between items
    legend.position = c(0.05, 0.55),     # inside: (x, y) in [0,1]
    legend.justification = c("left", "bottom"),
    legend.direction = "vertical",
    legend.title = element_text(size=7, face = "bold", color = "white"),
    legend.text = element_text(size = 6, colour = "white"),
    axis.ticks.length = grid::unit(-1, "mm"),  # a bit shorter
    # width of tick marks in mm
    axis.ticks = element_line(linewidth = .25, colour = "white"),
    
    axis.text.x = ggplot2::element_text(
      size   = 5,
      colour = "white",
      margin = grid::unit(c(-3, 0, 0, 0), "mm")  # t, r, b, l  (pull up)
    ),
    axis.text.y = ggplot2::element_text(
      size   = 5,
      angle  = 270,
      hjust  = 0.5,           # centre on the tick horizontally
      vjust  = 0.5,           # centre on the tick vertically
      colour = "white",
      margin = grid::unit(c(0, -3, 0, 0), "mm")  # pull right into panel
    ),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = NA, colour = NA),
    #axis.title = element_text(size = 10),
    axis.title = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.5),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin = unit(c(-0.5, 0.1, 0.5, 0.1), "cm")
  )


P_sci_png

P_sci_png$coordinates


asp <- P_sci_png$layout$panel_params[[1]]$aspect_ratio
asp


# lon_min <- 143.5
# lon_max <- 156.5
# lat_min <- -13
# lat_max <- -3
# 
# lon_range <- lon_max - lon_min   # 13
# lat_range <- lat_max - lat_min   # 10
# mid_lat   <- (lat_min + lat_max) / 2
# 
# aspect <- lat_range / (lon_range * cos(mid_lat * pi/180))
# aspect
# 
# height_cm  <- 10
# width_cm <- height_cm / aspect   # ~10.1


ggsave("SCI_PNG.png", plot = P_sci_png, path ="Output_Figs", scale =1, width = width_cm, height = height_cm, units = "cm", dpi = 400)

ggsave("SCI_PNG.pdf", plot = P_sci_png, path ="Output_Figs", scale =1, width = width_cm, height = height_cm, units = "cm", dpi = 400, device = "pdf")














# Map ship density  -------------------------------------------------------



ships_crop
ships_crop


cities3 <- data.frame(Loc = c(
  "Cairns", 
  "Abbot Point",
  "Hay Point",
  "Gladstone", 
  "Brisbane", 
  "P. Newcastle"
  # "Sydney"
  ),
  Port = c(
    "Cargo",
    "Coal",
    "Coal",
    "Coal",
    "Cargo",
    "Coal"
    # "City"
    ),
  lat = c( 
    -16.918246, 
    -19.8993, 
    -21.2928, 
    -23.8416, 
    -27.4705, 
    -32.90
    # -33.911609
    ),
  lon = c( 
    145.771359, 
    148.0813,  
    149.2559, 
    151.2498, 
    153.026,
    151.76
    # 151.186715
    )) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) 

aus_states <- rnaturalearth::ne_states(
  country     = "Australia",
  returnclass = "sf"
)



world_map_globe <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data   = world$geometry,
    fill   = "lightgray",
    colour = NA,
    linewidth = 0.1
  ) +
  
  # ggplot2::geom_sf(
  #   data      = states_au,
  #   fill      = NA,
  #   colour    = "white",  
  #   linewidth = 0.01        
  # ) +
  
  ggplot2::geom_sf(
    data      = study_area,
    fill      = NA,
    colour    = "firebrick1",
    linewidth = 0.4
  ) +
  
  ggplot2::coord_sf(
    crs = "+proj=ortho +lat_0=-15 +lon_0=135"  # centre of the globe
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(
    panel.border      = ggplot2::element_blank(),
    panel.background  = ggplot2::element_blank()
  )

world_map_globe



fnq_aoi <- sf::st_as_sfc(
    sf::st_bbox(c(xmin = 142, xmax = 147, ymin = -18, ymax = -10), crs = sf::st_crs(4326))
  ) |>
  sf::st_as_sf() |>
  dplyr::mutate(name = "FNQ SCI")


png_aoi <- sf::st_as_sfc(
  sf::st_bbox(c(xmin = 143.5, xmax = 156.5, ymin = -13.05, ymax = -2.95), crs = sf::st_crs(4326))
) |>
  sf::st_as_sf() |>
  dplyr::mutate(name = "PNG SCI")




sz = 2
P4 <- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = ships_crop) +
  
  geom_sf(data = world$geometry, fill = "grey20", color = "grey20", linewidth = 0.5) +
  
  geom_sf(data = aus_states,
          fill   = NA,
          colour = "black",
          linewidth = 0.1) +
  
  # geom_sf(data = SRR_zone[2:3, ]$geometry, fill = NA, color = "green2", linewidth = 0.5) +
  
  geom_sf(data = cities3,
          shape = 21,
          colour = "black", 
          fill = "red", 
          alpha = 0.5, 
          size = 1,
          show.legend = FALSE) +
  
  coord_sf(xlim = c(140, 170), ylim = c(-40, 0), expand = FALSE) + 
  
  geom_sf(
    data = fnq_aoi,
    fill = NA,
    colour = "white",
    linewidth = 0.2,
    linetype = "dashed"
  ) +
  
  geom_sf(
    data = png_aoi,
    fill = NA,
    colour = "white",
    linewidth = 0.2,
    linetype = "dashed"
  ) +
  
  ggsflabel::geom_sf_text_repel(data = cities3,
                                colour = "white", 
                                aes(label = Loc), 
                                nudge_x = -1, 
                                nudge_y = 0, 
                                size = 1.75, 
                                # force = 10,
                                # force_pull = 10,
                                segment.colour = "black",  
                                segment.size   = 0.15,       
                                segment.linetype = "solid",
                                seed = 10) +
  
  
  geom_sf(data = cities2,
          shape = 21,
          colour = "black", 
          fill = "red", 
          alpha = 0.5, 
          size = 1,
          show.legend = FALSE) +
  
  
  ggsflabel::geom_sf_text_repel(data = cities2,
                                colour = "white", 
                                aes(label = Loc), 
                                nudge_x = 0.5, 
                                nudge_y = 0, 
                                size = 1.75, 
                                force = 1,
                                force_pull = 10,
                                segment.colour = "black",  
                                segment.size   = 0.15,       
                                segment.linetype = "solid",
                                seed = 12) +
  

  
  scale_fill_gradientn(
    # colors = viridisLite::plasma(256),   # or use the manual one above
    colors = cmocean::cmocean(name = "thermal", start = 0, end = 1)(300),
    trans = 'log10',
    #colors = map_effort_dark,
    labels = scales::comma,
    limits = c(1.5, 400),
    breaks = c(5, 10, 25, 100, 300),
    # labels = c("Low", "High"),
    na.value = NA,
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black",
                           frame.linewidth = 0.15,
                           tick.linewidth = 0.15)
  ) +
  
  labs(fill = "Vessel\nDensity", x = "Longitude", y = "Latitude", title = "") +
  
  scale_y_continuous(limits = c(-40, -0), breaks = seq(-35, -5, by = 10), expand = c(0,0)) +
  scale_x_continuous(limits = c(140, 170), breaks = seq(145, 165, by = 10),expand = c(0,0)) +
  
  # ggspatial::annotation_scale(location = "bl",
  #                             pad_x = unit(0.5, "cm"),
  #                             pad_y = unit(1.25, "cm"),
  #                             style = "ticks",
  #                             #plot_unit = "km",
  #                             width_hint = 0.15,
  #                             line_col = "white",
  #                             text_col = "white"
  # ) +
  
  ggspatial::annotation_north_arrow(location = "bl",
                                    which_north = "true", 
                                    height = unit(0.6, "cm"),
                                    width = unit(0.6, "cm"),
                                    pad_x = unit(0.4, "cm"),
                                    pad_y = unit(0.75, "cm"),
                                    style =  north_arrow_fancy_orienteering(text_col = "white", text_size = 6)) +
  
  # annotation_custom(
  #   grob = ggplotGrob(AUS_map),  # Convert the inset map to a grob
  #   xmin = 141, xmax = 149, ymin = -33, ymax = -22  # Adjust position & size of inset map
  # ) +
  
  annotation_custom(
    grob = ggplotGrob(world_map_globe),  # Convert the inset map to a grob
    xmin = 140, xmax = 150, ymin = -37, ymax = -20  # Adjust position & size of inset map
  ) +
  
  annotate("text", x = 145, y = -29.75, label = "AUS", color = "black", size = 1.75, fontface = "bold") +
  annotate("text", x = 143, y = -22, label = "QLD", color = "black", size = sz, fontface = "bold") +
  annotate("text", x = 145, y = -33, label = "NSW", color = "black", size = sz, fontface = "bold") +
  annotate("text", x = 144, y = -6, label = "PNG", color = "white", size = sz, fontface = "bold") +
  annotate("text", x = 165, y = -21, label = "NC", color = "white", size = sz, fontface = "bold") +
  annotate("text", x = 158.5, y = -8, label = "SI", color = "white", size = sz, fontface = "bold") +
  annotate("text", x = 166.75, y = -15.5, label = "VU", color = "white", size = sz, fontface = "bold") +
  annotate("text", x = 149, y = -13.5, label = "Coral Sea", color = "white", size = sz, fontface = "italic") +
  annotate("text", x = 153, y = -7.5, label = "Solomon\nSea", color = "white", size = sz, fontface = "italic")+
  annotate("text", x = 160, y = -35, label = "Tasman Sea", color = "white", size = sz, fontface = "italic")+
  # annotate("text", x = 139, y = -14, label = "Gulf of\nCarpentaria", color = "blue4", size = 3, fontface = "italic")+
  # annotate("text", x = 163.75, y = -30, label = "AUS SRR", color = "green2", size = 3, fontface = "italic", angle = 270)+
  # annotate("text", x = 145, y = -9, label = "Gulf of\nPapua", color = "white", size = 3, fontface = "italic")+
  annotate("text", x = 142.5, y = -10, label = "Torres Str.", color = "white", size = 1.25, fontface = "italic")+
  
  annotate("text", x = 142.75, y = -17.25, label = "(1)", color = "white", size = 2, fontface = "bold") +
  annotate("text", x = 155.75, y = -3.5, label = "(2)", color = "white", size = 2, fontface = "bold") +
  annotate("text", x = 141, y = -1, label = "A", color = "white", size = 4) +
  
  
  
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.width = rel(1.25),
    legend.key.height = rel(0.5),
    legend.title = element_text(size=6),
    legend.text = element_text(size = 5),
    # axis.text = element_text(size = 8),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    #axis.title = element_text(size = 10),
    axis.title = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
    panel.background = element_rect(fill = "black", colour = NA),
    panel.grid = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")) 



P4

ggsave("Shipping_Desnity_meanMonth_2018x2025_EastCoastAUS_GFW.png", plot = P4, path ="Output_Figs", scale =1, width = 9, height = 12, units = "cm", dpi = 400)

ggsave("Shipping_Desnity_meanMonth_2018x2025_EastCoastAUS_GFW.eps", plot = P4, path ="Output_Figs", scale =1, width = 9, height = 12, units = "cm")

ggsave("Shipping_Desnity_meanMonth_2018x2025_EastCoastAUS_GFW.pdf", plot = P4, path ="Output_Figs", scale =1, width = 9, 
       height = 12,
       units = "cm", 
       device = "pdf")





library(leaflet)
library(mapview)

# Convert to RGB or use a color palette for single-layer rasters
# Log-transform raster (adding 1 to avoid log(0) if needed)
ships_log <- log10(ships_crop)
ships_masked <- terra::ifel(ships_crop >= 1, ships_crop, NA)

ships_masked_log <- ships_masked |>
  terra::app(\(x) log10(x + 1))

# Create palette using log-transformed range
pal <- leaflet::colorNumeric(
  palette = cmocean::cmocean("thermal")(100),
  domain = terra::values(ships_masked),
  na.color = "transparent"
)

# leaflet() |>
#   addProviderTiles("CartoDB.DarkMatter") |>
#   addRasterImage(ships_log, colors = pal, opacity = 1, project = TRUE) |>
#   addLegend(pal = pal,
#             values = terra::values(ships_log),
#             title = "Vessel Count",
#             labFormat = leaflet::labelFormat(transform = function(x) round(10^x)))  # back-transform labels

ships_crop

mapview::mapview(ships_masked_log, col.regions = cmocean::cmocean("thermal")(100), layer.name = "Vessel Count") +
  mapview::mapview(cities, zcol = "Loc", col.regions = "red") +
  mapview::mapview(WB, zcol = "Loc", col.regions = "red") #+
  # mapview::mapview(world, col.regions = "grey20", alpha.regions = 1)





# SDM map  ----------------------------------------------------------------

season_2_ensemble <- terra::rast("/Volumes/Ingo_PhD/PhD_Data_Analysis/PhD_WhaleSharks_SDMs_Enviro_Layers/Chapter2/SDM_Outputs_Rev/SDM_whalesharks_Tracks_mp_crwPA_Ensemble_seasonal2_mean_climate.tif")

sdm <- season_2_ensemble$Monsoon_Season_Ensemble_Tracking


plot(sdm)

w3 <- matrix(1, 3, 3)
w5 <- matrix(1, 5, 5)
sdm_f <-
  sdm |>
  # Pass 1: fill NAs with 3x3 local mean
  (\(r) terra::cover(
    r,
    terra::focal(r, w = w3, fun = base::mean, na.rm = TRUE)
  ))() |>
  # Pass 2: repeat 3x3 to grow into slightly larger gaps
  (\(r) terra::cover(
    r,
    terra::focal(r, w = w3, fun = base::mean, na.rm = TRUE)
  ))() |>
  # Pass 3 (optional): one 5x5 sweep for stubborn holes
  (\(r) terra::cover(
    r,
    terra::focal(r, w = w5, fun = base::mean, na.rm = TRUE)
  ))() |>
  # Clamp to [0, 1]
  (\(r) terra::ifel(r < 0, 0, terra::ifel(r > 1, 1, r)))()

plot(sdm_f)


P_sdm <- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = sdm_f) +
  
  geom_sf(data = world$geometry, fill = "grey20", color = "grey20", linewidth = 0.5) +
  
  geom_sf(data = aus_states,
          fill   = NA,
          colour = "black",
          linewidth = 0.1) +
  
  
  ggsflabel::geom_sf_text_repel(data = WB,
                                colour = "black",
                                aes(label = Loc),
                                nudge_x = 3,
                                nudge_y = -0.5,
                                size = 2,
                                fontface = "bold",
                                force = 1,
                                force_pull = 10,
                                segment.colour = "black",  
                                segment.size   = 0.3,    
                                seed = 15) +
  
  geom_sf(
    data = fnq_aoi,
    fill = NA,
    colour = "black",
    linewidth = 0.2,
    linetype = "dashed"
  ) +
  
  geom_sf(
    data = png_aoi,
    fill = NA,
    colour = "black",
    linewidth = 0.2,
    linetype = "dashed"
  ) +
  
  
  
  scale_fill_gradientn(
    # colours = cm_ocean_palette,
    colours = viridisLite::turbo(n = 100, direction = 1, begin = 0, end = 1),
    limits  = c(0, 1),
    breaks  = seq(0, 1, by = 0.2),
    labels  = scales::number_format(accuracy = 0.1),
    oob     = scales::squish,
    name    = "Rel. Habitat\nSuitability",
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black",
                           frame.linewidth = 0.15,
                           tick.linewidth = 0.15)
  ) +
  
  labs(fill = "Rel. Habitat\nSuitability", x = "Longitude", y = "Latitude", title = "") +
  
  scale_y_continuous(limits = c(-40, 0), breaks = seq(-35, -5, by = 5), expand = c(0,0)) +
  scale_x_continuous(limits = c(140, 170), breaks = seq(145, 165, by = 5),expand = c(0,0)) +
  
  annotate("text", x = 142.75, y = -17.25, label = "(1)", color = "black", size = 2, fontface = "bold") +
  annotate("text", x = 155.75, y = -3.5, label = "(2)", color = "black", size = 2, fontface = "bold") +
  annotate("text", x = 141, y = -1, label = "B", color = "black", size = 4) +
  
 
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.width = rel(1.25),
    legend.key.height = rel(0.5),
    legend.title = element_text(size=6),
    legend.text = element_text(size = 5),
    # axis.text = element_text(size = 8),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    #axis.title = element_text(size = 10),
    axis.title = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
    panel.background = element_rect(fill = "black", colour = NA),
    panel.grid = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")) 




P_sdm


ggsave("SDM_monsoon_panel.png", plot = P_sdm, path ="Output_Figs", 
       scale = 1, 
       width = 9, 
       height = 12,
       units = "cm", dpi = 400)

ggsave("SDM_monsoon_panel.eps", plot = P_sdm, path ="Output_Figs", 
       scale = 1, 
       width = 9, 
       height = 12, 
       units = "cm"
       )

ggsave("SDM_monsoon_panel.pdf", plot = P_sdm, 
       path ="Output_Figs", scale =1, 
       width = 9, 
       height = 12, 
       units = "cm",
       device = "pdf"
       )
 ?ggsave






# Climate change  ---------------------------------------------------------


CC_monsoon <- terra::rast("/Users/ingo/Library/CloudStorage/OneDrive-JamesCookUniversity/02_PhD/06_Chapters/DataChapters/Chapter4_Whalesharks_Risks/ClimateChange/data/Processed_files/CC_monsoon.tif")

CC_monsoon_res <- terra::resample(CC_monsoon, ships_crop, method = "bilinear")
names(CC_monsoon_res)



CC_monsoon_res <- CC_monsoon_res[[ !stringr::str_detect(names(CC_monsoon_res), "ssp370") ]]
CC_monsoon_res <- CC_monsoon_res[[ !stringr::str_detect(names(CC_monsoon_res), "Climate_2100") ]]

# CC_585_monsoon <- CC_monsoon_res$ssp585_2090_2100
# plot(CC_585_monsoon, range = c(0, 1))
# 
# CC_245_monsoon <- CC_monsoon_res$ssp245_2090_2100
# CC_126_monsoon <- CC_monsoon_res$ssp126_2090_2100
# 
# 
# 
# CC_126_monsoon_sci <- CC_126_monsoon * ships_q01_qnorm 
# CC_245_monsoon_sci <- CC_245_monsoon * ships_q01_qnorm 
# CC_585_monsoon_sci <- CC_585_monsoon * ships_q01_qnorm 
# 
# plot(CC_585_monsoon_sci)
# 
# CC_126_monsoon_sci
# CC_245_monsoon_sci
# CC_585_monsoon_sci


CC_monsoon_sci <- CC_monsoon_res * ships_q01_qnorm 

na_col <- cmocean::cmocean("ice", direction = 1)(300)[[1]]





P_sci_cc_126 <- ggplot2::ggplot() +
  
  # tidyterra::geom_spatraster(data = CC_126_monsoon_sci, na.rm = TRUE) +
  tidyterra::geom_spatraster(data = CC_monsoon_sci$ssp126_2090_2100, na.rm = TRUE) +
  
  geom_sf(data = world$geometry, fill = "grey20", color = "black", linewidth = 0.15) +
  
  
  geom_sf(data = aus_states,
          fill   = NA,
          colour = "black",
          linewidth = 0.1) +
  
  coord_sf(xlim = c(140, 170), ylim = c(-40, 0), expand = FALSE) + 
  
  
  scale_fill_gradientn(
    colours = cmocean::cmocean(name = "ice", direction = 1, start = 0, end = 1)(300),
    limits  = c(0, high),
    breaks  = c(0, high),
    labels = c("low", "high"),
    na.value = na_col
  ) +
  
  labs(fill = "SCI", x = "Longitude", y = "Latitude", title = "SSP1-2.6") +
  
  scale_y_continuous(limits = c(-40, -0), breaks = seq(-35, -5, by = 10), expand = c(0,0)) +
  scale_x_continuous(limits = c(140, 170), breaks = seq(145, 165, by = 10),expand = c(0,0)) +
  

  theme(
    legend.position = "none",
    axis.text = element_text(size = 8),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 10),
    panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
    panel.grid = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")) 


P_sci_cc_126




P_sci_cc_245 <- ggplot2::ggplot() +
  
  tidyterra::geom_spatraster(data = CC_monsoon_sci$ssp245_2090_2100, na.rm = TRUE) +
  
  geom_sf(data = world$geometry, fill = "grey20", color = "black", linewidth = 0.15) +
  
  
  geom_sf(data = aus_states,
          fill   = NA,
          colour = "black",
          linewidth = 0.1) +
  
  coord_sf(xlim = c(140, 170), ylim = c(-40, 0), expand = FALSE) + 
  
  scale_fill_gradientn(
    colours = cmocean::cmocean(name = "ice", direction = 1, start = 0, end = 1)(300),
    limits  = c(0, high),
    breaks  = c(0, high),
    labels = c("low", "high"),
    na.value = na_col
  ) +
  
  labs(fill = "SCI", x = "Longitude", y = "Latitude", title = "SSP2-4.5") +
  
  scale_y_continuous(limits = c(-40, -0), breaks = seq(-35, -5, by = 10), expand = c(0,0)) +
  scale_x_continuous(limits = c(140, 170), breaks = seq(145, 165, by = 10),expand = c(0,0)) +
  
  
  
  theme(
    legend.position = "none",
    axis.text = element_text(size = 8),
    # axis.text.y = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 10),
    panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
    panel.grid = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")) 


P_sci_cc_245




P_sci_cc_585 <- ggplot2::ggplot() +
  
  tidyterra::geom_spatraster(data = CC_monsoon_sci$ssp585_2090_2100, na.rm = TRUE) +
  
  geom_sf(data = world$geometry, fill = "grey20", color = "black", linewidth = 0.15) +
  
  
  geom_sf(data = aus_states,
          fill   = NA,
          colour = "black",
          linewidth = 0.1) +
  
  coord_sf(xlim = c(140, 170), ylim = c(-40, 0), expand = FALSE) + 
  
  scale_fill_gradientn(
    colours = cmocean::cmocean(name = "ice", direction = 1, start = 0, end = 1)(300),
    limits  = c(0, high),
    breaks  = c(0, high),
    labels = c("low", "high"),
    na.value = na_col
  ) +
  
  labs(fill = "SCI", x = "Longitude", y = "Latitude", title = "SSP5-8.5") +
  
  scale_y_continuous(limits = c(-40, -0), breaks = seq(-35, -5, by = 10), expand = c(0,0)) +
  scale_x_continuous(limits = c(140, 170), breaks = seq(145, 165, by = 10),expand = c(0,0)) +
  
  
  theme(
    legend.position = "none",
    axis.text = element_text(size = 8),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 10),
    panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
    panel.grid = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")) 


P_sci_cc_585







sci_current <- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = sci_monsoon_raster, na.rm = TRUE) +
  
  geom_sf(data = world$geometry, fill = "grey20", color = "black", linewidth = 0.15) +
  
  
  geom_sf(data = aus_states,
          fill   = NA,
          colour = "black",
          linewidth = 0.1) +
  
  # geom_sf(data = SRR_zone[2:3, ]$geometry, fill = NA, color = "green2", linewidth = 0.5) +
  
  geom_sf(data = cities3,
          shape = 21,
          colour = "black", 
          fill = "yellow", 
          alpha = 0.5, 
          size = 1,
          show.legend = FALSE) +
  
  coord_sf(xlim = c(140, 170), ylim = c(-40, 0), expand = FALSE) + 
  
  
  ggsflabel::geom_sf_text_repel(data = cities3,
                                colour = "black", 
                                aes(label = Loc), 
                                nudge_x = -1, 
                                nudge_y = 0, 
                                size = 1.75, 
                                # force = 10,
                                # force_pull = 10,
                                segment.colour = "black",  
                                segment.size   = 0.15,       
                                segment.linetype = "solid",
                                seed = 10) +
  
  
  geom_sf(data = cities2,
          shape = 21,
          colour = "black", 
          fill = "yellow", 
          alpha = 0.5, 
          size = 1,
          show.legend = FALSE) +
  
  
  ggsflabel::geom_sf_text_repel(data = cities2,
                                colour = "black", 
                                aes(label = Loc), 
                                nudge_x = 0.5, 
                                nudge_y = 0, 
                                size = 1.75, 
                                force = 1,
                                force_pull = 10,
                                segment.colour = "black",  
                                segment.size   = 0.15,       
                                segment.linetype = "solid",
                                seed = 12) +
  
  
  
  scale_fill_gradientn(
    colours = cmocean::cmocean(name = "ice", direction = 1, start = 0, end = 1)(300),
    limits  = c(0, high),
    breaks  = c(0, high),
    labels = c("low", "high"),
    na.value = na_col
  ) +
  
  labs(fill = "SCI", x = "Longitude", y = "Latitude", title = "Current Climate") +
  
  scale_y_continuous(limits = c(-40, -0), breaks = seq(-35, -5, by = 10), expand = c(0,0)) +
  scale_x_continuous(limits = c(140, 170), breaks = seq(145, 165, by = 10),expand = c(0,0)) +
  
  # ggspatial::annotation_scale(location = "bl",
  #                             pad_x = unit(0.5, "cm"),
  #                             pad_y = unit(1.25, "cm"),
  #                             style = "ticks",
  #                             #plot_unit = "km",
  #                             width_hint = 0.15,
  #                             line_col = "white",
  #                             text_col = "white"
  # ) +
  
  # ggspatial::annotation_north_arrow(location = "bl",
  #                                   which_north = "true", 
  #                                   height = unit(0.6, "cm"),
  #                                   width = unit(0.6, "cm"),
  #                                   pad_x = unit(0.4, "cm"),
  #                                   pad_y = unit(0.75, "cm"),
  #                                   style =  north_arrow_fancy_orienteering(text_col = "white", text_size = 6)) +
  
  # annotation_custom(
  #   grob = ggplotGrob(AUS_map),  # Convert the inset map to a grob
  #   xmin = 141, xmax = 149, ymin = -33, ymax = -22  # Adjust position & size of inset map
  # ) +
  
  # annotation_custom(
  #   grob = ggplotGrob(world_map_globe),  # Convert the inset map to a grob
  #   xmin = 140, xmax = 150, ymin = -37, ymax = -20  # Adjust position & size of inset map
  # ) +
  # 
  # annotate("text", x = 145, y = -29.75, label = "AUS", color = "black", size = 1.75, fontface = "bold") +
  # annotate("text", x = 143, y = -22, label = "QLD", color = "black", size = sz, fontface = "bold") +
  # annotate("text", x = 145, y = -33, label = "NSW", color = "black", size = sz, fontface = "bold") +
  # annotate("text", x = 144, y = -6, label = "PNG", color = "black", size = sz, fontface = "bold") +
  # annotate("text", x = 165, y = -21, label = "NC", color = "black", size = sz, fontface = "bold") +
  # annotate("text", x = 158.5, y = -8, label = "SI", color = "black", size = sz, fontface = "bold") +
  # annotate("text", x = 166.75, y = -15.5, label = "VU", color = "black", size = sz, fontface = "bold") +
  annotate("text", x = 149, y = -13.5, label = "Coral Sea", color = "white", size = sz, fontface = "italic") +
  annotate("text", x = 153, y = -7.5, label = "Solomon\nSea", color = "white", size = sz, fontface = "italic")+
  annotate("text", x = 160, y = -35, label = "Tasman Sea", color = "white", size = sz, fontface = "italic")+
  annotate("text", x = 142.5, y = -10, label = "Torres Str.", color = "white", size = 1.25, fontface = "italic")+
  
  
  theme(
    legend.position = "none",
    # legend.position = "bottom",
    # legend.direction = "horizontal",
    # legend.key.width = rel(1.5),
    # legend.key.height = rel(0.75),
    # legend.title = element_text(size=8, face = "bold"),
    # legend.text = element_text(size =8),
    axis.text = element_text(size = 8),
    plot.title = element_text(size = 10),
    axis.title = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
    #panel.background = element_rect(fill = "black", colour = NA),
    panel.grid = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")) 


sci_current



sci_cc <- sci_current + P_sci_cc_126 + P_sci_cc_245 + P_sci_cc_585 +
  patchwork::plot_layout(guides = "collect", axes = "collect", axis_titles = "collect", ncol = 2) &
  ggplot2::theme(
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.key.width = rel(1.5),
    legend.key.height = rel(0.75),
    legend.title = element_text(size=8, face = "bold", vjust = 0.75, hjust = 0.5),
    legend.text = element_text(size =8),
  )


sci_cc

ggsave("SCI_cc_supps.png", plot = sci_cc, path ="Output_Figs", 
       scale = 1, 
       width = 18, 
       height = 20,
       units = "cm", dpi = 400)





# CC_ density plots -------------------------------------------------------
library(ggridges)

names(CC_monsoon_res)

# names(sci_monsoon_raster) <- "Current"
# names(CC_126_monsoon_sci) <- "ssp126"
# names(CC_245_monsoon_sci) <- "ssp245"
# names(CC_585_monsoon_sci) <- "ssp585"

# sci_stack <- c(
#   sci_monsoon_raster,
#   CC_126_monsoon_sci,
#   CC_245_monsoon_sci,
#   CC_585_monsoon_sci)

sci_stack

sci_monsoon_raster
CC_monsoon_res

# sci_df <- terra::as.data.frame(sci_stack, xy = TRUE, long = TRUE, na.rm = FALSE) |> 
#   tidyr::pivot_longer(
#     cols = names(sci_stack),
#     names_to = "lyr",
#     values_to = "SCI") |> 
#   dplyr::rename(scenario = lyr,
#                 lat = y,
#                 lon = x) |> 
#   dplyr::mutate(scenario = as.factor(scenario))

CC_monsoon_sci
sci_df <- terra::as.data.frame(CC_monsoon_sci, xy = TRUE, long = TRUE, na.rm = FALSE) |> 
  tidyr::pivot_longer(
    cols = names(CC_monsoon_sci),
    names_to = "lyr",
    values_to = "SCI") |> 
  dplyr::rename(scenario = lyr,
                lat = y,
                lon = x) |> 
  dplyr::mutate(scenario = as.factor(scenario))


head(sci_df)

sci_df <- sci_df |>
  dplyr::mutate(
    year = dplyr::case_when(
      stringr::str_detect(scenario, "Current")    ~ "Current",   # or "2100" if you prefer
      stringr::str_detect(scenario, "2050|2045_2055")  ~ "2050",
      stringr::str_detect(scenario, "2100|2090_2100")  ~ "2100",
      TRUE ~ NA_character_
    ),
    scenario = dplyr::case_when(
      stringr::str_detect(scenario, "Current") ~ "Current",
      stringr::str_detect(scenario, "ssp126")  ~ "SSP1-2.6",
      stringr::str_detect(scenario, "ssp245")  ~ "SSP2-4.5",
      stringr::str_detect(scenario, "ssp585")  ~ "SSP5-8.5",
      TRUE ~ NA_character_
    )
  )


str(sci_df)

levs <- unique(sci_df$scenario)
levs

pal <- stats::setNames(
  c(
    "grey70",          # current / baseline
    "#1b9e77",         # ssp126 (green/teal)
    "#d95f02",         # ssp245 (orange)
    "hotpink"          # ssp585 (purple)
  )[seq_along(levs)],
  levs
)


dens_sci <- sci_df |> dplyr::filter(SCI > 0.5 & year != "2050") |> 
  ggplot(aes(x = lat, y = scenario, fill = scenario)) +
  stat_density_ridges(na.rm = TRUE, alpha = 0.5, 
                      scale = 10, 
                      panel_scaling = FALSE,
                      linewidth = 0.1,
                      bandwidth = 1,
                      quantile_lines = TRUE, quantiles = c(0.025, 0.975)) +
  # coord_sf(crs = st_crs(4326), expand = FALSE) +
  scale_fill_manual(values = cmocean::cmocean("phase")(8)) +
  # ggplot2::scale_fill_manual(values = pal) +
  coord_flip() +
  labs(x = "", y = "", title = "") +
  # facet_wrap(~year) +
  #geom_vline(xintercept = -12.132504, linetype = "dashed", color = "red") +
  #annotate("segment", y = -Inf, yend = Inf, x = -12.132504, xend = -12.132504, linetype = "dashed", color = "red") +
  scale_y_discrete(position = "left") +  # Ensure y-axis labels appear correctly
  scale_x_continuous(limits = c(-36, 2), breaks = seq(-35, -5, 10)) + 
  theme_minimal() +
  theme(
    legend.position = "none",
    # axis.text.x.top = element_blank(),  # Set axis title x top size
    # axis.text.x.top = element_text(size = 7, margin = margin(b = -15), angle = 90),    # Set axis text x top size
    # axis.text.x.bottom = element_text(size = 7, margin = margin(b = 0), angle = 90),
     # axis.text.y.bottom = element_text(size = 7, angle = 90),
    axis.text.x = element_text(size = 7, angle = 90, margin = margin(b = 10)),
    legend.title = element_text(face = "bold", size = 8),
    legend.text = element_text(size = 7),
    panel.grid.major.y = element_line(),
    panel.grid.major.x = element_line(),
    panel.grid.minor.y = element_line(),
    panel.grid.minor.x = element_line(),
    axis.title.x = element_blank(),  # Hide original x-axis title
    # axis.text.x = element_blank(),    # Hide original x-axis text
    axis.text.y = element_text(size = 7, margin = margin(l=0) ),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
  )

dens_sci


sci_plot <- sci_df |>
  dplyr::mutate(
    # Assign Current to a year block (choose "2050" or "2100")
    year = dplyr::if_else(scenario == "Current", "2050", year)
  ) |>
  dplyr::filter(!(scenario == "Current" & year == "2100")) |>  # keeps Current only once
  dplyr::mutate(
    year     = factor(year, levels = c("2050", "2100")),
    scenario = factor(scenario, levels = c("Current", "SSP1-2.6", "SSP2-4.5", "SSP5-8.5")),
    xgroup   = interaction(year, scenario, sep = "___", lex.order = TRUE)
  )
unique(sci_plot$year)

# 2) Order x levels so all 2050 first, then 2100
x_levels <- sci_plot |>
  dplyr::distinct(year, scenario, xgroup) |>
  dplyr::arrange(year, scenario) |>
  dplyr::pull(xgroup)

sci_plot <- sci_plot |>
  dplyr::mutate(xgroup = factor(xgroup, levels = x_levels))

sci_plot



xlev <- levels(sci_plot$xgroup)

pal_xgroup <- setNames(rep(NA_character_, length(xlev)), xlev)


# Identify blocks
cur_lev   <- xlev[grepl("___Current$", xlev)]
lev_2050  <- xlev[grepl("^2050___SSP", xlev)]
lev_2100  <- xlev[grepl("^2100___SSP", xlev)]

# Define spacer level names (must not clash with existing)
sp1 <- "gap_1"
sp2 <- "gap_2"

# Relevel with gaps inserted
new_levels <- c(cur_lev, sp1, lev_2050, sp2, lev_2100)

sci_plot <- sci_plot |>
  dplyr::mutate(
    xgroup = factor(as.character(xgroup), levels = new_levels)
  )

lab_fun <- function(x) {
  x <- stringr::str_replace(x, "^(2050|2100)___", "")
  x[x %in% c("gap_1", "gap_2")] <- ""
  x
}



# Current
pal_xgroup[grepl("___Current$", xlev)] <- "grey70"



# 2050 SSPs -> blues
blue3 <- c("#c6dbef", "#6baed6", "#2171b5")
pal_xgroup[grepl("^2050___SSP", xlev)] <- blue3[seq_len(sum(grepl("^2050___SSP", xlev)))]

# 2100 SSPs -> reds
red3  <- c("#fcbba1", "#fb6a4a", "#cb181d")
pal_xgroup[grepl("^2100___SSP", xlev)] <- red3[seq_len(sum(grepl("^2100___SSP", xlev)))]



pal_xgroup <- pal_xgroup
pal_xgroup[c("gap_1", "gap_2")] <- NA  # not used (no data), but keeps scale happy



dens_sci <- sci_plot |>
  dplyr::filter(SCI > 0.5) |>
  ggplot2::ggplot(ggplot2::aes(
    x     = lat,
    y     = xgroup,        # discrete axis for ridges
    fill  = xgroup,
    group = xgroup
  )) +
  
  
  # add custom grid
  ggplot2::geom_hline(
    yintercept = c(1, 3, 4, 5, 7, 8, 9),
    linewidth = 0.2,
    colour = "grey50"
  ) +
  ggplot2::geom_vline(
    xintercept = seq(-40, 0, by = 5),
    linewidth = 0.2,
    colour = "grey50"
  ) +
  
  
  ggridges::geom_density_ridges2(
    na.rm = TRUE,
    alpha = 0.5,
    scale = 10,
    panel_scaling = FALSE,
    linewidth = NA,
    bandwidth = 1,
    quantile_lines = FALSE, quantiles = c(0.025, 0.975)
  ) +
  ggplot2::scale_fill_manual(values = pal_xgroup) +
  ggplot2::scale_y_discrete(
    labels = lab_fun, drop = FALSE
    # labels = function(x) stringr::str_replace(x, "^(2050|2100)___", "")  # scenario only
  ) +
  ggplot2::scale_x_continuous(
    limits = c(-40, 0),
    breaks = seq(-35, -5, 10),
    expand = c(0, 0)
  ) +
  # ggplot2::coord_cartesian(xlim = c(-50, 2)) +
  ggplot2::coord_flip(clip = "off") + 
  ggplot2::annotate("text", x = -40, y = 4, label = "2050\n(2045-2055)", vjust = 3.6, size = 2, lineheight = 1) +
  ggplot2::annotate("text", x = -40, y = 8, label = "2100\n(2090-2100)", vjust = 3.6, size = 2, lineheight = 1) +
  ggplot2::annotate("text", x = -40, y = 1, label = "2019-\n2025", vjust = 3.6, size = 2, lineheight = 1) +
  
  

  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none",
    axis.title = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = 5, angle = 90, vjust = 0.5, hjust = -0.1, color = "black"),
    # axis.text.y = ggplot2::element_text(size = 5, color = "black"),
    # panel.grid.major.y  = ggplot2::element_line(color= "black", linewidth = 0.1),
    # panel.grid.minor.y = ggplot2::element_line(color = "black", linewidth = 0.1),
    # panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank(),
    plot.margin = ggplot2::margin(t = 0, r = 0, b = 25, l = 10)
  )

dens_sci



ggsave("SCI_dense_CC.png", plot = dens_sci, path ="Output_Figs", 
       scale = 1, 
       width = 8, 
       height = 15,
       units = "cm", dpi = 400)


ggsave("SCI_dense_CC.pdf", plot = dens_sci, path ="Output_Figs", 
       scale = 1, 
       width = 8, 
       height = 15,
       units = "cm", dpi = 400,
       device = "pdf")

ggsave("SCI_dense_CC.tif", plot = dens_sci, path ="Output_Figs", 
       scale = 1, 
       width = 8, 
       height = 15,
       units = "cm", dpi = 400)





sci_df
# Wide format PER year
sci_wide <- sci_df |>
  dplyr::filter(year %in% c("2050", "2100")) |>
  dplyr::select(lon, lat, year, scenario, SCI) |>
  tidyr::pivot_wider(
    id_cols     = c(lon, lat, year),
    names_from  = scenario,
    values_from = SCI
  )

names(sci_wide)


current_df <- sci_df |>
  dplyr::filter(scenario == "Current") |>
  dplyr::select(lon, lat, Current = SCI)


sci_wide2 <- sci_wide |>
  dplyr::left_join(current_df, by = c("lon", "lat"))

sci_wide2


sci_delta <- sci_wide2 |>
  dplyr::mutate(
    `SSP1-2.6` = `SSP1-2.6` - Current,
    `SSP2-4.5` = `SSP2-4.5` - Current,
    `SSP5-8.5` = `SSP5-8.5` - Current
  ) |>
  dplyr::select(lon, lat, year, `SSP1-2.6`, `SSP2-4.5`, `SSP5-8.5`) |>
  tidyr::pivot_longer(
    cols      = c(`SSP1-2.6`, `SSP2-4.5`, `SSP5-8.5`),
    names_to  = "scenario",
    values_to = "dSCI"
  ) |>
  dplyr::mutate(
    scenario = factor(scenario, levels = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5")),
    year     = factor(year, levels = c("2050", "2100"))
  )

sci_delta



na_col2 <- cmocean::cmocean("balance")(100)[[50]]

P_delta_sci <- ggplot2::ggplot(sci_delta, ggplot2::aes(lon, lat, fill = dSCI)) +
  ggplot2::geom_raster() +
  
  geom_sf(data = world$geometry, fill = "grey20", color = "black", linewidth = 0.15, inherit.aes = FALSE) +
  
  
  geom_sf(data = aus_states,
          fill   = NA,
          colour = "black",
          linewidth = 0.1,
          inherit.aes = FALSE) +
  
  coord_sf(xlim = c(140, 170), ylim = c(-40, 0), expand = FALSE) + 
  
  ggplot2::facet_grid(year ~ scenario) +
  ggplot2::scale_fill_gradientn(
    colors = cmocean::cmocean(name = "balance", start = 0, end = 1)(100),
    limits = c(-0.5, 0.5),
    breaks = c(-0.5, 0, 0.5),
    # labels = c("Low", "High"),
    guide = guide_colorbar(frame.colour = "black", ticks.colour = "black"),
    na.value = na_col2,
    name = expression(Delta*"SCI")
  ) +
  
  scale_y_continuous(limits = c(-40, -0), breaks = seq(-35, -5, by = 10), expand = c(0,0)) +
  scale_x_continuous(limits = c(140, 170), breaks = seq(145, 165, by = 10),expand = c(0,0)) +
  
  ggplot2::theme_bw() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.width = rel(1.5),
    legend.key.height = rel(0.75),
    legend.title = element_text(size=8, face = "bold", vjust = 0.75),
    legend.text = element_text(size =8),
    axis.text = element_text(size = 8),
    plot.title = element_text(size = 10),
    axis.title = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
    #panel.background = element_rect(fill = "black", colour = NA),
    panel.grid = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")) 


P_delta_sci


ggsave("SCI_delta.png", plot = P_delta_sci, path ="Output_Figs", 
       scale = 1, 
       width = 18, 
       height = 19,
       units = "cm", dpi = 400)


ggsave("SCI_delta.pdf", plot = P_delta_sci, path ="Output_Figs", 
       scale = 1, 
       width = 18, 
       height = 19,
       units = "cm", dpi = 400,
       device = "pdf")

ggsave("SCI_delta.tif", plot = P_delta_sci, path ="Output_Figs", 
       scale = 1, 
       width = 18, 
       height = 19,
       units = "cm", dpi = 400)




P_sci_cc_585_b <- ggplot2::ggplot() +
  
  tidyterra::geom_spatraster(data = CC_monsoon_sci$ssp585_2090_2100, na.rm = TRUE) +
  
  geom_sf(data = world$geometry, fill = "grey20", color = "black", linewidth = 0.15) +
  
  
  geom_sf(data = aus_states,
          fill   = NA,
          colour = "black",
          linewidth = 0.1) +
  
  coord_sf(xlim = c(140, 170), ylim = c(-40, 0), expand = FALSE) + 
  
  
  geom_sf(data = cities3,
          shape = 21,
          colour = "black", 
          fill = "yellow", 
          alpha = 0.5, 
          size = 1,
          show.legend = FALSE) +
  
  coord_sf(xlim = c(140, 170), ylim = c(-40, 0), expand = FALSE) + 
  
  
  ggsflabel::geom_sf_text_repel(data = cities3,
                                colour = "white", 
                                aes(label = Loc), 
                                nudge_x = -1, 
                                nudge_y = 0, 
                                size = 1.75, 
                                # force = 10,
                                # force_pull = 10,
                                segment.colour = "black",  
                                segment.size   = 0.15,       
                                segment.linetype = "solid",
                                seed = 10) +
  
  
  geom_sf(data = cities2,
          shape = 21,
          colour = "black", 
          fill = "yellow", 
          alpha = 0.5, 
          size = 1,
          show.legend = FALSE) +
  
  
  ggsflabel::geom_sf_text_repel(data = cities2,
                                colour = "white", 
                                aes(label = Loc), 
                                nudge_x = 0.5, 
                                nudge_y = 0, 
                                size = 1.75, 
                                force = 1,
                                force_pull = 10,
                                segment.colour = "black",  
                                segment.size   = 0.15,       
                                segment.linetype = "solid",
                                seed = 12) +
  
  
  
  scale_fill_gradientn(
    colours = cmocean::cmocean(name = "ice", direction = 1, start = 0, end = 1)(300),
    limits  = c(0, high),
    breaks  = c(0, high),
    labels = c("low", "high"),
    na.value = na_col
  ) +
  
  labs(fill = "SCI", x = "Longitude", y = "Latitude", title = "SSP5-8.5") +
  
  scale_y_continuous(limits = c(-40, -0), breaks = seq(-35, -5, by = 10), expand = c(0,0)) +
  scale_x_continuous(limits = c(140, 170), breaks = seq(145, 165, by = 10),expand = c(0,0)) +
  
  
  theme(
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.key.width = rel(1.5),
    legend.key.height = rel(0.5),
    legend.title = element_text(size=6, face = "bold", vjust = 0.75, hjust = 0.5),
    legend.text = element_text(size =6),
    axis.text = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title = element_blank(),
    plot.title = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.5),
    panel.grid = element_blank(),
    plot.margin = unit(c(0.1, 0, 0, 0), "cm")) 


P_sci_cc_585_b








ggsave("SCI_585_2100.png", plot = P_sci_cc_585_b, path ="Output_Figs", 
       scale = 1, 
       width = 10, 
       height = 15,
       units = "cm", dpi = 400)


ggsave("SCI_585_2100.pdf", plot = P_sci_cc_585_b, path ="Output_Figs", 
       scale = 1, 
       width = 10, 
       height = 15,
       units = "cm", dpi = 400,
       device = "pdf")

ggsave("SCI_585_2100.tif", plot = P_sci_cc_585_b, path ="Output_Figs", 
       scale = 1, 
       width = 10, 
       height = 15,
       units = "cm", dpi = 400)




