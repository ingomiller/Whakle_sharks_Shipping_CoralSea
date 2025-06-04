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


ships <- terra::rast( "Shipping_MeanVessels_AUS_2018-2025.tif")



mean_SDM <- terra::rast("/Volumes/Ingo_PhD/PhD_Data_Analysis/PhD_WhaleSharks_SDMs_Enviro_Layers/Chapter2/Processed_Raster_Files/SDM_Outputs_Final/WhaleSharks_SDM_Mean_Enssemble_Current.tif")
seasons_SDM <- terra::rast( "/Volumes/Ingo_PhD/PhD_Data_Analysis/PhD_WhaleSharks_SDMs_Enviro_Layers/Chapter2/Processed_Raster_Files/SDM_Outputs_Final/WhaleSharks_SDM_Mean_Season_Enssemble_Current.tif")


mean_SDM
seasons_SDM

terra::plot(ships)
# Optional: align extents/resolution
ships_crop <- terra::resample(ships, mean_SDM)
ships_crop
terra::plot(ships_crop)

# Set CZU (collision zone use value)
czu_value <- 0.55 # as per Womersley et al. 2022 | I think this is not needed when using habitat suitability

# Compute SCI
sci_raster <- mean_SDM * ships_crop #* czu_value
sci_raster

terra::plot(sci_raster)

# Extract monsoon layer
sdm_monsoon <- seasons_SDM$Monsoon_Season_Ensemble
plot(sdm_monsoon)

# Compute SCI fro monsoon seasosn 
sci_monsoon_raster <- sdm_monsoon * ships_crop #* czu_value
sci_monsoon_raster

terra::plot(sci_monsoon_raster, range = c(10, 90))

# Save
writeRaster(sci_raster, "SCI_mean_current_climate.tif", overwrite = TRUE)
writeRaster(sci_monsoon_raster, "SCI_monsson_current_climate.tif", overwrite = TRUE)


# Compute mean SCI value across all non-NA cells
mean_sci <- terra::global(sci_raster, fun = "mean", na.rm = TRUE)
mean_sci

mean_monsson_sci <- terra::global(sci_monsoon_raster, fun = "mean", na.rm = TRUE)
mean_monsson_sci



### SCI in high suitability areas

# Mask: keep only cells with suitability > 0.5
# Mask to show only cells > 0.5
high_suitability <- classify(sdm_monsoon, rcl = matrix(c(-Inf, 0.5, NA), ncol = 3, byrow = TRUE))

# Plot high suitability areas only
plot(high_suitability, main = "High Suitability Areas (Suitability > 0.5)", col = "darkgreen", legend = FALSE)

plot(high_suitability)
# Compute SCI only in high-suitability areas
sci_high <- high_suitability * ships_crop
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




library(terra)
library(rasterVis)

AUS_region <- c(xmin=135, xmax = 160, ymin = -30, ymax = -5)
max_values <- terra::app(sci_monsoon_raster, fun = "max", na.rm = TRUE)
high <- global(max_values, fun = "max", na.rm = TRUE) |> as.numeric()
min_values <- terra::app(sci_monsoon_raster, fun = "min", na.rm = TRUE)
low <- global(max_values, fun = "min", na.rm = TRUE) |> as.numeric()
high = 0.8


cities <- data.frame(Loc = c("Cooktown", "Cairns",  "Mackay", "Brisbane"),
                     Group = c("Town", "Town",  "Town", "Town"),
                     # Season = c("Monsoon Season (Nov - Apr)"),
                     # lyr = 1,
                     lat = c(-15.4758, -16.918246, -21.1434, -27.4705),
                     lon = c(145.2471, 145.771359, 149.1868, 153.026),
                     lyr = "Monsoon Season (Nov - Apr)") |>   # This must match the facet name) |>
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
plot(AUS_PNG)

# Define the extent of the study area 
ext <- c(xmin = 136, xmax = 160, ymin = -30, ymax = -5)

# Create the inset map of Australia + Papua New Guinea and add the study area rectangle
AUS_map <- ggplot() +
  geom_sf(data = AUS_PNG, fill = "lightgray", colour = NA, size = 0.1) +
  coord_sf(xlim = c(112, 160), ylim = c(-45, -4), expand = FALSE) +
  geom_rect(aes(xmin = ext["xmin"], xmax = ext["xmax"], ymin = ext["ymin"], ymax = ext["ymax"]), 
            fill = NA, color = "black", 
            linetype = "solid", size = 0.5) +  # Study area rectangle
  theme_void() +  
  theme(panel.border = element_blank(),
        panel.background = element_blank())  

AUS_map


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
  
  #tidyterra::geom_spatraster(data = sci_monsoon_raster) +
  tidyterra::geom_spatraster(data = sci_high, na.rm = TRUE) +
  
  geom_sf(data = AUS_PNG$geometry, fill = "grey50", color = "grey50", linewidth = 0.2) +
  
  geom_sf(data = cities,
          shape = 21,
          colour = "black", 
          fill = "yellow", 
          alpha = 0.5, 
          size = 1.5,
          show.legend = FALSE) +
  coord_sf(xlim = c(136, 160), ylim = c(-30, -5), expand = FALSE) + 
  
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
  coord_sf(xlim = c(136, 160), ylim = c(-30, -5), expand = FALSE) + 
  
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
    limits = c(0, high),
    breaks = c(0, high),
    labels = c("Low", "High"),
    #labels = scales::comma,
    na.value = "grey10",
    guide = guide_colorbar(frame.colour = "black", ticks.colour = NA)
  ) +
  
  labs(fill = "SCI", x = "Longitude", y = "Latitude", title = "") +
  
  
  
  
  scale_y_continuous(limits = c(-30, -4.95), breaks = seq(-25, -10, by = 5), expand = c(0,0)) +
  scale_x_continuous(limits = c(136, 160), breaks = seq(140, 155, by = 5),expand = c(0,0)) +
  
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


P1
 


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



max_values <- terra::app(sci_high, fun = "max", na.rm = TRUE)
high <- global(max_values, fun = "max", na.rm = TRUE) |> as.numeric()
min_values <- terra::app(sci_high, fun = "min", na.rm = TRUE)
low <- global(min_values, fun = "min", na.rm = TRUE) |> as.numeric()
high = 0.8



P2 <- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = sci_monsoon_raster) +
  #tidyterra::geom_spatraster(data = sci_high, na.rm = TRUE) +
  # geom_sf(data = GBR_zone_shifted, fill = NA, color = "green", alpha = 1, linewidth = 0.5, linetype = "dashed") +
  geom_sf(data = AUS_PNG$geometry, fill = "grey50", color = "black", linewidth = 0.5) +
  
  geom_sf(data = WB_95,
          aes(linetype = factor("Home Range", levels = c("Home Range", "95% CI"))),
          fill = NA, color = "blue1", alpha = 0.5, linewidth = 0.5, show.legend = TRUE) +

  geom_sf(data = WB_95_high,
          aes(linetype = factor("95% CI", levels = c("Home Range", "95% CI"))),
          fill = NA, color = "blue1", alpha = 1, linewidth = 0.5, show.legend = TRUE) +

  geomtextpath::geom_textpath(
    data = WB_95_high_df_latlon,
    aes(x = x, y = y, label = label, group = L1),
    size = 2,
    color = "blue1",
    linewidth = 0.5,
    linetype = "blank",
    vjust = 0,
    text_smoothing = 1
  ) +

  
  geom_sf(data = GBR_features |> dplyr::filter(FEAT_NAME == "Reef"), fill = NA, color = "grey50", alpha = 1, linewidth = 0.1) +

  geom_sf(data = GBR_zone, fill = NA, color = "black", alpha = 1, linewidth = 0.5, linetype = "longdash") +
  
  
  geom_sf(data = cities,
          shape = 21,
          colour = "black", 
          fill = "yellow", 
          alpha = 1, 
          size = 2,
          show.legend = FALSE) +
  
  coord_sf(xlim = c(142, 147), ylim = c(-20, -10), expand = FALSE) + 
  
  ggsflabel::geom_sf_text_repel(data = cities,
                                colour = "black", 
                                aes(label = Loc), 
                                nudge_x = -0.75, 
                                nudge_y = 0.25, 
                                size = 3, 
                                force = 1,
                                force_pull = 10,
                                seed = 10) +

  
  ggsflabel::geom_sf_text_repel(data = WB,
                                colour = "black",
                                aes(label = Loc),
                                nudge_x = 0.8,
                                nudge_y = -0.6,
                                size = 2.5,
                                #fontface = "bold",
                                force = 1,
                                force_pull = 10,
                                seed = 15) +
  
  scale_fill_gradientn(
    #colors = viridisLite::plasma(256),   # or use the manual one above
    trans = 'log10',
    colors = cmocean::cmocean(name = "matter", start = 0.1, end = 0.9)(300),
    #colors = map_effort_dark,
    limits = c(0, high),
    breaks = c(0, high),
    labels = c("Low", "High"),
    na.value = NA,
    guide = guide_colorbar(frame.colour = "black", ticks.colour = NA)
  ) +
  
  labs(fill = "SCI", x = "Longitude", y = "Latitude", title = "") +
  
  scale_linetype_manual(
    name = "Whale Shark\nAggregation\nSpace Use",
    values = c("Home Range" = "solid", "95% CI" = "dashed"),
    guide = guide_legend(order = 2)
  ) +
  
  scale_y_continuous(limits = c(-20, -10), breaks = seq(-19, -11, by = 2), expand = c(0,0)) +
  scale_x_continuous(limits = c(142, 147), breaks = seq(143, 146, by = 2),expand = c(0,0)) +
  
  ggspatial::annotation_scale(location = "bl",
                              pad_x = unit(2, "cm"),
                              pad_y = unit(0.25, "cm"),
                              style = "ticks",
                              #plot_unit = "km",
                              width_hint = 0.3
                              ) +
  
  ggspatial::annotation_north_arrow(location = "bl",
                                    which_north = "true", 
                                    height = unit(1, "cm"),
                                    width = unit(1, "cm"),
                                    pad_x = unit(0.5, "cm"),
                                    pad_y = unit(0.25, "cm"),
                                    style =  north_arrow_fancy_orienteering) +
  
  guides(alpha = "none",
         linetype = guide_legend(
           override.aes = list(
             fill = c(NA, NA),
             color = c("blue1", "blue1")
           )
         ),
         fill = guide_colorbar(order = 1)) +
  
  
  # Add manual text at specific coordinates
  # annotate("text", x = 141, y = -26, label = "AUS", color = "white", size = 3, fontface = "bold") +
  # annotate("text", x = 144, y = -6, label = "PNG", color = "white", size = 3, fontface = "bold") +
  # annotate("text", x = 152, y = -15, label = "Coral Sea", color = "white", size = 3, fontface = "italic") +
  # annotate("text", x = 154, y = -8.5, label = "Solomon\nSea", color = "white", size = 3, fontface = "italic")+
  # annotate("text", x = 139, y = -14, label = "Gulf of\nCarpentaria", color = "white", size = 3, fontface = "italic")+
  # annotate("text", x = 145, y = -9, label = "Gulf of\nPapua", color = "white", size = 3, fontface = "italic")+
  # annotate("text", x = 142.5, y = -10, label = "Torres Str.", color = "white", size = 2, fontface = "italic")+ 
  
  
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    legend.key.width = rel(0.75),
    legend.key.height = rel(1),
    legend.title = element_text(size=8, face = "bold"),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 8),
    #axis.title = element_text(size = 10),
    axis.title = element_blank(),
    panel.border = element_rect(fill = NA, colour = "black", linewidth = 1),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")) 


P2

ggsave("SCI_FN_GBR.png", plot = P2, path ="Output_Figs", scale =1, width = 10, height = 15, units = "cm", dpi = 300)

