library(sf)
library(raster); library(dplyr); library(ggplot2); library(scales)
library(terra)
# set working directory to where the fire data is saved 
setwd("C:/Users/marin/Documents/1. Liu Lab/Practice_data_folder/cambodia_field_sites")
savanna_plots <- st_read("savanna_plots.gpkg")
forest_plots <- st_read("forest_plots.gpkg")
kr_2024_rgb <- rast("kr_2024_rgb.tif")
# using base terra to plot the data 
plotRGB(kr_2024_rgb, r=1, g=2, b=3, stretch="lin")
plot(st_geometry(savanna_plots), add=TRUE, col="orange", pch=16)
plot(st_geometry(forest_plots), add=TRUE, col="darkgreen", pch=16)


## Prep the data to plot it in ggplot
# define a single, shared CRS object
target_crs <- st_crs(32648) # UTM Zone 48

# force both datasets to use this exact object
savanna_plots <- st_transform(savanna_plots, target_crs)
forest_plots <- st_transform(forest_plots, target_crs)

# Align the raster to the plots
all_plots <- rbind(savanna_plots, forest_plots) # Use original to avoid double-transforming
st_crs(all_plots) <- "EPSG:32648" 

# Force the raster to match the plot extent (Georeferencing)
plot_extent <- ext(all_plots) + 500
ext(kr_2024_rgb) <- plot_extent
crs(kr_2024_rgb) <- st_crs(all_plots)$wkt

# Save the .tif to georeferenced file
# This saves your fixed file so you don't have to georeference it next time
writeRaster(kr_2024_rgb, "kr_2024_rgb_georeferenced.tif", overwrite = TRUE)

# align the areas to the crop region
# Crop to the actual area of interest to keep the data frame small
kr_cropped <- crop(kr_2024_rgb, plot_extent)
kr_rescaled <- stretch(kr_cropped, minv = 0, maxv = 1)

# Convert to data frame for ggplot
kr_df <- as.data.frame(kr_rescaled, xy = TRUE, na.rm = TRUE)
colnames(kr_df)[3:5] <- c("R", "G", "B")

# ggplot
ggplot() +
  geom_raster(data = kr_df, aes(x = x, y = y, fill = rgb(R, G, B))) +
  scale_fill_identity() +
  geom_sf(data = savanna_plots, fill = "orange", shape = 21, size = 3, color = "white") +
  geom_sf(data = forest_plots, fill = "darkgreen", shape = 21, size = 3, color = "white") +
  theme_minimal() +
  labs(title = "Field Sites: Cambodia (2024)",
       subtitle = "Basemap georeferenced to UTM Zone 48N")

ggsave("Cambodia_Field_Sites_2024.png", 
       width = 10, 
       height = 8, 
       units = "in", 
       dpi = 300)


## try to add a base layer for a greater region

library(ggspatial)

# Create the regional map
ggplot() +
  # 1. Add a regional basemap (zooms out automatically)
  #annotation_map_tile(type = "osm", zoom = 10) + 
  
  # 2. Add your high-res raster (it will appear as a small patch)
  geom_raster(data = kr_df, aes(x = x, y = y, fill = rgb(R, G, B))) +
  scale_fill_identity() +
  
  # 3. Add your points
  geom_sf(data = savanna_plots, fill = "orange", shape = 21, size = 2) +
  geom_sf(data = forest_plots_fixed, fill = "darkgreen", shape = 21, size = 2) +
  # 4. Set the view to a larger area (e.g., Cambodia)
  # Adjust these coordinates to zoom out
  coord_sf(xlim = c(min(kr_df$x) - 100, max(kr_df$x) + 100),
                    ylim = c(min(kr_df$y) - 100, max(kr_df$y) + 100),
                    crs = st_crs(32648)) + 
  labs(title = "Field Sites: Cambodia (2024)",
       subtitle = "Basemap georeferenced to UTM Zone 48N")+
  theme_minimal()

ggsave("Cambodia_Field_Sites_2024_v2.png", 
       width = 10, 
       height = 8, 
       units = "in", 
       dpi = 300)

library(rnaturalearth)
library(ggspatial)

# 1. Get the Cambodia border
cambodia <- ne_countries(scale = "medium", country = "cambodia", returnclass = "sf") %>%
  st_transform(32648)

# 2. Plot with a Minimalist Type
ggplot() +
  # Use a light, minimal basemap style
  annotation_map_tile(type = "cartolight", zoom = 7) + 
  
  # Draw the country outline
  geom_sf(data = cambodia, fill = NA, color = "black", size = 0.5) +
  
  # Add your sites (scaled up slightly so they are visible)
  geom_sf(data = savanna_plots, fill = "orange", shape = 21, size = 3, color = "white") +
  geom_sf(data = forest_plots_fixed, fill = "darkgreen", shape = 21, size = 3, color = "white") +
  
  # Minimal Compass and Scale
  annotation_north_arrow(location = "br", which_north = "true", 
                         style = north_arrow_minimal()) +
  annotation_scale(location = "bl", style = "ticks") +
  
  # Clean up the theme
  theme_void() +
  theme(panel.background = element_rect(fill = "white", color = NA)) +
  labs(title = "Project Sites: Cambodia")
ggsave("Cambodia-field-site.png", 
       width = 10, 
       height = 8, 
       units = "in", 
       dpi = 300)

provinces <- ne_states(country = "cambodia", returnclass = "sf") %>%
  st_transform(32648)

siem_reap <- provinces %>% filter(name == "Siemreab")
s_bbox <- st_bbox(siem_reap)

site_plot <- ggplot() +
  # 1. Background Tiles (Minimal Carto Light)
  annotation_map_tile(type = "cartolight", zoom = 9) + 
  
  # 2. Province Boundaries (All and specific Siem Reap)
  geom_sf(data = provinces, fill = NA, color = "gray80", size = 0.2) +
  geom_sf(data = siem_reap, fill = NA, color = "black", size = 0.8) +
  
  # 3. Field Sites with LEGEND mapping
  # Note: fill is moved inside aes() to create the legend
  geom_sf(data = savanna_plots, aes(fill = "Savanna"), shape = 21, size = 4, color = "white") +
  geom_sf(data = forest_plots_fixed, aes(fill = "Forest"), shape = 21, size = 4, color = "white") +
  
  # 4. Define Legend Colors and Title
  scale_fill_manual(values = c("Savanna" = "orange", "Forest" = "darkgreen"), 
                    name = "Plot Type") +
  
  # 5. Minimalist Compass and Scale
  annotation_north_arrow(location = "tr", style = north_arrow_minimal()) +
  annotation_scale(location = "bl", style = "ticks") +
  
  # 6. FIXED ZOOM: Force the view to the bounding box
  # 'expand = FALSE' is critical to stop R from adding extra space around the edges
  coord_sf(xlim = c(s_bbox["xmin"], s_bbox["xmax"]), 
           ylim = c(s_bbox["ymin"], s_bbox["ymax"]),
           crs = st_crs(32648), # Explicitly tell R we are using UTM Meters
           expand = FALSE) +
  
  theme_void() + 
  theme(legend.position = "right", 
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(title = "Field Sites: Siem Reap Province, Cambodia")

site_plot