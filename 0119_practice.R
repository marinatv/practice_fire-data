
## Practice fire data 
#working with data from FIRMS and GFED4 to practice applying the spatial 
# data analysis techniques

library(sf)
library(raster); library(dplyr); library(ggplot2); library(scales)
# set working directory to where the fire data is saved 
setwd("C:/Users/marin/Documents/1. Liu Lab/Practice_data_folder")
firms_modis_25 <- st_read("FIRMS_2026-01-19/fire_nrt_M-C61_706801.shp")
viirs_25 <- st_read("FIRMS_2026-01-19/fire_archive_SV-C2_706804.shp")

# Point to a subfolder
file_list <- list.files(path = "C:/Users/marin/Documents/1. Liu Lab/Practice_data_folder/FIRMS_2026-01-19", 
                        pattern = "\\.shp$", 
                        full.names = TRUE)

# check the column names for all the related files
# lapply(file_list, function(x) names(st_read(x, quiet = TRUE)))

# Define the exact columns you want to keep
keep_cols <- c("ACQ_DATE", "FRP", "SATELLITE", "INSTRUMENT", "GEOMETRY")

# read and Compile
# all_fires <- do.call(rbind, lapply(file_list, function(x) {
#  temp_sf <- st_read(x, quiet = TRUE)
  # Ensure column names match your 'keep_cols' casing
  # NASA data can vary, so forcing to uppercase is a safe bet
#  names(temp_sf) <- toupper(names(temp_sf))
#  st_geometry(temp_sf) <- "GEOMETRY"
  # Select only the specific columns
# return(temp_sf[, keep_cols])
#}))
#check the files combined correctly 
#summary(all_fires)

# create a date object 
all_fires$ACQ_DATE <- as.Date(all_fires$ACQ_DATE)

# create a year column 
all_fires$YEAR <- as.numeric(format(all_fires$ACQ_DATE, "%Y"))

fire_counts <- as.data.frame(table(all_fires$YEAR))
colnames(fire_counts) <- c("Year", "Count")

# Save as a compressed R object
saveRDS(all_fires, "C:/Users/marin/Documents/1. Liu Lab/Practice_data_folder/all_fires_compiled.rds")

# To load the dataset for next session
all_fires <- readRDS("C:/Users/marin/Documents/1. Liu Lab/Practice_data_folder/all_fires_compiled.rds")

# line plot of fire frequency with all the data 
ggplot(fire_counts, aes(x = as.numeric(as.character(Year)), y = Count)) +
  geom_line(color = "red", size = 1) +
  geom_point() +
  labs(title = "Annual Fire Detections (2001 - 2025)",
       x = "Year",
       y = "Total Number of Detections") +
  theme_minimal()

## Given that the line plot above does not inform much
# create a plot that separates the VIIRS and MODIS satellites
# Group by Year and Satellite to get counts
comparison_counts <- all_fires %>%
  st_drop_geometry() %>% # remove spatial data 
  group_by(YEAR, INSTRUMENT) %>%
  summarize(Fire_Count = n(), .groups = 'drop')

# check the variable 
head(comparison_counts)

# create the plot to compare the two instruments
f1 <- ggplot(comparison_counts, aes(x = YEAR, y = Fire_Count, color = INSTRUMENT)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_log10(labels = label_comma()) + # log scale 
  scale_color_manual(values = c("MODIS" = "#E41A1C", "VIIRS" = "#377EB8")) +
  xlim(2001, 2025) + # removes the outlier 2026 data
  theme_minimal() +
  labs(
    title = "Fire detection comparison: MODIS vs. VIIRS (2001-2026)",
    subtitle = "VIIRS has a higher resolution to detect smaller fires",
    x = "Year",
    y = "Number of detections (Log Scale)",
    color = "Instrument sensor"
  ) +
  theme(legend.position = "bottom")
# view the plot
f1

## Create a plot to show a map of hotspots

# focus on only 4 years for a clean plot
target_years <- c(2005, 2010, 2020, 2025)

comparison_data <- all_fires[all_fires$YEAR %in% 
                               target_years & 
                               all_fires$INSTRUMENT == "MODIS", ]
st_geometry(comparison_data) <- "GEOMETRY"
# transform to GPS coordinates
comparison_data <- st_transform(comparison_data, 4326) 
# reverify the spatial data
comparison_data <- st_as_sf(comparison_data)

f2 <- ggplot(comparison_data) +
  geom_sf(aes(color = FRP), size = 0.5, alpha = 0.5) +
  scale_color_viridis_c(option = "inferno", trans = "log10") +
  facet_wrap(~YEAR) +
  theme_minimal() +
  labs(title = "Comparison across years (MODIS Only)",
       color = "Fire Intensity\n(Log FRP)")
f2

## Adding baselayer for the Thailand provinces
thailand_provinces <- st_read("C:/Users/marin/Documents/1. Liu Lab/Practice_data_folder/Thailand_provinces/tha_admbnda_adm1_rtsd_20190221.shp")

# ensure it is in the standard GPS format (WGS84)
thailand_provinces <- st_transform(thailand_provinces, 4326)

#filter for only one year to look at it closely
fire_2025 <- all_fires[all_fires$YEAR == 2025 & all_fires$INSTRUMENT == "VIIRS", ]

# check both layers are in the same Coordinate System (WGS84)
fire_2025 <- st_transform(fire_2025, 4326)
thailand_provinces <- st_transform(thailand_provinces, 4326)

# create the map
ggplot() +
  # Layer 1: Thailand Province Borders
  geom_sf(data = thailand_provinces, 
          fill = "gray98", 
          color = "gray80", 
          size = 0.3) +
  # Layer 2: Fire Points
  # We increase 'size' and 'alpha' since we are only looking at one year
  geom_sf(data = fire_2025, 
          aes(color = FRP), 
          size = 0.8, 
          alpha = 0.6) +
  # Styling
  scale_color_viridis_c(option = "inferno", 
                        trans = "log10", 
                        name = "Fire Intensity\n(FRP)") +
  theme_minimal() +
  labs(title = "Thailand Wildfire Detections (2025)",
       subtitle = "Data Source: NASA FIRMS (VIIRS)",
       x = "Longitude", y = "Latitude") # Optional: adds a scale bar