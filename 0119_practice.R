
## Practice fire data 
#working with data from FIRMS and GFED4 to practice applying the spatial 
# data analysis techniques

library(sf)
library(raster); library(dplyr); library(ggplot2)
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
all_fires <- do.call(rbind, lapply(file_list, function(x) {
  temp_sf <- st_read(x, quiet = TRUE)
  # Ensure column names match your 'keep_cols' casing
  # NASA data can vary, so forcing to uppercase is a safe bet
  names(temp_sf) <- toupper(names(temp_sf))
  st_geometry(temp_sf) <- "GEOMETRY"
  # Select only the specific columns
  return(temp_sf[, keep_cols])
}))
#check the files combined correctly 
#summary(all_fires)

# create a date object 
all_fires$ACQ_DATE <- as.Date(all_fires$ACQ_DATE)

# create a year column 
all_fires$YEAR <- as.numeric(format(all_fires$ACQ_DATE, "%Y"))

fire_counts <- as.data.frame(table(all_fires$YEAR))
colnames(fire_counts) <- c("Year", "Count")
colnames(fire_counts)

# Save as a compressed R object
saveRDS(all_fires, "C:/Users/marin/Documents/1. Liu Lab/Practice_data_folder/all_fires_compiled.rds")

# To load it back tomorrow:
# all_fires <- readRDS("C:/Users/marin/Documents/1. Liu Lab/Practice_data_folder/all_fires_compiled.rds")

ggplot(data = all_fires, aes(x = FRP)) +
    geom_sf(color = "red", size = 0.5) +
    theme_minimal() +
    labs(title = "Fire Detections")



