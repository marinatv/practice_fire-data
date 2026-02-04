---
  title: 'GEOS 507B Practice data analysis'
output:
  html_notebook: default
---
  
library(sf)
library(raster); library(dplyr); library(ggplot2)
# I am going to focus on the downloaded FIRMS dataset
setwd("C:/Users/marin/OneDrive - UBC/2. Coursework/2025 Term 2 - GEOS 507B/Practice-data_folder/FIRMS_2026-01-19")

firms_modis <- st_read("fire_nrt_M-C61_706801.shp")
st_crs(firms_modis)

plot1 <- plot(firms_modis["FRP"])
plot1 

map1 <- ggplot(data = firms_modis, aes(x = FRP)) +
    geom_sf(color = "red", size = 0.5) +
    theme_minimal() +
    labs(title = "Fire Detections")
map1


