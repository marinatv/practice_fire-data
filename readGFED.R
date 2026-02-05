library("raster"); library("rhdf5")
library("ggplot2")
setwd("C:/Users/marin/Documents/1. Liu Lab/Practice_data_folder/GFED4s")

# list files in directory
# dir()

readGFED <- function(x,y,beta) {
  H5close()
  if (beta == F) {
    h5file <- h5read(paste0(x,".hdf5"),y)
  } else {h5file <- h5read(paste0(x,"_beta.hdf5"),y)}
  h5ras <- raster(t(h5file))
  extent(h5ras) <- c(-180,180,-90,90)
  projection(h5ras) <- crs(raster())
  return(h5ras)
}

# set up the variables that can be changed depending on what we want to analyse
years_to_process <- c("2010", "2012", "2016")
target_month <- "05"           
is_beta      <- FALSE

for (yr in years_to_process) {

# apply to the file which has the same core and distinct years
file_base <- paste0("GFED4.1s_", yr)

# automate the emissions path part of the file name
emissions_path <- paste0("emissions/", target_month, "/DM")

# apply to the last variables to the function to read in the GFED files
DMras    <- readGFED(file_base, emissions_path, is_beta)
GFEDarea <- readGFED(file_base, "ancill/grid_cell_area", is_beta)

# convert to dataframe for ggplot
DMras_df <- as.data.frame(DMras * GFEDarea, xy = TRUE)
colnames(DMras_df)[3] <- "emissions_val" # Rename for consistency

# create dynamic plot for ggplot
gg <- ggplot(data = DMras_df) + 
  geom_raster(aes(x = x, y = y, fill = emissions_val)) +
  scale_fill_viridis_c(
    option = "inferno", 
    trans = "pseudo_log",
    name = "kg DM"
  ) +
  labs(
    title = paste("GFED4.1s Emissions:", yr),
    subtitle = paste("Month:", month.name[as.numeric(target_month)]),
    caption = paste("Source File:", file_base)
  ) +
  theme_minimal()

ggsave("test.pdf",gg,width=8,height=4.5,units="in",dpi=500)
print(paste("GFED4.1s Emissions for year:", yr))
print(gg)
}
# DMras <- readGFED("GFED4.1s_2016","emissions/11/DM",F)
# GFEDarea <- readGFED("GFED4.1s_2016","ancill/grid_cell_area",F)
# DMras_df <- as.data.frame(DMras * GFEDarea, xy=T)

# gg1 <- ggplot(data = DMras_df) + 
  #geom_raster(aes(x = x, y = y, fill = layer)) + 
  #scale_fill_viridis_c(
   # option = "inferno", 
    #trans = "log10", 
    #na.value = "black"
#  ) +
 # labs(
  #  title = "GFED4.1s Dry Matter Emissions",
   # subtitle = "November 2016",
    #caption = "Units: kg DM per grid cell")
  #theme_minimal()
#gg1 # consider changing the projection



