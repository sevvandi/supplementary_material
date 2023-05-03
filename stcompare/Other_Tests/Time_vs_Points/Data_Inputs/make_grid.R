# Load in the Africa polygons and create a grid of points over it

library(ggplot2)
library(raster)

# Requred: Set the working directory to the "Data_Inputs" directory
setwd("")

Africa_map <- readRDS("Africa_border.rds")

grid_whole <- makegrid(Africa_map, cellsize = 0.1)
colnames(grid_whole) <- c("x","y")

#convert to spatialPointsDataFrame
coordinates(grid_whole) <- ~x + y
proj4string(grid_whole) <- proj4string(Africa_map)

# Keep only the points in Africa
grid_cut <- grid_whole[Africa_map, ]

# Save the two grids. INLA and GPBoost use the cut grid. FRK and SPRF use
# the whole grid
save(grid_whole, grid_cut, file = "Africa_grids.RData")


# Make 0.15 degree grid
# ---------------------------------------------------------------------------
grid_whole_mh <- makegrid(Africa_map, cellsize = 0.15)
colnames(grid_whole_mh) <- c("x","y")
coordinates(grid_whole_mh) <- ~x + y
proj4string(grid_whole_mh) <- proj4string(Africa_map)
grid_cut_mh <- grid_whole_mh[Africa_map, ]
save(grid_whole_mh, grid_cut_mh, file = "Africa_grids_mh.RData")


