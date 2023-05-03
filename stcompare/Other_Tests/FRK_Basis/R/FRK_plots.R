# Make individual prevalence and uncertainty plots for each model on each
# of the three input data sets.
library(ggplot2)
library(dplyr)
library(patchwork)
library(viridis)
library(RColorBrewer)
library(raster)
library(ggforce) # plot circles

# Set working directory to "FRK_basis" directory
setwd()

# Load the predictions from each model
# -------------------------------------------------------------------------
# Load a chosen dataset to plot
FRK_Obs_nres3 <- readRDS("Data_Outputs/FRK_868_observation_data_3_1_1.25.rds")



# Mask the outputs using the MAP raseter
#------------------------------------------------------------------------
# Load the MAP raster
map_raster <- readRDS("Data_Inputs/Rect_pfpr_2_10_2009_surface.rds")
# Convert to spatial pixels
map_raster_sp <- as(map_raster, "SpatialPixelsDataFrame")

# Convert all the data to spatial points
to_sp <- function(data_container){
  coordinates(data_container$data) <- ~x+y
  proj4string(data_container$data) <- "+proj=longlat +datum=WGS84 +no_defs"
  return(data_container)
}

FRK_Obs_nres3_sp <- to_sp(FRK_Obs_nres3)


mask <- function(data_container){
  masked_data <- data_container$data[map_raster_sp,]
  data_container$data<- masked_data
  return(data_container)
}

FRK_Obs_nres3_masked <- mask(FRK_Obs_nres3_sp)

# Make plots for each model/data combination
#------------------------------------------------------------------------
Africa_border <- readRDS("Data_Inputs/Africa_border.rds")

im_width <- 12
im_height <- 11
im_res <- 300

background_fill <- "#f5f5f5"
border_colour <- "#a1a1a1"

make_plots <- function(data_container){
  legend_pos <- "none"
  uc_legend_pos <- "none"
  data = as.data.frame(data_container$data)

  # Plot the prevalence
  pred_plot <- ggplot(data = data, mapping = aes(x = x, y = y, fill = pred, color = pred)) +
    geom_polygon(data = Africa_border, mapping = aes(x = long, y = lat, group = group), color = border_colour, fill = background_fill, size = 0.2 )+
    geom_tile() +
    scale_fill_distiller(palette = "Spectral", limits = c(0,1), name = "Prevalence") +
    scale_color_distiller(palette = "Spectral", limits = c(0,1), name = "Prevalence") +
    theme_bw() +
    coord_quickmap() +
    theme(
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = legend_pos,
      plot.margin = unit(c(-0.6,-1,-0.6,-1), 'cm')
    )

  # Plot the uncertainty
  uncertainty_plot <- ggplot(data = data, mapping = aes(x = x, y = y, fill = sd, color = sd)) +
    geom_polygon(data = Africa_border, mapping = aes(x = long, y = lat, group = group), color = border_colour, fill = background_fill, size = 0.2 )+
    geom_tile()+
    scale_fill_viridis(option  = "rocket", limits = c(0,1), breaks = c(0,1), name = "SD", direction = -1) +
    scale_color_viridis(option = "rocket", limits = c(0,1), breaks = c(0,1), name = "SD", direction = -1) +
    theme_bw() +
    coord_quickmap() +
    theme(
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      plot.margin = unit(c(-0.6,-1,-0.6,-1), 'cm'),
      legend.position = uc_legend_pos
    )

  # Plot the prevalence with the basis function centers superimposed
  basis <- data_container$basis@df[,c("loc1","loc2","scale")]
  basis_plot <- ggplot() +
    geom_tile(data = data, mapping = aes(x = x, y = y, fill = pred, color = pred)) +
    geom_circle(data = basis, mapping = aes(x0 = loc1, y0 = loc2, r = scale/2), size = 0.1, colour = "#00000015") +
    geom_polygon(data = Africa_border, aes(x = long, y = lat, group = group), colour = "#00000015", size = 0.1, fill = NA)+
    scale_fill_distiller(palette = "Spectral", limits = c(0,1), name = "Prevalence") +
    scale_color_distiller(palette = "Spectral", limits = c(0,1), name = "Prevalence") +
    theme_bw() +
    coord_quickmap()+
    theme(
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.position = "left",
      plot.margin = unit(c(-0.6,0,-0.6,0), 'cm')
    )

  # Put the metadata into the filenames
  file_head <- paste0("Figures/", data_container$metadata$model, "_",
                     data_container$metadata$type,"_",
                     "reg",
                     data_container$metadata$regular,
                     "ap",
                     data_container$metadata$scale,
                     "nres",
                     data_container$metadata$nres,
                     "nbasis",
                     nrow(basis)
                     )

  prediction_plot_file <- paste0(file_head,
                                 "_prevalence", ".png")
  png(prediction_plot_file, units = "cm", width = im_width, height = im_height, res = im_res)
  print(pred_plot)
  dev.off()

  uncertainty_plot_file <- paste0(file_head,"_uncertainty", ".png")
  png(uncertainty_plot_file, units = "cm", width = im_width, height = im_height, res = im_res)
  print(uncertainty_plot)
  dev.off()

  basis_plot_file <- paste0(file_head,
                                  "_basis", ".png")
  png(basis_plot_file, units = "cm", width = im_width, height = im_height, res = im_res)
  print(basis_plot)
  dev.off()
}

make_plots(FRK_Obs_nres3_masked)



