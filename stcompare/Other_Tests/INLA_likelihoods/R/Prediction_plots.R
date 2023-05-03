# Make individual prevalence and uncertainty plots for each model on each
# of the three input data sets.
library(ggplot2)
library(dplyr)
library(patchwork)
library(viridis)
library(RColorBrewer)
library(raster)

# Set the working directory to the INLA_likelihoods directory
setwd()

# Load the predictions from each model
# -------------------------------------------------------------------------

# Trained on observations, change the path to plot the data from each likelihood
INLA_obs <- readRDS("Data_Outputs/n868typeobservation_datamodelINLAresponsegaussian.rds")


All_list <- list(INLA_obs)

clamp_values <- function(data_container){
  clamppred <- clamp(data_container$data$pred, 0,1)
  data_container$data$pred <- clamppred
  return(data_container)
}

All_list <- lapply(All_list, clamp_values)


# Mask the data
# -------------------------------------------------------------------------
# Load the map raster
map_raster <- readRDS("Data_Inputs/Rect_pfpr_2_10_2009_surface.rds")
class(map_raster)
# Convert to spatial pixels
map_raster_sp <- as(map_raster, "SpatialPixelsDataFrame")
class(map_raster_sp)
proj4string(map_raster_sp)

# Convert all the data to spatial points
to_sp <- function(data_container){
  coordinates(data_container$data) <- ~x+y
  proj4string(data_container$data) <- "+proj=longlat +datum=WGS84 +no_defs"
  return(data_container)
}
All_list <- lapply(All_list, to_sp)


mask <- function(data_container){
  masked_data <- data_container$data[map_raster_sp,]
  data_container$data<- masked_data
  return(data_container)
}

All_list_masked <- lapply(All_list, mask)



# Make plots for each model/data combination
#------------------------------------------------------------------------
Africa_border <- readRDS("Data_Inputs/Africa_border.rds")

im_width <- 12
im_height <- 11
im_res <- 300
background_fill <- "#f5f5f5"
border_colour <- "#a1a1a1"


make_plots <- function(data_container){
  # Add prevalence legend only for the first plot
  if(data_container$metadata$model == "INLA" & data_container$metadata$type == "observation_data"){
    legend_pos <- c(0.22, 0.27)
  }
  else{
    legend_pos <- "none"
  }
  # Add uncertainty legend to the observation plot for each model
  if(data_container$metadata$type == "observation_data"){
    uc_legend_pos <- c(0.22, 0.27)
  }
  else{
    uc_legend_pos <- "none"
  }

  data = as.data.frame(data_container$data)

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

  prediction_plot_file <- paste0("Figures/Prevalence/",
                                 data_container$metadata$model,"_",
                                 data_container$metadata$type,"_",
                                 data_container$metadata$n,"_prevalence_gaussian", ".png")
  png(prediction_plot_file, units = "cm", width = im_width, height = im_height, res = im_res)
  print(pred_plot)
  dev.off()

  uncertainty_plot_file <- paste0("Figures/Uncertainty/",
                                  data_container$metadata$model,"_",
                                  data_container$metadata$type,"_",
                                  data_container$metadata$n,"_uncertainty_gaussian", ".png")
  png(uncertainty_plot_file, units = "cm", width = im_width, height = im_height, res = im_res)
  print(uncertainty_plot)
  dev.off()
}

lapply(All_list_masked, make_plots)



